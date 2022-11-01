package prism.core.data

import munit.FunSuite
import prism.core.lattice.{Antichain, Timestamp}
import prism.core.semiring.DiffInt

class DataSuite extends FunSuite {
  given prism.core.semiring.AbelianGroup[Long] = DiffInt.diffIntGroup

  test("Datum values maintain types and total order") {
    val i = Datum.I64(42)
    val f = Datum.F64(42.0)
    val s = Datum.Str("42")
    val b = Datum.Bool(true)
    val sym = Datum.Sym("alpha")

    assertEquals(i.asLong, 42L)
    assertEquals(f.asDouble, 42.0)
    assertEquals(s.asString, "42")
    assertEquals(b.asBoolean, true)

    assert(b < i)
    assert(i < s)
    assert(s < sym)
  }

  test("Tuple projection, concatenation, and slicing") {
    val t = Tuple.of("alice", 30L, true, 4.5)
    assertEquals(t.arity, 4)
    assertEquals(t(0), Datum.Str("alice"))
    assertEquals(t(1), Datum.I64(30L))

    val proj = t.project(Array(0, 2))
    assertEquals(proj, Tuple.of("alice", true))

    val pref = t.prefix(2)
    assertEquals(pref, Tuple.of("alice", 30L))

    val suff = t.suffix(2)
    assertEquals(suff, Tuple.of(true, 4.5))

    val other = Tuple.of("engineer")
    val combined = t.concat(other)
    assertEquals(combined.arity, 5)
    assertEquals(combined(4), Datum.Str("engineer"))
  }

  test("Batch merge and consolidation with non-zero weights") {
    val ts0 = Timestamp(1L)
    val t1 = Tuple.of("x", 1L)
    val t2 = Tuple.of("y", 2L)

    val d1 = Delta(t1, ts0, 3L)
    val d2 = Delta(t1, ts0, -1L) // Cancels to weight 2
    val d3 = Delta(t2, ts0, 5L)

    val b1 = Batch.fromUnsorted(Array(d1, d3))
    val b2 = Batch.fromUnsorted(Array(d2))

    val merged = b1.merge(b2)
    assertEquals(merged.length, 2)
    assertEquals(merged(0).data, t1)
    assertEquals(merged(0).weight, 2L)
    assertEquals(merged(1).data, t2)
    assertEquals(merged(1).weight, 5L)

    // Complete cancellation
    val bCancel = Batch.fromUnsorted(Array(Delta(t1, ts0, -2L)))
    val finalBatch = merged.merge(bCancel)
    assertEquals(finalBatch.length, 1)
    assertEquals(finalBatch(0).data, t2)
  }

  test("DiffTrace LSM hierarchical indexing, accumulation, and compaction") {
    val trace = DiffTrace.empty[Tuple, Tuple, Long]
    val ts1 = Timestamp(1L)
    val ts2 = Timestamp(2L)

    val keyA = Tuple.of("user_1")
    val val1 = Tuple.of("page_home")
    val val2 = Tuple.of("page_cart")

    val batch1 = Batch.fromUnsorted(Array(
      Delta((keyA, val1), ts1, 1L),
      Delta((keyA, val2), ts2, 1L)
    ))

    trace.insert(batch1)
    assertEquals(trace.size, 2L)

    // Accumulate at ts1 should only see val1
    val snap1 = trace.accumulateKeyAt(keyA, ts1)
    assertEquals(snap1, Map(val1 -> 1L))

    // Accumulate at ts2 sees both
    val snap2 = trace.accumulateKeyAt(keyA, ts2)
    assertEquals(snap2, Map(val1 -> 1L, val2 -> 1L))

    // Retract val1 at ts3
    val ts3 = Timestamp(3L)
    trace.insert(Batch.fromUnsorted(Array(
      Delta((keyA, val1), ts3, -1L)
    )))

    val snap3 = trace.accumulateKeyAt(keyA, ts3)
    assertEquals(snap3, Map(val2 -> 1L))

    // Compacting past frontier ts3 folds history
    val frontier = Antichain(ts3)
    trace.compact(frontier)
    assertEquals(trace.queryKey(keyA).nonEmpty, true)
  }
}
