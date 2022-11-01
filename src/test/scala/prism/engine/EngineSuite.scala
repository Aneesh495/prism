package prism.engine

import munit.FunSuite
import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.DiffInt

class EngineSuite extends FunSuite {
  given prism.core.semiring.AbelianGroup[Long] = DiffInt.diffIntGroup

  test("Unary operators transform and filter batches correctly") {
    val mapOp = new MapOp[Long]("map_1", tup => Tuple.of(tup(0), tup(1).asLong * 2))
    val filterOp = new FilterOp[Long]("filter_1", tup => tup(1).asLong > 10)

    val ts = Timestamp(1L)
    val inBatch = Batch.fromUnsorted(Array(
      Delta(Tuple.of("a", 4L), ts, 1L),
      Delta(Tuple.of("b", 8L), ts, 1L)
    ))

    mapOp.receive(0, inBatch)
    val mapOut = mapOp.step()
    assertEquals(mapOut.length, 1)
    val mappedBatch = mapOut.head._2
    assertEquals(mappedBatch.length, 2)
    assertEquals(mappedBatch(0).data, Tuple.of("a", 8L))
    assertEquals(mappedBatch(1).data, Tuple.of("b", 16L))

    filterOp.receive(0, mappedBatch)
    val filterOut = filterOp.step()
    assertEquals(filterOut.length, 1)
    val filteredBatch = filterOut.head._2
    assertEquals(filteredBatch.length, 1)
    assertEquals(filteredBatch(0).data, Tuple.of("b", 16L))
  }

  test("JoinOp performs symmetric bilinear differential joins") {
    // A(id, name), B(id, department)
    val joinOp = new JoinOp[Long](
      "join_1",
      keyIndicesA = Array(0),
      keyIndicesB = Array(0),
      combine = JoinOp.naturalJoinCombine(Array(0))
    )

    val ts1 = Timestamp(1L)
    val batchA1 = Batch.single(Tuple.of(1L, "Alice"), ts1, 1L)
    val batchB1 = Batch.single(Tuple.of(1L, "Engineering"), ts1, 1L)

    // Send A1 first
    joinOp.receive(0, batchA1)
    val out1 = joinOp.step()
    // No matches in B yet
    assert(out1.isEmpty)

    // Send B1
    joinOp.receive(1, batchB1)
    val out2 = joinOp.step()
    assertEquals(out2.length, 1)
    val resBatch = out2.head._2
    assertEquals(resBatch.length, 1)
    assertEquals(resBatch(0).data, Tuple.of(1L, "Alice", "Engineering"))
  }

  test("AntijoinOp emits assertions and retractions on negated key lifecycle") {
    // A(user_id, name) antijoin B(user_id)
    val antijoinOp = new AntijoinOp[Long](
      "antijoin_1",
      keyIndicesA = Array(0),
      keyIndicesB = Array(0)
    )

    val ts1 = Timestamp(1L)
    val batchA = Batch.single(Tuple.of(10L, "Bob"), ts1, 1L)

    // Bob arrives in A, not in B => Bob emitted
    antijoinOp.receive(0, batchA)
    val out1 = antijoinOp.step()
    assertEquals(out1.length, 1)
    assertEquals(out1.head._2(0).data, Tuple.of(10L, "Bob"))
    assertEquals(out1.head._2(0).weight, 1L)

    // Later at ts2, Bob is added to B (blocked) => retraction (-1) emitted for Bob
    val ts2 = Timestamp(2L)
    val batchB = Batch.single(Tuple.of(10L), ts2, 1L)
    antijoinOp.receive(1, batchB)
    val out2 = antijoinOp.step()
    assertEquals(out2.length, 1)
    assertEquals(out2.head._2(0).data, Tuple.of(10L, "Bob"))
    assertEquals(out2.head._2(0).weight, -1L) // Retraction!

    // Later at ts3, Bob is removed from B => Bob restored (+1)
    val ts3 = Timestamp(3L)
    val batchBRemove = Batch.single(Tuple.of(10L), ts3, -1L)
    antijoinOp.receive(1, batchBRemove)
    val out3 = antijoinOp.step()
    assertEquals(out3.length, 1)
    assertEquals(out3.head._2(0).data, Tuple.of(10L, "Bob"))
    assertEquals(out3.head._2(0).weight, 1L) // Restored!
  }

  test("AggregateOp incrementally maintains group count and sum") {
    // GroupBy user_id, count
    val aggOp = new AggregateOp[Long](
      "agg_count",
      groupKeyIndices = Array(0),
      aggFunc = AggFunction.Count
    )

    val ts1 = Timestamp(1L)
    val in1 = Batch.fromUnsorted(Array(
      Delta(Tuple.of("userA", "event1"), ts1, 1L),
      Delta(Tuple.of("userA", "event2"), ts1, 1L)
    ))

    aggOp.receive(0, in1)
    val out1 = aggOp.step()
    assertEquals(out1.length, 1)
    // Initial count is 2
    assertEquals(out1.head._2(0).data, Tuple.of("userA", 2L))
    assertEquals(out1.head._2(0).weight, 1L)

    // Another event arrives at ts2
    val ts2 = Timestamp(2L)
    val in2 = Batch.single(Tuple.of("userA", "event3"), ts2, 1L)
    aggOp.receive(0, in2)
    val out2 = aggOp.step()
    assertEquals(out2.length, 1)
    val b2 = out2.head._2
    // Retract count 2, assert count 3
    assertEquals(b2.length, 2)
    assertEquals(b2(0).data, Tuple.of("userA", 2L))
    assertEquals(b2(0).weight, -1L)
    assertEquals(b2(1).data, Tuple.of("userA", 3L))
    assertEquals(b2(1).weight, 1L)
  }
}
