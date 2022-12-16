package prism.temporal

import munit.FunSuite
import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.{DiffInt, Tropical}
import prism.core.semiring.DiffInt.diffIntGroup
import prism.temporal.window.{IntervalJoin, SegmentTreeAggregator, Sessionizer}

class WindowSuite extends FunSuite {

  test("SegmentTreeAggregator maintains sliding range sums under DiffInt") {
    val tree = new SegmentTreeAggregator[Long](8)
    // Initialize elements: [10, 20, 30, 40, 50, 0, 0, 0]
    tree.update(0, 10L)
    tree.update(1, 20L)
    tree.update(2, 30L)
    tree.update(3, 40L)
    tree.update(4, 50L)

    assertEquals(tree.query(0, 2), 60L) // 10 + 20 + 30
    assertEquals(tree.query(1, 3), 90L) // 20 + 30 + 40
    assertEquals(tree.total(), 150L)

    // Point update at index 2
    tree.update(2, 5L)
    assertEquals(tree.query(0, 2), 35L) // 10 + 20 + 5
    assertEquals(tree.total(), 125L)
  }

  test("SegmentTreeAggregator maintains sliding minimums under Tropical semiring") {
    import Tropical.tropicalSemiring
    val tree = new SegmentTreeAggregator[Tropical](8)

    tree.update(0, Tropical(5.0))
    tree.update(1, Tropical(2.0))
    tree.update(2, Tropical(8.0))
    tree.update(3, Tropical(1.0))
    tree.update(4, Tropical(9.0))

    // Tropical plus is min: min(5.0, 2.0, 8.0) == 2.0
    assertEquals(tree.query(0, 2), Tropical(2.0))
    // min(2.0, 8.0, 1.0) == 1.0
    assertEquals(tree.query(1, 3), Tropical(1.0))
  }

  test("IntervalJoin pairs events within the temporal interval window") {
    val join = new IntervalJoin[Long](
      keyIndexR = 0,
      keyIndexS = 0,
      timestampColR = 1,
      timestampColS = 1,
      lowerBoundMillis = 50L,
      upperBoundMillis = 100L
    )

    // Left stream: (user1, 1000)
    val batchR = Batch.fromSeq(List(
      Delta(Tuple(Datum.Str("user1"), Datum.I64(1000L)), Timestamp(1L), 1L)
    ))
    val out1 = join.processLeft(batchR)
    assertEquals(out1.length, 0) // No matching S yet

    // Right stream: (user1, 1050) -> falls in [1000 - 50, 1000 + 100] = [950, 1100]
    // and (user1, 1200) -> outside interval
    val batchS = Batch.fromSeq(List(
      Delta(Tuple(Datum.Str("user1"), Datum.I64(1050L)), Timestamp(2L), 1L),
      Delta(Tuple(Datum.Str("user1"), Datum.I64(1200L)), Timestamp(2L), 1L)
    ))
    val out2 = join.processRight(batchS)
    assertEquals(out2.length, 1)
    assertEquals(out2.head.data.arity, 4) // (user1, 1000, user1, 1050)
  }

  test("Sessionizer groups contiguous events and merges on bridging arrivals") {
    val sessionizer = new Sessionizer(keyIndex = 0, timestampCol = 1, gapTimeoutMillis = 100L)

    // Initial batch: events at t=100 and t=150 (within 100ms gap) -> 1 session [100, 150]
    // Event at t=500 -> 2nd session [500, 500]
    val userKey = Datum.Str("alice")
    val batch1 = Batch.fromSeq(List(
      Delta(Tuple(userKey, Datum.I64(100L)), Timestamp(1L), 1L),
      Delta(Tuple(userKey, Datum.I64(150L)), Timestamp(1L), 1L),
      Delta(Tuple(userKey, Datum.I64(500L)), Timestamp(1L), 1L)
    ))

    val res1 = sessionizer.addEvents(batch1)
    val aliceSessions = res1(userKey)
    assertEquals(aliceSessions.length, 2)
    assertEquals(aliceSessions(0).startTime, 100L)
    assertEquals(aliceSessions(0).endTime, 150L)
    assertEquals(aliceSessions(0).eventCount, 2L)
    assertEquals(aliceSessions(1).startTime, 500L)

    // Out of order events arriving later bridging the gap: t=230, t=320, t=420
    val batch2 = Batch.fromSeq(List(
      Delta(Tuple(userKey, Datum.I64(230L)), Timestamp(2L), 1L),
      Delta(Tuple(userKey, Datum.I64(320L)), Timestamp(2L), 1L),
      Delta(Tuple(userKey, Datum.I64(420L)), Timestamp(2L), 1L)
    ))
    sessionizer.addEvents(batch2)
    val mergedSessions = sessionizer.getSessions(userKey)
    // All events are now bridged into one unified session [100, 500]
    assertEquals(mergedSessions.length, 1)
    assertEquals(mergedSessions.head.startTime, 100L)
    assertEquals(mergedSessions.head.endTime, 500L)
    assertEquals(mergedSessions.head.eventCount, 6L)
  }
}
