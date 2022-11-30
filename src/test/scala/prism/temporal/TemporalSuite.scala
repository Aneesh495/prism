package prism.temporal

import munit.FunSuite
import prism.core.data.Tuple

class TemporalSuite extends FunSuite {

  test("TimeWindow boundary detection and containment") {
    val win = TimeWindow(100L, 200L)
    assert(win.contains(100L))
    assert(win.contains(150L))
    assert(!win.contains(200L)) // Exclusive end
    assert(!win.contains(50L))
  }

  test("TumblingWindow partitions time into non-overlapping blocks") {
    val tumbling = TumblingWindow(10L)
    val w1 = tumbling.assignWindows(5L)
    val w2 = tumbling.assignWindows(10L)

    assertEquals(w1, Seq(TimeWindow(0L, 10L)))
    assertEquals(w2, Seq(TimeWindow(10L, 20L)))
  }

  test("SlidingWindow assigns overlapping windows") {
    val sliding = SlidingWindow(size = 10L, slide = 5L)
    val windows = sliding.assignWindows(7L)

    // 7 falls in [0, 10) and [5, 15)
    assertEquals(windows.length, 2)
    assert(windows.contains(TimeWindow(0L, 10L)))
    assert(windows.contains(TimeWindow(5L, 15L)))
  }

  test("CEPPatternMatcher detects temporal sequences within time window") {
    val matcher = new CEPPatternMatcher()

    val pLogin = TemporalExpr.Predicate("login", t => t(0).asString == "login")
    val pWithdraw = TemporalExpr.Predicate("withdraw", t => t(0).asString == "withdraw")
    val seq = TemporalExpr.FollowedBy(pLogin, pWithdraw, maxDuration = 100L)

    val tupLogin = Tuple.of("login", "user1")
    val tupWithdraw = Tuple.of("withdraw", "user1")

    // Event 1 at t=10
    matcher.registerSequence(seq, tupLogin, 10L)

    // Event 2 at t=50 (within 100 duration)
    val matches = matcher.processEvent(tupWithdraw, 50L)
    assertEquals(matches.length, 1)
    assertEquals(matches.head.startEpoch, 10L)
    assertEquals(matches.head.endEpoch, 50L)

    // Another event at t=200 should NOT match old expired sequence
    val lateMatches = matcher.processEvent(tupWithdraw, 200L)
    assert(lateMatches.isEmpty)
  }
}
