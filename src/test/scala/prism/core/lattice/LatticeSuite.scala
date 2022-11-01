package prism.core.lattice

import munit.FunSuite

class LatticeSuite extends FunSuite {

  test("Timestamp partial ordering and dimension access") {
    val t1 = Timestamp(1L, 2L)
    val t2 = Timestamp(1L, 3L)
    val t3 = Timestamp(2L, 1L)

    assertEquals(t1.apply(0), 1L)
    assertEquals(t1.apply(1), 2L)
    assertEquals(t1.apply(5), 0L) // Default out of bounds is 0

    assert(t1.lessOrEqual(t2))
    assert(!t2.lessOrEqual(t1))
    assert(t1.strictlyLess(t2))

    // Incomparable concurrent timestamps
    assert(t1.isConcurrent(t3))
    assert(t2.isConcurrent(t3))
  }

  test("Timestamp meet and join lattice operations") {
    val t1 = Timestamp(3L, 1L, 5L)
    val t2 = Timestamp(1L, 4L, 2L)

    val j = t1.join(t2)
    assertEquals(j, Timestamp(3L, 4L, 5L))

    val m = t1.meet(t2)
    assertEquals(m, Timestamp(1L, 1L, 2L))
  }

  test("Timestamp advancement along dimensions") {
    val t0 = Timestamp(1L, 0L)
    val tNext = t0.advance(1, 5L)
    assertEquals(tNext, Timestamp(1L, 5L))

    val tDim2 = t0.advance(2, 3L)
    assertEquals(tDim2, Timestamp(1L, 0L, 3L))
  }

  test("Antichain maintains mutually incomparable elements") {
    val ac = Antichain.Empty
    val t1 = Timestamp(2L, 2L)
    val t2 = Timestamp(3L, 3L) // Dominated by t1
    val t3 = Timestamp(1L, 4L) // Incomparable to t1

    val ac1 = ac.insert(t1)
    assertEquals(ac1.size, 1)

    // Inserting dominated timestamp t2 should be rejected
    val ac2 = ac1.insert(t2)
    assertEquals(ac2.size, 1)
    assert(ac2.lessOrEqual(t2))

    // Inserting incomparable timestamp t3 should be retained
    val ac3 = ac2.insert(t3)
    assertEquals(ac3.size, 2)

    // Inserting t0 <= t1 and t0 <= t3 should evict both
    val t0 = Timestamp(1L, 1L)
    val ac4 = ac3.insert(t0)
    assertEquals(ac4.size, 1)
    assertEquals(ac4.elements.head, t0)
  }

  test("FrontierTracker manages capability counts and dirty updates") {
    val tracker = new FrontierTracker()
    assert(tracker.isIdle)

    val t1 = Timestamp(1L, 0L)
    val t2 = Timestamp(2L, 0L)

    tracker.update(t1, 2L)
    tracker.update(t2, 1L)
    assert(!tracker.isIdle)

    val f1 = tracker.currentFrontier()
    assertEquals(f1.size, 1) // t1 dominates t2
    assertEquals(f1.elements.head, t1)

    // Release capabilities for t1
    tracker.update(t1, -2L)
    val f2 = tracker.currentFrontier()
    assertEquals(f2.size, 1)
    assertEquals(f2.elements.head, t2)

    // Release t2
    tracker.update(t2, -1L)
    assert(tracker.isIdle)
    assert(tracker.currentFrontier().isEmpty)
  }
}
