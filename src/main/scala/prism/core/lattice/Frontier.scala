package prism.core.lattice

import scala.collection.mutable

/**
 * An Antichain is a set of mutually incomparable timestamps.
 * In a partially ordered set (poset), an antichain contains elements
 * where no two elements are comparable (neither a <= b nor b <= a).
 * In differential dataflow, the frontier of active computations is an antichain.
 */
final class Antichain private (val elements: List[Timestamp]) extends Serializable {
  def isEmpty: Boolean = elements.isEmpty
  def nonEmpty: Boolean = elements.nonEmpty
  def size: Int = elements.size

  /**
   * Tests if any element in this antichain is less than or equal to the candidate timestamp.
   * If true, the candidate is in the future of (or at) the frontier.
   */
  def lessOrEqual(target: Timestamp): Boolean = {
    elements.exists(_.lessOrEqual(target))
  }

  /**
   * Tests if the target timestamp is strictly in the past of the frontier.
   * If not (frontier <= target), then no future event can be strictly less than or equal to target.
   */
  def isPast(target: Timestamp): Boolean = {
    !lessOrEqual(target)
  }

  /**
   * Adds a new timestamp to this antichain, maintaining the antichain invariant:
   * 1. If an existing element <= ts, ts is redundant and rejected.
   * 2. Otherwise, ts is added, and any existing elements with ts <= existing are evicted.
   */
  def insert(ts: Timestamp): Antichain = {
    if (elements.exists(_.lessOrEqual(ts))) {
      this
    } else {
      val filtered = elements.filterNot(ts.lessOrEqual)
      new Antichain(ts :: filtered)
    }
  }

  /**
   * Meets two antichains, computing the minimal timestamps across both sets.
   */
  def meet(other: Antichain): Antichain = {
    var result = this
    for (t <- other.elements) {
      result = result.insert(t)
    }
    result
  }

  override def equals(other: Any): Boolean = other match {
    case that: Antichain =>
      this.elements.toSet == that.elements.toSet
    case _ => false
  }

  override def hashCode(): Int = elements.toSet.hashCode()

  override def toString: String = elements.mkString("{", ", ", "}")
}

object Antichain {
  val Empty: Antichain = new Antichain(Nil)

  def apply(timestamps: Timestamp*): Antichain = {
    var ac = Empty
    for (ts <- timestamps) {
      ac = ac.insert(ts)
    }
    ac
  }

  def fromSeq(seq: Seq[Timestamp]): Antichain = {
    var ac = Empty
    for (ts <- seq) {
      ac = ac.insert(ts)
    }
    ac
  }
}

/**
 * Tracks multi-party capability counts across timestamps and computes progressive frontiers.
 * As operators consume work and yield capabilities, the frontier advances monotonically.
 */
final class FrontierTracker {
  private val capabilities = mutable.Map[Timestamp, Long]()
  private var cachedFrontier: Antichain = Antichain.Empty
  private var dirty: Boolean = false

  /**
   * Updates capability count for a given timestamp.
   * A positive delta creates or holds a capability.
   * A negative delta releases a capability.
   */
  def update(ts: Timestamp, delta: Long): Unit = synchronized {
    if (delta != 0L) {
      val curr = capabilities.getOrElse(ts, 0L)
      val updated = curr + delta
      if (updated <= 0L) {
        capabilities.remove(ts)
      } else {
        capabilities.put(ts, updated)
      }
      dirty = true
    }
  }

  /**
   * Returns the current antichain frontier of held capabilities.
   */
  def currentFrontier(): Antichain = synchronized {
    if (dirty) {
      cachedFrontier = Antichain.fromSeq(capabilities.keys.toSeq)
      dirty = false
    }
    cachedFrontier
  }

  /**
   * Checks if all capabilities have been completely discharged.
   */
  def isIdle: Boolean = synchronized {
    capabilities.isEmpty
  }

  /**
   * Clears all capabilities.
   */
  def clear(): Unit = synchronized {
    capabilities.clear()
    cachedFrontier = Antichain.Empty
    dirty = false
  }
}
