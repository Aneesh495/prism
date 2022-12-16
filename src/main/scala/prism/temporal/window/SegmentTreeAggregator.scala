package prism.temporal.window

import prism.core.semiring.Semiring

/**
 * Binary segment tree for incremental sliding window aggregations over general semirings.
 * Allows O(log W) point updates and O(1) sliding window range aggregations
 * for semirings without an inverse (e.g. Min/Max tropical semirings, matrix multiplication).
 */
final class SegmentTreeAggregator[V](val capacity: Int)(using val sr: Semiring[V]) {
  require(capacity > 0, "Capacity must be positive")

  // Next power of 2 for segment tree leaves
  val size: Int = {
    var s = 1
    while (s < capacity) s <<= 1
    s
  }

  private val tree: Array[Any] = Array.fill[Any](2 * size)(sr.zero)

  @inline private def getVal(idx: Int): V = tree(idx).asInstanceOf[V]
  @inline private def setVal(idx: Int, value: V): Unit = { tree(idx) = value }

  /**
   * Updates the value at a leaf position (0-indexed).
   */
  def update(index: Int, value: V): Unit = {
    require(index >= 0 && index < capacity, s"Index $index out of bounds [0, $capacity)")
    var pos = index + size
    setVal(pos, value)
    pos >>= 1
    while (pos > 0) {
      val leftVal = getVal(pos << 1)
      val rightVal = getVal((pos << 1) | 1)
      setVal(pos, sr.plus(leftVal, rightVal))
      pos >>= 1
    }
  }

  /**
   * Computes the semiring sum over the range [from, to] inclusive.
   */
  def query(from: Int, to: Int): V = {
    if (from > to || from < 0 || to >= capacity) return sr.zero

    var left = from + size
    var right = to + size
    var leftAcc = sr.zero
    var rightAcc = sr.zero

    while (left <= right) {
      if ((left & 1) == 1) {
        leftAcc = sr.plus(leftAcc, getVal(left))
        left += 1
      }
      if ((right & 1) == 0) {
        rightAcc = sr.plus(getVal(right), rightAcc)
        right -= 1
      }
      left >>= 1
      right >>= 1
    }
    sr.plus(leftAcc, rightAcc)
  }

  /**
   * Returns the aggregate across the entire active capacity [0, capacity - 1].
   */
  def total(): V = getVal(1)

  /**
   * Clears the tree by resetting all nodes to the semiring additive zero.
   */
  def clear(): Unit = {
    var i = 0
    while (i < tree.length) {
      setVal(i, sr.zero)
      i += 1
    }
  }
}
