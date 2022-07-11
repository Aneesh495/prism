package prism.core.lattice

import scala.annotation.targetName

/**
 * Multidimensional timestamp coordinate representing a point in a product poset.
 * Used by the differential engine to order streaming events and recursive iterations.
 * Coordinates satisfy a partial order: a <= b iff a(i) <= b(i) for all dimensions i.
 */
final class Timestamp private (val coords: Array[Long]) extends Serializable {
  def dimensions: Int = coords.length

  def apply(dim: Int): Long = {
    if (dim < 0 || dim >= coords.length) 0L
    else coords(dim)
  }

  /**
   * Advances the specified dimension by delta. Returns a new timestamp.
   */
  def advance(dim: Int, delta: Long = 1L): Timestamp = {
    val newLen = math.max(coords.length, dim + 1)
    val next = new Array[Long](newLen)
    System.arraycopy(coords, 0, next, 0, coords.length)
    next(dim) = next(dim) + delta
    Timestamp.fromArray(next)
  }

  /**
   * Partial order comparison: a <= b iff for all i, a(i) <= b(i).
   */
  def lessOrEqual(that: Timestamp): Boolean = {
    val maxLen = math.max(this.dimensions, that.dimensions)
    var i = 0
    var result = true
    while (i < maxLen && result) {
      val a = this.apply(i)
      val b = that.apply(i)
      if (a > b) {
        result = false
      }
      i += 1
    }
    result
  }

  /**
   * Strict partial order: a < b iff a <= b and a != b.
   */
  def strictlyLess(that: Timestamp): Boolean = {
    lessOrEqual(that) && !this.equals(that)
  }

  /**
   * Determines if this timestamp is concurrent with (incomparable to) that timestamp.
   */
  def isConcurrent(that: Timestamp): Boolean = {
    !this.lessOrEqual(that) && !that.lessOrEqual(this)
  }

  /**
   * Least upper bound (join): component-wise maximum.
   */
  def join(that: Timestamp): Timestamp = {
    val maxLen = math.max(this.dimensions, that.dimensions)
    val res = new Array[Long](maxLen)
    var i = 0
    while (i < maxLen) {
      res(i) = math.max(this.apply(i), that.apply(i))
      i += 1
    }
    Timestamp.fromArray(res)
  }

  /**
   * Greatest lower bound (meet): component-wise minimum.
   */
  def meet(that: Timestamp): Timestamp = {
    val maxLen = math.max(this.dimensions, that.dimensions)
    val res = new Array[Long](maxLen)
    var i = 0
    while (i < maxLen) {
      res(i) = math.min(this.apply(i), that.apply(i))
      i += 1
    }
    Timestamp.fromArray(res)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Timestamp =>
      val maxLen = math.max(this.dimensions, that.dimensions)
      var i = 0
      var eq = true
      while (i < maxLen && eq) {
        if (this.apply(i) != that.apply(i)) eq = false
        i += 1
      }
      eq
    case _ => false
  }

  override def hashCode(): Int = {
    var h = 1
    var i = 0
    while (i < coords.length) {
      val c = coords(i)
      h = 31 * h + (c ^ (c >>> 32)).toInt
      i += 1
    }
    h
  }

  override def toString: String = coords.mkString("[", ",", "]")
}

object Timestamp {
  val Zero: Timestamp = new Timestamp(Array(0L))

  def apply(c0: Long): Timestamp = new Timestamp(Array(c0))
  def apply(c0: Long, c1: Long): Timestamp = new Timestamp(Array(c0, c1))
  def apply(c0: Long, c1: Long, c2: Long): Timestamp = new Timestamp(Array(c0, c1, c2))

  def fromSeq(seq: Seq[Long]): Timestamp = {
    if (seq.isEmpty) Zero
    else new Timestamp(seq.toArray)
  }

  def fromArray(arr: Array[Long]): Timestamp = {
    var lastNonZero = arr.length - 1
    while (lastNonZero > 0 && arr(lastNonZero) == 0L) {
      lastNonZero -= 1
    }
    if (lastNonZero == arr.length - 1) new Timestamp(arr)
    else {
      val trimmed = new Array[Long](lastNonZero + 1)
      System.arraycopy(arr, 0, trimmed, 0, lastNonZero + 1)
      new Timestamp(trimmed)
    }
  }

  given PartialOrdering[Timestamp] with {
    def tryCompare(x: Timestamp, y: Timestamp): Option[Int] = {
      val le = x.lessOrEqual(y)
      val ge = y.lessOrEqual(x)
      if (le && ge) Some(0)
      else if (le) Some(-1)
      else if (ge) Some(1)
      else None
    }

    def lteq(x: Timestamp, y: Timestamp): Boolean = x.lessOrEqual(y)
  }
}
