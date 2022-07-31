package prism.core.data

import java.util.Arrays

/**
 * High-performance array-backed immutable relational tuple.
 * Provides zero-overhead projection, concatenation, and lexicographic ordering.
 */
final class Tuple private (val values: Array[Datum]) extends Ordered[Tuple] with Serializable {
  def arity: Int = values.length

  def apply(idx: Int): Datum = values(idx)

  def get(idx: Int): Option[Datum] = {
    if (idx >= 0 && idx < values.length) Some(values(idx))
    else None
  }

  def project(indices: Array[Int]): Tuple = {
    val projected = new Array[Datum](indices.length)
    var i = 0
    while (i < indices.length) {
      projected(i) = values(indices(i))
      i += 1
    }
    new Tuple(projected)
  }

  def project(indices: Seq[Int]): Tuple = project(indices.toArray)

  def concat(that: Tuple): Tuple = {
    val total = this.values.length + that.values.length
    val combined = new Array[Datum](total)
    System.arraycopy(this.values, 0, combined, 0, this.values.length)
    System.arraycopy(that.values, 0, combined, this.values.length, that.values.length)
    new Tuple(combined)
  }

  def prefix(length: Int): Tuple = {
    val clamped = math.min(length, values.length)
    val slice = new Array[Datum](clamped)
    System.arraycopy(values, 0, slice, 0, clamped)
    new Tuple(slice)
  }

  def suffix(fromIndex: Int): Tuple = {
    if (fromIndex >= values.length) Tuple.empty
    else {
      val len = values.length - fromIndex
      val slice = new Array[Datum](len)
      System.arraycopy(values, fromIndex, slice, 0, len)
      new Tuple(slice)
    }
  }

  def compare(that: Tuple): Int = {
    val minLen = math.min(this.values.length, that.values.length)
    var i = 0
    var diff = 0
    while (i < minLen && diff == 0) {
      diff = this.values(i).compare(that.values(i))
      i += 1
    }
    if (diff != 0) diff
    else this.values.length.compare(that.values.length)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Tuple =>
      if (this.values.length != that.values.length) false
      else {
        var i = 0
        var eq = true
        while (i < this.values.length && eq) {
          if (this.values(i) != that.values(i)) eq = false
          i += 1
        }
        eq
      }
    case _ => false
  }

  override def hashCode(): Int = {
    var h = 1
    var i = 0
    while (i < values.length) {
      h = 31 * h + values(i).hashCode()
      i += 1
    }
    h
  }

  override def toString: String = values.mkString("(", ", ", ")")
}

object Tuple {
  val empty: Tuple = new Tuple(new Array[Datum](0))

  def apply(values: Datum*): Tuple = new Tuple(values.toArray)
  def fromArray(arr: Array[Datum]): Tuple = new Tuple(arr.clone())
  def fromSeq(seq: Seq[Datum]): Tuple = new Tuple(seq.toArray)

  def of(values: Any*): Tuple = {
    val data = values.map {
      case d: Datum => d
      case l: Long => Datum.I64(l)
      case i: Int => Datum.I64(i.toLong)
      case d: Double => Datum.F64(d)
      case s: String => Datum.Str(s)
      case b: Boolean => Datum.Bool(b)
      case other => Datum.Str(other.toString)
    }.toArray
    new Tuple(data)
  }

  given Ordering[Tuple] with {
    def compare(x: Tuple, y: Tuple): Int = x.compare(y)
  }
}
