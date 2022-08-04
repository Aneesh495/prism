package prism.core.data

import prism.core.lattice.Timestamp
import prism.core.semiring.Semiring
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * Immutable sorted and consolidated batch of differential mutations.
 * Invariants:
 * 1. Entries are sorted strictly by Delta Ordering (data, then timestamp).
 * 2. No two adjacent entries share both the same data and the same timestamp.
 * 3. All entries have non-zero weights under the active semiring.
 */
final class Batch[D, R] private (
  val entries: Array[Delta[D, R]]
)(using val ordD: Ordering[D], val sr: Semiring[R]) extends Serializable {

  def length: Int = entries.length
  def isEmpty: Boolean = entries.isEmpty
  def nonEmpty: Boolean = entries.nonEmpty

  def apply(idx: Int): Delta[D, R] = entries(idx)

  def iterator: Iterator[Delta[D, R]] = entries.iterator

  /**
   * Merges two sorted batches in O(N + M) time, consolidating weights for matching keys.
   */
  def merge(that: Batch[D, R]): Batch[D, R] = {
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val n1 = this.entries.length
      val n2 = that.entries.length
      val result = new ArrayBuffer[Delta[D, R]](n1 + n2)

      var i = 0
      var j = 0
      val ordDelta = summon[Ordering[Delta[D, R]]]

      while (i < n1 && j < n2) {
        val d1 = this.entries(i)
        val d2 = that.entries(j)
        val cmp = ordDelta.compare(d1, d2)
        if (cmp < 0) {
          result += d1
          i += 1
        } else if (cmp > 0) {
          result += d2
          j += 1
        } else {
          val mergedWeight = sr.plus(d1.weight, d2.weight)
          if (!sr.isZero(mergedWeight)) {
            result += Delta(d1.data, d1.timestamp, mergedWeight)
          }
          i += 1
          j += 1
        }
      }

      while (i < n1) {
        result += this.entries(i)
        i += 1
      }
      while (j < n2) {
        result += that.entries(j)
        j += 1
      }

      new Batch(result.toArray)
    }
  }

  /**
   * Filters the batch preserving sorted order.
   */
  def filter(predicate: D => Boolean): Batch[D, R] = {
    val filtered = entries.filter(d => predicate(d.data))
    new Batch(filtered)
  }

  /**
   * Maps data items, sorting and consolidating the result.
   */
  def mapData[D2: Ordering: ClassTag](f: D => D2): Batch[D2, R] = {
    val mapped = entries.map(d => Delta(f(d.data), d.timestamp, d.weight))
    Batch.fromUnsorted(mapped)
  }

  /**
   * Flat-maps data items, sorting and consolidating the result.
   */
  def flatMapData[D2: Ordering: ClassTag](f: D => Iterable[D2]): Batch[D2, R] = {
    val buf = new ArrayBuffer[Delta[D2, R]]()
    for (d <- entries) {
      for (out <- f(d.data)) {
        buf += Delta(out, d.timestamp, d.weight)
      }
    }
    Batch.fromUnsorted(buf.toArray)
  }

  /**
   * Binary search for the first delta with data >= target.
   */
  def lowerBound(target: D): Int = {
    var low = 0
    var high = entries.length
    while (low < high) {
      val mid = (low + high) >>> 1
      if (ordD.compare(entries(mid).data, target) < 0) {
        low = mid + 1
      } else {
        high = mid
      }
    }
    low
  }

  /**
   * Binary search for the first delta with data > target.
   */
  def upperBound(target: D): Int = {
    var low = 0
    var high = entries.length
    while (low < high) {
      val mid = (low + high) >>> 1
      if (ordD.compare(entries(mid).data, target) <= 0) {
        low = mid + 1
      } else {
        high = mid
      }
    }
    low
  }

  /**
   * Returns a slice of deltas whose data exactly equals target.
   */
  def find(target: D): Array[Delta[D, R]] = {
    val start = lowerBound(target)
    val end = upperBound(target)
    if (start < end) {
      val count = end - start
      val slice = new Array[Delta[D, R]](count)
      System.arraycopy(entries, start, slice, 0, count)
      slice
    } else {
      Array.empty[Delta[D, R]]
    }
  }

  /**
   * Reconstructs snapshot accumulation at logical coordinate targetTime:
   * sum { weight | (data, t, weight) in batch, t <= targetTime }
   */
  def accumulateAt(targetTime: Timestamp): Map[D, R] = {
    val acc = collection.mutable.Map[D, R]()
    var i = 0
    while (i < entries.length) {
      val d = entries(i)
      if (d.timestamp.lessOrEqual(targetTime)) {
        val curr = acc.getOrElse(d.data, sr.zero)
        val updated = sr.plus(curr, d.weight)
        if (sr.isZero(updated)) {
          acc.remove(d.data)
        } else {
          acc.put(d.data, updated)
        }
      }
      i += 1
    }
    acc.toMap
  }

  override def toString: String = {
    val sample = entries.take(10).mkString(", ")
    val suffix = if (entries.length > 10) s", ... (${entries.length} total)" else ""
    s"Batch[$sample$suffix]"
  }
}

object Batch {
  def empty[D: Ordering, R: Semiring]: Batch[D, R] = {
    new Batch(Array.empty[Delta[D, R]])
  }

  def single[D: Ordering, R: Semiring](data: D, ts: Timestamp, weight: R): Batch[D, R] = {
    if (summon[Semiring[R]].isZero(weight)) empty
    else new Batch(Array(Delta(data, ts, weight)))
  }

  def fromSeq[D: Ordering: ClassTag, R: Semiring](seq: Seq[Delta[D, R]]): Batch[D, R] = {
    fromUnsorted(seq.toArray)
  }

  def fromUnsorted[D: Ordering: ClassTag, R: Semiring](arr: Array[Delta[D, R]]): Batch[D, R] = {
    if (arr.isEmpty) empty
    else {
      val sr = summon[Semiring[R]]
      val ord = summon[Ordering[Delta[D, R]]]
      java.util.Arrays.sort(arr, ord)

      val compacted = new ArrayBuffer[Delta[D, R]](arr.length)
      var i = 0
      while (i < arr.length) {
        val head = arr(i)
        var accWeight = head.weight
        var next = i + 1
        while (next < arr.length && ord.compare(head, arr(next)) == 0) {
          accWeight = sr.plus(accWeight, arr(next).weight)
          next += 1
        }
        if (!sr.isZero(accWeight)) {
          compacted += Delta(head.data, head.timestamp, accWeight)
        }
        i = next
      }
      new Batch(compacted.toArray)
    }
  }
}
