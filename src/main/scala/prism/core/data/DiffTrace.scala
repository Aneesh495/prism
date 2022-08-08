package prism.core.data

import prism.core.lattice.{Timestamp, Antichain}
import prism.core.semiring.Semiring
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * Hierarchical LSM-inspired differential trace.
 * Maintains indexed history of (Key, Value) pairs across multidimensional time.
 * Provides logarithmic-time cursor lookups and incremental historical compaction.
 */
final class DiffTrace[K, V, R](
  val branchingFactor: Int = 4
)(using 
  val ordK: Ordering[K], 
  val ordV: Ordering[V], 
  val sr: Semiring[R], 
  val tagK: ClassTag[K], 
  val tagV: ClassTag[V]
) extends Serializable {

  given ordKV: Ordering[(K, V)] with {
    def compare(x: (K, V), y: (K, V)): Int = {
      val kc = ordK.compare(x._1, y._1)
      if (kc != 0) kc else ordV.compare(x._2, y._2)
    }
  }

  // Hierarchical levels of sorted batches (LSM-style)
  private val levels = new ArrayBuffer[Batch[(K, V), R]]()
  private var totalDeltaCount: Long = 0L

  def size: Long = totalDeltaCount
  def isEmpty: Boolean = totalDeltaCount == 0L

  /**
   * Appends an incoming batch into the trace, triggering cascade merges across levels.
   */
  def insert(batch: Batch[(K, V), R]): Unit = synchronized {
    if (batch.nonEmpty) {
      totalDeltaCount += batch.length
      var carry = batch
      var lvl = 0
      while (lvl < levels.length && carry.nonEmpty) {
        val current = levels(lvl)
        if (current.isEmpty) {
          levels(lvl) = carry
          carry = Batch.empty[(K, V), R]
        } else {
          // Merge current level with carry and promote to next level
          carry = current.merge(carry)
          levels(lvl) = Batch.empty[(K, V), R]
          lvl += 1
        }
      }
      if (carry.nonEmpty) {
        levels += carry
      }
    }
  }

  /**
   * Returns all historical deltas for a specific key across all levels.
   */
  def queryKey(key: K): Array[Delta[V, R]] = synchronized {
    val buf = new ArrayBuffer[Delta[V, R]]()
    for (batch <- levels if batch.nonEmpty) {
      // Find range of entries where _._1 == key
      var low = 0
      var high = batch.length
      while (low < high) {
        val mid = (low + high) >>> 1
        if (ordK.compare(batch(mid).data._1, key) < 0) {
          low = mid + 1
        } else {
          high = mid
        }
      }
      val start = low

      high = batch.length
      while (low < high) {
        val mid = (low + high) >>> 1
        if (ordK.compare(batch(mid).data._1, key) <= 0) {
          low = mid + 1
        } else {
          high = mid
        }
      }
      val end = low

      var i = start
      while (i < end) {
        val entry = batch(i)
        buf += Delta(entry.data._2, entry.timestamp, entry.weight)
        i += 1
      }
    }
    buf.toArray
  }

  /**
   * Evaluates the active value multi-set for a key at coordinate targetTime.
   */
  def accumulateKeyAt(key: K, targetTime: Timestamp): Map[V, R] = synchronized {
    val acc = collection.mutable.Map[V, R]()
    val deltas = queryKey(key)
    var i = 0
    while (i < deltas.length) {
      val d = deltas(i)
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

  /**
   * Historical compaction past the given antichain frontier.
   * Collapses intermediate timestamps that have fallen strictly into the past
   * of the frontier, reclaiming trace memory.
   */
  def compact(frontier: Antichain): Unit = synchronized {
    if (frontier.nonEmpty && levels.nonEmpty) {
      // Consolidate all levels into a single batch
      var merged = Batch.empty[(K, V), R]
      for (batch <- levels if batch.nonEmpty) {
        merged = merged.merge(batch)
      }

      val compactedDeltas = new ArrayBuffer[Delta[(K, V), R]](merged.length)
      var i = 0
      while (i < merged.length) {
        val d = merged(i)
        // If the delta timestamp is strictly behind the frontier, fold it to a canonical lower bound
        val foldedTs = if (frontier.isPast(d.timestamp)) {
          // Map to least upper bound with frontier or zero coordinate
          Timestamp.Zero
        } else {
          d.timestamp
        }
        compactedDeltas += Delta(d.data, foldedTs, d.weight)
        i += 1
      }

      val consolidated = Batch.fromUnsorted(compactedDeltas.toArray)
      levels.clear()
      if (consolidated.nonEmpty) {
        levels += consolidated
      }
      totalDeltaCount = consolidated.length
    }
  }

  /**
   * Returns a flattened batch of all deltas currently contained in the trace.
   */
  def toBatch: Batch[(K, V), R] = synchronized {
    var acc = Batch.empty[(K, V), R]
    for (b <- levels if b.nonEmpty) {
      acc = acc.merge(b)
    }
    acc
  }
}

object DiffTrace {
  def empty[K: Ordering: ClassTag, V: Ordering: ClassTag, R: Semiring]: DiffTrace[K, V, R] = {
    new DiffTrace[K, V, R]()
  }
}
