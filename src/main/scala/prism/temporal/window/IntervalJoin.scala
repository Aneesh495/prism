package prism.temporal.window

import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.Semiring
import scala.collection.mutable

/**
 * Interval differential join between two temporal streams.
 * Joins tuples sharing a common join key where timestamps satisfy:
 *   t_r - lowerBound <= t_s <= t_r + upperBound
 */
final class IntervalJoin[R: Semiring](
  val keyIndexR: Int,
  val keyIndexS: Int,
  val timestampColR: Int,
  val timestampColS: Int,
  val lowerBoundMillis: Long,
  val upperBoundMillis: Long
) {
  require(lowerBoundMillis >= 0, "Lower bound must be non-negative")
  require(upperBoundMillis >= 0, "Upper bound must be non-negative")

  private val sr = summon[Semiring[R]]

  // Index maps: key -> list of (eventTime, tuple, weight)
  private val stateR = mutable.Map[Datum, mutable.ArrayBuffer[(Long, Tuple, R)]]()
  private val stateS = mutable.Map[Datum, mutable.ArrayBuffer[(Long, Tuple, R)]]()

  private def extractTime(tup: Tuple, col: Int): Long = tup(col) match {
    case Datum.I64(v) => v
    case Datum.F64(v) => v.toLong
    case _ => 0L
  }

  /**
   * Processes incoming deltas from the left stream (R).
   * Emits joined result tuples: (key, tupR, tupS).
   */
  def processLeft(batch: Batch[Tuple, R]): Seq[Delta[Tuple, R]] = synchronized {
    val results = mutable.ArrayBuffer[Delta[Tuple, R]]()

    for (delta <- batch.iterator) {
      val key = delta.data(keyIndexR)
      val timeR = extractTime(delta.data, timestampColR)
      val weightR = delta.weight

      // 1. Probe right state S
      stateS.get(key).foreach { sList =>
        for ((timeS, tupS, weightS) <- sList) {
          if (timeS >= timeR - lowerBoundMillis && timeS <= timeR + upperBoundMillis) {
            val joinedTup = delta.data.concat(tupS)
            val combinedWeight = sr.times(weightR, weightS)
            if (!sr.isZero(combinedWeight)) {
              val outTs = delta.timestamp.join(Timestamp(math.max(timeR, timeS)))
              results += Delta(joinedTup, outTs, combinedWeight)
            }
          }
        }
      }

      // 2. Accumulate into left state R
      val bufR = stateR.getOrElseUpdate(key, mutable.ArrayBuffer())
      bufR += ((timeR, delta.data, weightR))
    }

    results.toSeq
  }

  /**
   * Processes incoming deltas from the right stream (S).
   */
  def processRight(batch: Batch[Tuple, R]): Seq[Delta[Tuple, R]] = synchronized {
    val results = mutable.ArrayBuffer[Delta[Tuple, R]]()

    for (delta <- batch.iterator) {
      val key = delta.data(keyIndexS)
      val timeS = extractTime(delta.data, timestampColS)
      val weightS = delta.weight

      // 1. Probe left state R
      stateR.get(key).foreach { rList =>
        for ((timeR, tupR, weightR) <- rList) {
          if (timeS >= timeR - lowerBoundMillis && timeS <= timeR + upperBoundMillis) {
            val joinedTup = tupR.concat(delta.data)
            val combinedWeight = sr.times(weightR, weightS)
            if (!sr.isZero(combinedWeight)) {
              val outTs = delta.timestamp.join(Timestamp(math.max(timeR, timeS)))
              results += Delta(joinedTup, outTs, combinedWeight)
            }
          }
        }
      }

      // 2. Accumulate into right state S
      val bufS = stateS.getOrElseUpdate(key, mutable.ArrayBuffer())
      bufS += ((timeS, delta.data, weightS))
    }

    results.toSeq
  }

  /**
   * Clears internal state buffers.
   */
  def clear(): Unit = synchronized {
    stateR.clear()
    stateS.clear()
  }
}
