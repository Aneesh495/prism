package prism.temporal.window

import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import scala.collection.mutable

final case class Session(
  key: Datum,
  startTime: Long,
  endTime: Long,
  eventCount: Long
)

/**
 * Event-time session window aggregator with inactivity gap timeouts.
 * Dynamically expands and merges session intervals when events arrive.
 */
final class Sessionizer(
  val keyIndex: Int,
  val timestampCol: Int,
  val gapTimeoutMillis: Long
) {
  require(gapTimeoutMillis > 0, "Gap timeout must be strictly positive")

  // Key -> list of active sessions sorted by startTime
  private val sessions = mutable.Map[Datum, mutable.ArrayBuffer[Session]]()

  private def extractTime(tup: Tuple): Long = tup(timestampCol) match {
    case Datum.I64(v) => v
    case Datum.F64(v) => v.toLong
    case _ => 0L
  }

  /**
   * Ingests a batch of events and returns the updated set of sessions for affected keys.
   */
  def addEvents(batch: Batch[Tuple, Long]): Map[Datum, Seq[Session]] = synchronized {
    val affectedKeys = mutable.Set[Datum]()

    for (delta <- batch.iterator if delta.weight > 0) {
      val key = delta.data(keyIndex)
      val time = extractTime(delta.data)
      affectedKeys += key

      val keySessions = sessions.getOrElseUpdate(key, mutable.ArrayBuffer())
      insertEvent(keySessions, key, time)
    }

    affectedKeys.map(k => k -> sessions(k).toSeq).toMap
  }

  private def insertEvent(buf: mutable.ArrayBuffer[Session], key: Datum, time: Long): Unit = {
    // Check if event extends or merges existing sessions
    val overlapping = mutable.ArrayBuffer[Int]()

    var i = 0
    while (i < buf.length) {
      val s = buf(i)
      // An event belongs to session s if time is within [s.startTime - gap, s.endTime + gap]
      if (time >= s.startTime - gapTimeoutMillis && time <= s.endTime + gapTimeoutMillis) {
        overlapping += i
      }
      i += 1
    }

    if (overlapping.isEmpty) {
      // Create new singleton session
      val newSession = Session(key, time, time, 1L)
      buf += newSession
      buf.sortInPlaceBy(_.startTime)
    } else {
      // Merge all overlapping sessions with this new event
      var minStart = time
      var maxEnd = time
      var totalCount = 1L

      for (idx <- overlapping) {
        val s = buf(idx)
        minStart = math.min(minStart, s.startTime)
        maxEnd = math.max(maxEnd, s.endTime)
        totalCount += s.eventCount
      }

      // Remove overlapping in reverse order
      for (idx <- overlapping.reverse) {
        buf.remove(idx)
      }

      val merged = Session(key, minStart, maxEnd, totalCount)
      buf += merged
      buf.sortInPlaceBy(_.startTime)
    }
  }

  def getSessions(key: Datum): Seq[Session] = synchronized {
    sessions.getOrElse(key, Seq.empty).toSeq
  }

  def activeKeyCount: Int = synchronized { sessions.size }
}
