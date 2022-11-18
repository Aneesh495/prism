package prism.temporal

import prism.core.data.{Datum, Tuple}
import prism.core.lattice.Timestamp
import scala.collection.mutable

final case class ComplexEvent(
  matchedPattern: String,
  startEpoch: Long,
  endEpoch: Long,
  events: List[Tuple]
)

/**
 * Streaming Complex Event Processing (CEP) engine.
 * Matches multi-stage sequence patterns across sliding temporal windows.
 */
final class CEPPatternMatcher {
  private val activeSequences = mutable.ArrayBuffer[(TemporalExpr.FollowedBy, Long, List[Tuple])]()
  private val detectedMatches = mutable.ArrayBuffer[ComplexEvent]()

  def processEvent(tuple: Tuple, timestamp: Long): Seq[ComplexEvent] = synchronized {
    val newMatches = mutable.ArrayBuffer[ComplexEvent]()
    val nextSequences = mutable.ArrayBuffer[(TemporalExpr.FollowedBy, Long, List[Tuple])]()

    // Check ongoing sequences
    for ((seq, startTs, collected) <- activeSequences) {
      if (timestamp - startTs <= seq.maxDuration) {
        // Test second event predicate
        seq.second match {
          case TemporalExpr.Predicate(name, test) if test(tuple) =>
            val matchEvt = ComplexEvent(
              matchedPattern = s"${seq.first} -> $name",
              startEpoch = startTs,
              endEpoch = timestamp,
              events = collected :+ tuple
            )
            newMatches += matchEvt
            detectedMatches += matchEvt
          case _ =>
            // Keep waiting if within duration
            nextSequences += ((seq, startTs, collected))
        }
      }
    }

    activeSequences.clear()
    activeSequences ++= nextSequences
    newMatches.toSeq
  }

  def registerSequence(seq: TemporalExpr.FollowedBy, triggerTuple: Tuple, timestamp: Long): Unit = synchronized {
    seq.first match {
      case TemporalExpr.Predicate(_, test) if test(triggerTuple) =>
        activeSequences += ((seq, timestamp, List(triggerTuple)))
      case _ => ()
    }
  }

  def allMatches: Seq[ComplexEvent] = synchronized {
    detectedMatches.toSeq
  }

  def clear(): Unit = synchronized {
    activeSequences.clear()
    detectedMatches.clear()
  }
}
