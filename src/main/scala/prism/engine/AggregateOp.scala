package prism.engine

import prism.core.data.{Batch, Datum, Delta, DiffTrace, Tuple}
import prism.core.lattice.{Antichain, Timestamp}
import prism.core.semiring.{AbelianGroup, Semiring}
import scala.collection.mutable.ArrayBuffer

/**
 * Aggregation function descriptor defining the reduction logic over group values.
 */
trait AggFunction {
  def name: String
  def compute(values: Iterable[(Tuple, Long)]): Option[Datum]
}

object AggFunction {
  case object Count extends AggFunction {
    def name: String = "count"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      val totalCount = values.map(_._2).sum
      if (totalCount > 0) Some(Datum.I64(totalCount)) else None
    }
  }

  final case class Sum(valueColumn: Int) extends AggFunction {
    def name: String = s"sum(col$valueColumn)"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      var total = 0.0
      var count = 0L
      var isDouble = false
      for ((tup, mult) <- values if mult > 0) {
        tup(valueColumn) match {
          case Datum.I64(v) =>
            total += v * mult
            count += mult
          case Datum.F64(v) =>
            total += v * mult
            count += mult
            isDouble = true
          case _ => ()
        }
      }
      if (count > 0) {
        if (isDouble) Some(Datum.F64(total))
        else Some(Datum.I64(total.toLong))
      } else None
    }
  }

  final case class Min(valueColumn: Int) extends AggFunction {
    def name: String = s"min(col$valueColumn)"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      val candidates = values.filter(_._2 > 0).map(_._1(valueColumn))
      if (candidates.nonEmpty) Some(candidates.min) else None
    }
  }

  final case class Max(valueColumn: Int) extends AggFunction {
    def name: String = s"max(col$valueColumn)"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      val candidates = values.filter(_._2 > 0).map(_._1(valueColumn))
      if (candidates.nonEmpty) Some(candidates.max) else None
    }
  }
}

/**
 * Differential GroupBy and Aggregation operator.
 * Groups incoming tuples by key columns and computes aggregate functions.
 * Emits differential retractions and assertions whenever the group result updates.
 */
final class AggregateOp[R](
  val id: String,
  val groupKeyIndices: Array[Int],
  val aggFunc: AggFunction
)(using val group: AbelianGroup[R]) extends Operator[R] {

  val numInputs: Int = 1
  val numOutputs: Int = 1

  private val trace = DiffTrace.empty[Tuple, Tuple, R]
  private val currentAggregates = collection.mutable.Map[Tuple, Datum]()
  private val pending = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      pending += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = synchronized {
    if (newFrontier.nonEmpty) {
      trace.compact(newFrontier)
    }
  }

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    if (pending.isEmpty) Nil
    else {
      val emitted = new ArrayBuffer[Delta[Tuple, R]]()

      for (batch <- pending) {
        val indexed = new ArrayBuffer[Delta[(Tuple, Tuple), R]](batch.length)
        val affectedKeys = collection.mutable.Set[Tuple]()

        var i = 0
        while (i < batch.length) {
          val delta = batch(i)
          val key = delta.data.project(groupKeyIndices)
          indexed += Delta((key, delta.data), delta.timestamp, delta.weight)
          affectedKeys += key
          i += 1
        }

        trace.insert(Batch.fromUnsorted(indexed.toArray))

        // Recompute aggregate for affected keys
        for (key <- affectedKeys) {
          // Get current state at batch head timestamp
          val ts = batch(0).timestamp
          val groupContents = trace.accumulateKeyAt(key, ts)

          val valuesWithMult: Iterable[(Tuple, Long)] = groupContents.map { case (tup, w) =>
            val mult = w match {
              case l: Long => l
              case other => 1L
            }
            (tup, mult)
          }

          val newAggOpt = aggFunc.compute(valuesWithMult)
          val oldAggOpt = currentAggregates.get(key)

          (oldAggOpt, newAggOpt) match {
            case (Some(oldVal), Some(newVal)) if oldVal != newVal =>
              // Retract old aggregate and assert new aggregate
              val oldTuple = key.concat(Tuple(oldVal))
              val newTuple = key.concat(Tuple(newVal))
              emitted += Delta(oldTuple, ts, group.negate(group.one))
              emitted += Delta(newTuple, ts, group.one)
              currentAggregates.put(key, newVal)

            case (None, Some(newVal)) =>
              // Initial assertion
              val newTuple = key.concat(Tuple(newVal))
              emitted += Delta(newTuple, ts, group.one)
              currentAggregates.put(key, newVal)

            case (Some(oldVal), None) =>
              // Group emptied: retract old aggregate
              val oldTuple = key.concat(Tuple(oldVal))
              emitted += Delta(oldTuple, ts, group.negate(group.one))
              currentAggregates.remove(key)

            case _ => () // No change in aggregate value
          }
        }
      }
      pending.clear()

      if (emitted.isEmpty) Nil
      else Seq((0, Batch.fromUnsorted(emitted.toArray)))
    }
  }

  def hasPendingWork: Boolean = synchronized { pending.nonEmpty }
}
