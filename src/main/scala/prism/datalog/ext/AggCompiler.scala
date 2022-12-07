package prism.datalog.ext

import prism.core.data.{Datum, Tuple}
import prism.engine.AggFunction

/**
 * Advanced numeric aggregators supporting mean, sample variance, and median estimation.
 */
object AggCompiler {

  final case class Mean(valueColumn: Int) extends AggFunction {
    def name: String = s"mean(col$valueColumn)"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      var sum = 0.0
      var count = 0L
      for ((tup, mult) <- values if mult > 0) {
        val v = tup(valueColumn) match {
          case Datum.I64(l) => l.toDouble
          case Datum.F64(d) => d
          case _ => 0.0
        }
        sum += v * mult
        count += mult
      }
      if (count > 0) Some(Datum.F64(sum / count.toDouble)) else None
    }
  }

  final case class Variance(valueColumn: Int) extends AggFunction {
    def name: String = s"variance(col$valueColumn)"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      val raw = collection.mutable.ArrayBuffer[Double]()
      for ((tup, mult) <- values if mult > 0) {
        val v = tup(valueColumn) match {
          case Datum.I64(l) => l.toDouble
          case Datum.F64(d) => d
          case _ => 0.0
        }
        for (_ <- 0 until mult.toInt) raw += v
      }
      if (raw.length >= 2) {
        val mean = raw.sum / raw.length.toDouble
        val sumSq = raw.map(x => math.pow(x - mean, 2)).sum
        val variance = sumSq / (raw.length - 1).toDouble
        Some(Datum.F64(variance))
      } else None
    }
  }

  final case class Median(valueColumn: Int) extends AggFunction {
    def name: String = s"median(col$valueColumn)"
    def compute(values: Iterable[(Tuple, Long)]): Option[Datum] = {
      val raw = collection.mutable.ArrayBuffer[Double]()
      for ((tup, mult) <- values if mult > 0) {
        val v = tup(valueColumn) match {
          case Datum.I64(l) => l.toDouble
          case Datum.F64(d) => d
          case _ => 0.0
        }
        for (_ <- 0 until mult.toInt) raw += v
      }
      if (raw.nonEmpty) {
        val sorted = raw.sorted
        val mid = sorted.length / 2
        val median = if (sorted.length % 2 == 1) sorted(mid) else (sorted(mid - 1) + sorted(mid)) / 2.0
        Some(Datum.F64(median))
      } else None
    }
  }
}
