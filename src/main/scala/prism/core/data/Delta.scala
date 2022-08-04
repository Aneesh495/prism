package prism.core.data

import prism.core.lattice.Timestamp
import prism.core.semiring.{Semiring, AbelianGroup}

/**
 * Fundamental differential delta primitive: (data, timestamp, weight).
 * Represents a point mutation in a collection at a multidimensional logical coordinate.
 */
final case class Delta[D, R](
  data: D,
  timestamp: Timestamp,
  weight: R
) {
  def mapData[D2](f: D => D2): Delta[D2, R] = {
    Delta(f(data), timestamp, weight)
  }

  def mapWeight[R2](f: R => R2): Delta[D, R2] = {
    Delta(data, timestamp, f(weight))
  }

  def negate(using g: AbelianGroup[R]): Delta[D, R] = {
    Delta(data, timestamp, g.negate(weight))
  }

  override def toString: String = s"Delta($data @ $timestamp, w=$weight)"
}

object Delta {
  given [D: Ordering, R]: Ordering[Delta[D, R]] with {
    def compare(x: Delta[D, R], y: Delta[D, R]): Int = {
      val dataCmp = summon[Ordering[D]].compare(x.data, y.data)
      if (dataCmp != 0) dataCmp
      else {
        val maxLen = math.max(x.timestamp.dimensions, y.timestamp.dimensions)
        var i = 0
        var diff = 0
        while (i < maxLen && diff == 0) {
          diff = java.lang.Long.compare(x.timestamp(i), y.timestamp(i))
          i += 1
        }
        diff
      }
    }
  }
}
