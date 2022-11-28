package prism.testkit

import prism.core.semiring.Semiring
import scala.util.boundary

/**
 * Algebraic verification harness testing semiring axioms over sample elements.
 */
object SemiringLawChecker {

  def checkAxioms[R](samples: Seq[R])(using s: Semiring[R]): Boolean = boundary {
    for (a <- samples) {
      // Additive identity: a + 0 = a
      if (s.plus(a, s.zero) != a) boundary.break(false)

      // Multiplicative identity: a * 1 = a
      if (s.times(a, s.one) != a) boundary.break(false)

      // Multiplicative zero: a * 0 = 0
      if (s.times(a, s.zero) != s.zero) boundary.break(false)

      for (b <- samples) {
        // Additive commutativity: a + b = b + a
        if (s.plus(a, b) != s.plus(b, a)) boundary.break(false)

        // Multiplicative commutativity: a * b = b * a
        if (s.times(a, b) != s.times(b, a)) boundary.break(false)

        for (c <- samples) {
          // Additive associativity: (a + b) + c = a + (b + c)
          if (s.plus(s.plus(a, b), c) != s.plus(a, s.plus(b, c))) boundary.break(false)

          // Multiplicative associativity: (a * b) * c = a * (b * c)
          if (s.times(s.times(a, b), c) != s.times(a, s.times(b, c))) boundary.break(false)

          // Left distributivity: a * (b + c) = (a * b) + (a * c)
          if (s.times(a, s.plus(b, c)) != s.plus(s.times(a, b), s.times(a, c))) boundary.break(false)
        }
      }
    }
    true
  }
}
