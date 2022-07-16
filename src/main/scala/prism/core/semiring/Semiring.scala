package prism.core.semiring

/**
 * Typeclass defining an algebraic Commutative Semiring (K, +, *, 0, 1).
 * 
 * Axioms:
 * 1. (K, +, 0) is a commutative monoid with identity 0:
 *    a + b = b + a
 *    (a + b) + c = a + (b + c)
 *    a + 0 = a
 * 
 * 2. (K, *, 1) is a commutative monoid with identity 1:
 *    a * b = b * a
 *    (a * b) * c = a * (b * c)
 *    a * 1 = a
 * 
 * 3. Multiplication distributes over addition:
 *    a * (b + c) = (a * b) + (a * c)
 * 
 * 4. 0 is an annihilator for multiplication:
 *    a * 0 = 0
 */
trait Semiring[R] extends Serializable {
  def zero: R
  def one: R
  def plus(a: R, b: R): R
  def times(a: R, b: R): R

  def isZero(a: R): Boolean = a == zero
  def isOne(a: R): Boolean = a == one

  extension (a: R) {
    def +(b: R): R = plus(a, b)
    def *(b: R): R = times(a, b)
  }
}

object Semiring {
  def apply[R](using s: Semiring[R]): Semiring[R] = s
}

/**
 * An Abelian Group extending a Semiring with additive inverses.
 * Required for relational difference operators with exact retractions (-1).
 */
trait AbelianGroup[R] extends Semiring[R] {
  def negate(a: R): R
  def minus(a: R, b: R): R = plus(a, negate(b))

  extension (a: R) {
    def -(b: R): R = minus(a, b)
    def unary_- : R = negate(a)
  }
}

object AbelianGroup {
  def apply[R](using g: AbelianGroup[R]): AbelianGroup[R] = g
}

/**
 * Idempotent semiring where addition is idempotent (a + a = a).
 * Induces a natural partial order: a <= b iff a + b = b.
 */
trait LatticeSemiring[R] extends Semiring[R] {
  def order(a: R, b: R): Boolean = plus(a, b) == b
}
