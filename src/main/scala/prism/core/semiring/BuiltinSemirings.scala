package prism.core.semiring

/**
 * Standard integer multiplicity semiring (Z, +, *, 0, 1).
 * Supports negative multiplicities (-1) representing retractions and deletions.
 */
object DiffInt {
  given diffIntGroup: AbelianGroup[Long] with {
    def zero: Long = 0L
    def one: Long = 1L
    def plus(a: Long, b: Long): Long = a + b
    def times(a: Long, b: Long): Long = a * b
    def negate(a: Long): Long = -a
  }
}

/**
 * Tropical (Min-Plus) Semiring over extended reals (R union {+inf}, min, +, +inf, 0).
 * Addition is min (optimal choice).
 * Multiplication is scalar addition (accumulation of edge weights).
 * Zero element is positive infinity (no path).
 * One element is 0.0 (identity path length).
 */
final case class Tropical(value: Double) extends AnyVal {
  def isInfinite: Boolean = value.isPosInfinity
}

object Tropical {
  val Infinity: Tropical = Tropical(Double.PositiveInfinity)
  val ZeroPath: Tropical = Tropical(0.0)

  given tropicalSemiring: LatticeSemiring[Tropical] with {
    def zero: Tropical = Infinity
    def one: Tropical = ZeroPath

    def plus(a: Tropical, b: Tropical): Tropical = {
      if (a.value <= b.value) a else b
    }

    def times(a: Tropical, b: Tropical): Tropical = {
      if (a.isInfinite || b.isInfinite) Infinity
      else Tropical(a.value + b.value)
    }
  }
}

/**
 * Boolean Lattice Semiring (B, ||, &&, false, true).
 * Models classical reachability and existence without multi-set counts.
 */
object BoolLattice {
  given boolSemiring: LatticeSemiring[Boolean] with {
    def zero: Boolean = false
    def one: Boolean = true
    def plus(a: Boolean, b: Boolean): Boolean = a || b
    def times(a: Boolean, b: Boolean): Boolean = a && b
  }
}

/**
 * Fuzzy Logic Semiring over unit interval [0, 1].
 * Addition is max (strongest possibility).
 * Multiplication is min (conjunction under Gödel t-norm).
 */
final case class Fuzzy(value: Double) extends AnyVal

object Fuzzy {
  def safe(v: Double): Fuzzy = {
    require(v >= 0.0 && v <= 1.0, s"Fuzzy value must be in [0, 1], got $v")
    Fuzzy(v)
  }

  val Zero: Fuzzy = Fuzzy(0.0)
  val One: Fuzzy = Fuzzy(1.0)

  given fuzzySemiring: LatticeSemiring[Fuzzy] with {
    def zero: Fuzzy = Zero
    def one: Fuzzy = One
    def plus(a: Fuzzy, b: Fuzzy): Fuzzy = Fuzzy(math.max(a.value, b.value))
    def times(a: Fuzzy, b: Fuzzy): Fuzzy = Fuzzy(math.min(a.value, b.value))
  }
}

/**
 * Independent Probability Semiring over [0, 1].
 * Addition represents union of independent events: P(A union B) = P(A) + P(B) - P(A)*P(B).
 * Multiplication represents joint independence: P(A intersect B) = P(A) * P(B).
 */
final case class Probabilistic(prob: Double) extends AnyVal

object Probabilistic {
  def safe(p: Double): Probabilistic = {
    require(p >= 0.0 && p <= 1.0, s"Probability must be in [0, 1], got $p")
    Probabilistic(p)
  }

  val Zero: Probabilistic = Probabilistic(0.0)
  val One: Probabilistic = Probabilistic(1.0)

  given probSemiring: Semiring[Probabilistic] with {
    def zero: Probabilistic = Zero
    def one: Probabilistic = One
    def plus(a: Probabilistic, b: Probabilistic): Probabilistic = {
      val res = a.prob + b.prob - (a.prob * b.prob)
      Probabilistic(math.min(1.0, math.max(0.0, res)))
    }
    def times(a: Probabilistic, b: Probabilistic): Probabilistic = {
      Probabilistic(a.prob * b.prob)
    }
  }
}

/**
 * Security and Access Control Lattice Semiring.
 * Levels: Public < Internal < Confidential < Secret.
 * Addition is meet (least privileged access required to see either source).
 * Multiplication is join (most privileged clearance required to see combined derivation).
 */
enum SecurityLevel(val level: Int) {
  case Public extends SecurityLevel(0)
  case Internal extends SecurityLevel(1)
  case Confidential extends SecurityLevel(2)
  case Secret extends SecurityLevel(3)
}

object SecurityLevel {
  given securitySemiring: LatticeSemiring[SecurityLevel] with {
    def zero: SecurityLevel = SecurityLevel.Secret
    def one: SecurityLevel = SecurityLevel.Public

    def plus(a: SecurityLevel, b: SecurityLevel): SecurityLevel = {
      if (a.level <= b.level) a else b
    }

    def times(a: SecurityLevel, b: SecurityLevel): SecurityLevel = {
      if (a.level >= b.level) a else b
    }
  }
}
