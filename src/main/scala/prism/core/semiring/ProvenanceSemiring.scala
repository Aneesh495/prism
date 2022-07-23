package prism.core.semiring

import scala.collection.immutable.SortedMap

/**
 * Monomial representing a product of algebraic provenance variables:
 * x_1^{e_1} * x_2^{e_2} * ... * x_k^{e_k}.
 * Variables are sorted canonically by name.
 */
final case class Monomial(factors: SortedMap[String, Int]) {
  def isEmpty: Boolean = factors.isEmpty

  def multiply(that: Monomial): Monomial = {
    var merged = this.factors
    for ((k, exp) <- that.factors) {
      val curr = merged.getOrElse(k, 0)
      merged = merged.updated(k, curr + exp)
    }
    Monomial(merged)
  }

  def variables: Set[String] = factors.keySet

  def evaluate(valuation: Map[String, Double]): Double = {
    factors.foldLeft(1.0) { case (acc, (varName, exp)) =>
      val base = valuation.getOrElse(varName, 0.0)
      acc * math.pow(base, exp)
    }
  }

  def evaluateBoolean(activeVars: Set[String]): Boolean = {
    factors.keys.forall(activeVars.contains)
  }

  def derivative(varName: String): (Int, Monomial) = {
    factors.get(varName) match {
      case Some(exp) if exp > 1 =>
        (exp, Monomial(factors.updated(varName, exp - 1)))
      case Some(1) =>
        (1, Monomial(factors - varName))
      case _ =>
        (0, Monomial.One)
    }
  }

  override def toString: String = {
    if (factors.isEmpty) "1"
    else factors.map {
      case (k, 1) => k
      case (k, exp) => s"$k^$exp"
    }.mkString(" * ")
  }
}

object Monomial {
  val One: Monomial = Monomial(SortedMap.empty[String, Int])
  def variable(name: String): Monomial = Monomial(SortedMap(name -> 1))
}

/**
 * Provenance Polynomial in N[X] representing algebraic lineage.
 * Form: sum_i (coeff_i * monomial_i)
 * 
 * Formal semiring properties:
 * - Addition represents alternative derivation paths (unions/projections)
 * - Multiplication represents joint conjunctive dependency (joins)
 * - Coefficients count the multiplicity of derivations
 */
final case class ProvenancePolynomial(terms: Map[Monomial, Long]) {
  def isZero: Boolean = terms.isEmpty
  def isOne: Boolean = terms.size == 1 && terms.get(Monomial.One).contains(1L)

  def +(that: ProvenancePolynomial): ProvenancePolynomial = {
    var merged = this.terms
    for ((mono, coeff) <- that.terms) {
      val curr = merged.getOrElse(mono, 0L)
      val updated = curr + coeff
      if (updated == 0L) {
        merged = merged - mono
      } else {
        merged = merged.updated(mono, updated)
      }
    }
    ProvenancePolynomial(merged)
  }

  def *(that: ProvenancePolynomial): ProvenancePolynomial = {
    var result = Map.empty[Monomial, Long]
    for {
      (m1, c1) <- this.terms
      (m2, c2) <- that.terms
    } {
      val productMono = m1.multiply(m2)
      val productCoeff = c1 * c2
      val existing = result.getOrElse(productMono, 0L)
      val updated = existing + productCoeff
      if (updated == 0L) {
        result = result - productMono
      } else {
        result = result.updated(productMono, updated)
      }
    }
    ProvenancePolynomial(result)
  }

  def variables: Set[String] = {
    terms.keys.flatMap(_.variables).toSet
  }

  /**
   * Evaluates the polynomial given numeric values for variables.
   */
  def evaluate(valuation: Map[String, Double]): Double = {
    terms.foldLeft(0.0) { case (acc, (mono, coeff)) =>
      acc + (coeff.toDouble * mono.evaluate(valuation))
    }
  }

  /**
   * Boolean reachability: returns true if the polynomial evaluates to > 0
   * when activeVars are set to 1 and all other variables to 0.
   */
  def evaluateBoolean(activeVars: Set[String]): Boolean = {
    terms.keys.exists(_.evaluateBoolean(activeVars))
  }

  /**
   * Minimal witnesses (Why-provenance):
   * Returns the minimal subsets of variables whose presence guarantees derivation.
   */
  def minimalWitnesses: Set[Set[String]] = {
    val candidateWitnesses = terms.keys.map(_.variables).toSet
    candidateWitnesses.filterNot { w1 =>
      candidateWitnesses.exists(w2 => w2 != w1 && w2.subsetOf(w1))
    }
  }

  /**
   * Computes the formal partial derivative dP/dx with respect to an input fact token.
   * Useful for sensitivity analysis and marginal impact scoring in deductive graphs.
   */
  def partialDerivative(varName: String): ProvenancePolynomial = {
    var derived = Map.empty[Monomial, Long]
    for ((mono, coeff) <- terms) {
      val (exp, newMono) = mono.derivative(varName)
      if (exp > 0) {
        val newCoeff = coeff * exp
        val curr = derived.getOrElse(newMono, 0L)
        derived = derived.updated(newMono, curr + newCoeff)
      }
    }
    ProvenancePolynomial(derived)
  }

  override def toString: String = {
    if (terms.isEmpty) "0"
    else {
      terms.toList.sortBy(_._1.toString).map {
        case (Monomial.One, coeff) => s"$coeff"
        case (mono, 1L) => mono.toString
        case (mono, coeff) => s"$coeff*($mono)"
      }.mkString(" + ")
    }
  }
}

object ProvenancePolynomial {
  val Zero: ProvenancePolynomial = ProvenancePolynomial(Map.empty)
  val One: ProvenancePolynomial = ProvenancePolynomial(Map(Monomial.One -> 1L))

  def variable(name: String): ProvenancePolynomial = {
    ProvenancePolynomial(Map(Monomial.variable(name) -> 1L))
  }

  given provenanceSemiring: Semiring[ProvenancePolynomial] with {
    def zero: ProvenancePolynomial = Zero
    def one: ProvenancePolynomial = One
    def plus(a: ProvenancePolynomial, b: ProvenancePolynomial): ProvenancePolynomial = a + b
    def times(a: ProvenancePolynomial, b: ProvenancePolynomial): ProvenancePolynomial = a * b
  }
}
