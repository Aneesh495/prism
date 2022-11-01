package prism.core.semiring

import munit.FunSuite
import prism.core.semiring.DiffInt.given
import prism.core.semiring.BoolLattice.given

class SemiringSuite extends FunSuite {

  test("DiffInt abelian group satisfies ring axioms") {
    val g = summon[AbelianGroup[Long]]
    val a = 5L
    val b = -3L

    assertEquals(g.plus(a, b), 2L)
    assertEquals(g.times(a, b), -15L)
    assertEquals(g.negate(a), -5L)
    assertEquals(g.minus(a, b), 8L)
    assertEquals(g.plus(a, g.zero), a)
    assertEquals(g.times(a, g.one), a)
    assertEquals(g.times(a, g.zero), g.zero)
  }

  test("Tropical semiring selects optimal path and accumulates costs") {
    val s = summon[LatticeSemiring[Tropical]]
    val p1 = Tropical(10.0)
    val p2 = Tropical(7.5)
    val edge = Tropical(1.2)

    // Plus is min
    assertEquals(s.plus(p1, p2), p2)

    // Times is scalar addition
    assertEquals(s.times(p2, edge), Tropical(8.7))

    // Zero is infinity, one is zero path
    assertEquals(s.times(p1, s.zero), s.zero)
    assertEquals(s.times(p1, s.one), p1)
  }

  test("BoolLattice models reachability") {
    val s = summon[LatticeSemiring[Boolean]]
    assertEquals(s.plus(false, true), true)
    assertEquals(s.times(true, false), false)
    assertEquals(s.times(true, true), true)
  }

  test("Fuzzy semiring computes max-min Gödel conjunctions") {
    val s = summon[LatticeSemiring[Fuzzy]]
    val f1 = Fuzzy.safe(0.8)
    val f2 = Fuzzy.safe(0.4)

    assertEquals(s.plus(f1, f2), f1) // max
    assertEquals(s.times(f1, f2), f2) // min
  }

  test("Probabilistic semiring computes independent union and product") {
    val s = summon[Semiring[Probabilistic]]
    val p1 = Probabilistic.safe(0.5)
    val p2 = Probabilistic.safe(0.5)

    // P(A union B) = 0.5 + 0.5 - 0.25 = 0.75
    val unionProb = s.plus(p1, p2)
    assertEquals(unionProb.prob, 0.75)

    // P(A intersect B) = 0.25
    val jointProb = s.times(p1, p2)
    assertEquals(jointProb.prob, 0.25)
  }

  test("Provenance polynomial arithmetic, evaluation, and symbolic derivatives") {
    val p1 = ProvenancePolynomial.variable("e1")
    val p2 = ProvenancePolynomial.variable("e2")
    val p3 = ProvenancePolynomial.variable("e3")

    // Alternative paths: e1 + e2
    val alt = p1 + p2
    // Conjunction with e3: (e1 + e2) * e3 = e1*e3 + e2*e3
    val total = alt * p3

    assertEquals(total.terms.size, 2)
    assertEquals(total.variables, Set("e1", "e2", "e3"))

    // Valuation test: e1=1, e2=2, e3=3 => (1 + 2) * 3 = 9
    val valuation = Map("e1" -> 1.0, "e2" -> 2.0, "e3" -> 3.0)
    assertEquals(total.evaluate(valuation), 9.0)

    // Boolean evaluation with active facts
    assert(total.evaluateBoolean(Set("e1", "e3")))
    assert(!total.evaluateBoolean(Set("e1", "e2"))) // misses e3

    // Minimal witnesses: {e1, e3} and {e2, e3}
    val witnesses = total.minimalWitnesses
    assertEquals(witnesses, Set(Set("e1", "e3"), Set("e2", "e3")))

    // Symbolic partial derivative d/d(e1) = e3
    val d_de1 = total.partialDerivative("e1")
    assertEquals(d_de1.variables, Set("e3"))
    assertEquals(d_de1.evaluate(valuation), 3.0)
  }
}
