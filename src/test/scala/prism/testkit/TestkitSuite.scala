package prism.testkit

import munit.FunSuite
import prism.core.data.Datum
import prism.core.semiring.{DiffInt, Tropical}
import prism.datalog.ast._

class TestkitSuite extends FunSuite {

  test("SemiringLawChecker validates DiffInt and Tropical algebraic axioms") {
    import DiffInt.given
    val diffSamples = Seq(-5L, -1L, 0L, 1L, 7L)
    assert(SemiringLawChecker.checkAxioms(diffSamples))

    val tropSamples = Seq(Tropical(0.0), Tropical(1.5), Tropical(10.0), Tropical.Infinity)
    assert(SemiringLawChecker.checkAxioms(tropSamples))
  }

  test("DifferentialInvarianceChecker asserts Q(D + dD) == Q(D) + dQ") {
    val initialFacts = List(
      Fact("edge", List(Datum.I64(1), Datum.I64(2))),
      Fact("edge", List(Datum.I64(2), Datum.I64(3)))
    )

    val rules = List(
      Rule(
        PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))),
        List(PositiveAtom("edge", List(Term.Var("X"), Term.Var("Y"))))
      ),
      Rule(
        PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))),
        List(
          PositiveAtom("path", List(Term.Var("X"), Term.Var("Z"))),
          PositiveAtom("edge", List(Term.Var("Z"), Term.Var("Y")))
        )
      )
    )

    val mutationFacts = List(
      Fact("edge", List(Datum.I64(3), Datum.I64(4)))
    )

    val isInvariant = DifferentialInvarianceChecker.verifyInvariance(
      rules,
      initialFacts,
      mutationFacts,
      "path"
    )

    assert(isInvariant, "Differential update must match scratch recomputation exactly")
  }
}
