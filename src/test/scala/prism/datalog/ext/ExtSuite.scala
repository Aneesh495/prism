package prism.datalog.ext

import munit.FunSuite
import prism.core.data.{Datum, Tuple}
import prism.datalog.ast._

class ExtSuite extends FunSuite {

  test("TypeInference correctly infers types from facts and rules") {
    val fact1 = Fact("edge", List(Datum.I64(1), Datum.I64(2)))
    val fact2 = Fact("edge", List(Datum.I64(2), Datum.I64(3)))

    // path(X, Y) :- edge(X, Y).
    // path(X, Z) :- path(X, Y), edge(Y, Z).
    val rule1 = Rule(
      PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))),
      List(PositiveAtom("edge", List(Term.Var("X"), Term.Var("Y"))))
    )
    val rule2 = Rule(
      PositiveAtom("path", List(Term.Var("X"), Term.Var("Z"))),
      List(
        PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))),
        PositiveAtom("edge", List(Term.Var("Y"), Term.Var("Z")))
      )
    )

    val program = Program(List(fact1, fact2), List(rule1, rule2))
    val ti = new TypeInference()
    val signatures = ti.inferProgram(program)

    assertEquals(signatures("edge"), List(DType.TInt, DType.TInt))
    assertEquals(signatures("path"), List(DType.TInt, DType.TInt))
  }

  test("TypeInference rejects incompatible type unifications") {
    val fact = Fact("person", List(Datum.Str("Alice"), Datum.I64(30)))
    val badRule = Rule(
      PositiveAtom("res", List(Term.Var("X"))),
      List(
        PositiveAtom("person", List(Term.Var("X"), Term.Var("Age"))),
        // Age is int, trying to add string
        PositiveAtom("person", List(Term.Var("Age"), Term.Var("Y")))
      )
    )
    val program = Program(List(fact), List(badRule))
    val ti = new TypeInference()
    intercept[TypeError] {
      ti.inferProgram(program)
    }
  }

  test("PlanExplainer generates structured physical execution plan") {
    val fact = Fact("edge", List(Datum.I64(1), Datum.I64(2)))
    val rule1 = Rule(
      PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))),
      List(PositiveAtom("edge", List(Term.Var("X"), Term.Var("Y"))))
    )
    val rule2 = Rule(
      PositiveAtom("path", List(Term.Var("X"), Term.Var("Z"))),
      List(
        PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))),
        PositiveAtom("edge", List(Term.Var("Y"), Term.Var("Z")))
      )
    )
    val program = Program(List(fact), List(rule1, rule2))
    val explanation = PlanExplainer.explain(program)

    assert(explanation.contains("PRISM PHYSICAL EXECUTION PLAN"))
    assert(explanation.contains("Stratum"))
    assert(explanation.contains("IterateOp Feedback Loop"))
  }

  test("AggCompiler evaluates mean, variance, and median aggregations") {
    // Data: values 10, 20, 30 with multiplicity 1
    val items = List(
      (Tuple(Datum.Str("k1"), Datum.F64(10.0)), 1L),
      (Tuple(Datum.Str("k1"), Datum.F64(20.0)), 1L),
      (Tuple(Datum.Str("k1"), Datum.F64(30.0)), 1L)
    )

    val meanRes = AggCompiler.Mean(1).compute(items)
    assertEquals(meanRes, Some(Datum.F64(20.0)))

    // Sample variance of [10, 20, 30] is ((10-20)^2 + (20-20)^2 + (30-20)^2) / 2 = 200 / 2 = 100.0
    val varRes = AggCompiler.Variance(1).compute(items)
    assertEquals(varRes, Some(Datum.F64(100.0)))

    val medianRes = AggCompiler.Median(1).compute(items)
    assertEquals(medianRes, Some(Datum.F64(20.0)))
  }
}
