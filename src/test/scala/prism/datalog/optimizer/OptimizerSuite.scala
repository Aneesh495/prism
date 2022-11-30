package prism.datalog.optimizer

import munit.FunSuite
import prism.core.data.Datum
import prism.datalog.ast._

class OptimizerSuite extends FunSuite {

  test("HyperLogLog estimates distinct elements within standard error") {
    val hll = new HyperLogLog(10)
    for (i <- 0 until 5000) {
      hll.add(i.toLong)
    }

    val estimate = hll.estimate()
    val err = math.abs(estimate - 5000L).toDouble / 5000.0
    // Standard error for b=10 is ~3.25%, verify within 10% tolerance
    assert(err < 0.10, s"Error $err exceeded threshold, estimate was $estimate")
  }

  test("EquiWidthHistogram calculates range selectivity accurately") {
    val hist = new EquiWidthHistogram(0.0, 100.0, 10)
    for (i <- 0 to 100) {
      hist.add(i.toDouble)
    }

    val sel = hist.estimateSelectivityLessOrEqual(50.0)
    // Roughly 50%
    assert(math.abs(sel - 0.5) < 0.05, s"Selectivity was $sel")
  }

  test("RuleRewriter performs constant folding and filter pushdown") {
    // p(X) :- a(X, Y), b(Y, Z), X > (2 + 3).
    val rule = Rule(
      PositiveAtom("p", List(Term.Var("X"))),
      List(
        PositiveAtom("a", List(Term.Var("X"), Term.Var("Y"))),
        PositiveAtom("b", List(Term.Var("Y"), Term.Var("Z"))),
        ComparisonLiteral(CmpOp.Gt, Term.Var("X"), Term.BinaryExpr(BinOp.Add, Term.Const(Datum.I64(2)), Term.Const(Datum.I64(3))))
      )
    )

    val optimized = RuleRewriter.optimize(rule)
    // Filter should be folded to X > 5 and pushed immediately after a(X, Y)
    assertEquals(optimized.body(0), PositiveAtom("a", List(Term.Var("X"), Term.Var("Y"))))
    assertEquals(optimized.body(1), ComparisonLiteral(CmpOp.Gt, Term.Var("X"), Term.Const(Datum.I64(5))))
    assertEquals(optimized.body(2), PositiveAtom("b", List(Term.Var("Y"), Term.Var("Z"))))
  }

  test("JoinOrderPlanner orders body atoms to minimize cross-products") {
    // r(X, Y, Z) :- b(Y, Z), a(X, Y).
    val rule = Rule(
      PositiveAtom("r", List(Term.Var("X"), Term.Var("Y"), Term.Var("Z"))),
      List(
        PositiveAtom("b", List(Term.Var("Y"), Term.Var("Z"))),
        PositiveAtom("a", List(Term.Var("X"), Term.Var("Y")))
      )
    )

    val planned = JoinOrderPlanner.plan(rule, Map("a" -> 10L, "b" -> 500L))
    // Smallest relation 'a' should be placed first
    assertEquals(planned.body.head.asInstanceOf[PositiveAtom].predicate, "a")
  }
}
