package prism.egraph.validator

import munit.FunSuite
import prism.egraph.Expr
import prism.egraph.Expr._

class ValidatorSuite extends FunSuite {

  test("BooleanSimplifier simplifies double negations and identity operations") {
    // not(not(x)) -> x
    val expr1 = Op("not", List(Op("not", List(Var("x")))))
    val simplified1 = BooleanSimplifier.simplify(expr1)
    assertEquals(simplified1, Var("x"))

    // and(x, false) -> false
    val expr2 = Op("and", List(Var("x"), Const(0L))) // in boolean domain 0 is false
    // or test and_true: and(x, true) -> x
    val expr3 = Op("and", List(Var("x"), Var("true")))
    val simplified3 = BooleanSimplifier.simplify(expr3)
    assertEquals(simplified3, Var("x"))
  }

  test("TranslationValidator proves arithmetic rewrite equivalence") {
    // expr1: (x + 0) * 1
    // expr2: x
    val expr1 = Op("*", List(Op("+", List(Var("x"), Const(0L))), Const(1L)))
    val expr2 = Var("x")

    val result = TranslationValidator.validate(expr1, expr2)
    result match {
      case ValidationResult.Equivalent(proof) =>
        assert(proof.steps.nonEmpty)
      case ValidationResult.NonEquivalent(reason) =>
        fail(s"Expected terms to be equivalent, but validation failed: $reason")
    }
  }

  test("CounterexampleFinder synthesizes valuations disproving false claims") {
    // expr1: x + 1
    // expr2: x + 2
    val expr1 = Op("+", List(Var("x"), Const(1L)))
    val expr2 = Op("+", List(Var("x"), Const(2L)))

    val cexOpt = CounterexampleFinder.find(expr1, expr2)
    assert(cexOpt.isDefined)
    val cex = cexOpt.get
    assert(cex.originalResult != cex.optimizedResult)
    val xVal = cex.valuation("x")
    assertEquals(cex.originalResult, xVal + 1L)
    assertEquals(cex.optimizedResult, xVal + 2L)
  }
}
