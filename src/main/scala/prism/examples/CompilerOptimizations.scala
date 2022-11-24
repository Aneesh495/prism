package prism.examples

import prism.egraph._

/**
 * Compiler optimization pass pipeline implemented via equality saturation.
 * Performs strength reduction, constant propagation, and dead subterm pruning.
 */
object CompilerOptimizations {

  val strengthReductionRules: List[Rewrite] = {
    import Pattern._
    val x = v("x")
    val zero = c("0")
    val one = c("1")
    val two = c("2")

    List(
      Rewrite.rule("mul_by_two_to_shift", op("*", x, two), op("shl", x, one)),
      Rewrite.rule("mul_by_one", op("*", x, one), x),
      Rewrite.rule("add_zero", op("+", x, zero), x),
      Rewrite.rule("sub_self", op("-", x, x), zero),
      Rewrite.rule("div_self", op("/", x, x), one)
    )
  }

  def optimize(expr: Expr): (Expr, SaturationReport) = {
    val egraph = new EGraph()
    val targetClass = egraph.addExpr(expr)

    val engine = new SaturationEngine(
      Rewrite.standardArithmeticRules ++ strengthReductionRules,
      maxIterations = 8,
      nodeLimit = 2000
    )

    val report = engine.saturate(egraph)
    val extractor = new Extractor(egraph, OperatorLatencyCost)
    val optimalExpr = extractor.extract(targetClass)

    (optimalExpr, report)
  }
}
