package prism.egraph.validator

import prism.egraph._

/**
 * Boolean logic simplifier employing equality saturation with De Morgan laws and absorption.
 */
object BooleanSimplifier {

  val booleanRules: List[Rewrite] = {
    import Pattern._
    val x = v("x")
    val y = v("y")
    val t = c("true")
    val f = c("false")

    List(
      // Annihilation and identity
      Rewrite.rule("and_false", op("and", x, f), f),
      Rewrite.rule("and_true", op("and", x, t), x),
      Rewrite.rule("or_true", op("or", x, t), t),
      Rewrite.rule("or_false", op("or", x, f), x),
      // Idempotence
      Rewrite.rule("and_self", op("and", x, x), x),
      Rewrite.rule("or_self", op("or", x, x), x),
      // Double negation
      Rewrite.rule("not_not", op("not", op("not", x)), x),
      // De Morgan
      Rewrite.rule("demorgan_and", op("not", op("and", x, y)), op("or", op("not", x), op("not", y))),
      Rewrite.rule("demorgan_or", op("not", op("or", x, y)), op("and", op("not", x), op("not", y)))
    )
  }

  def simplify(expr: Expr): Expr = {
    val eg = new EGraph()
    val id = eg.addExpr(expr)
    val engine = new SaturationEngine(booleanRules, maxIterations = 8, nodeLimit = 2000)
    engine.saturate(eg)
    val extractor = new Extractor(eg, AstSizeCost)
    extractor.extract(id)
  }
}
