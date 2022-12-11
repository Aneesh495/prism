package prism.egraph.validator

import prism.egraph._

sealed trait ValidationResult
object ValidationResult {
  final case class Equivalent(proof: EquivalenceProof) extends ValidationResult
  final case class NonEquivalent(reason: String) extends ValidationResult
}

/**
 * Translation validator verifying compiler rewrite transformations.
 * Proves that an unoptimized expression and an optimized expression
 * compute identical outputs across all possible variable valuations.
 */
object TranslationValidator {

  def validate(
    original: Expr,
    optimized: Expr,
    rules: List[Rewrite] = Rewrite.standardArithmeticRules,
    maxIterations: Int = 10
  ): ValidationResult = {
    val egraph = new EGraph()
    val idOrig = egraph.addExpr(original)
    val idOpt = egraph.addExpr(optimized)

    val engine = new SaturationEngine(rules, maxIterations = maxIterations)
    val report = engine.saturate(egraph)

    if (egraph.find(idOrig) == egraph.find(idOpt)) {
      val proof = ProofExplainer.explain(egraph, original, optimized, report)
      ValidationResult.Equivalent(proof)
    } else {
      ValidationResult.NonEquivalent(
        s"Terms did not converge to the same equivalence class after ${report.iterations} iterations"
      )
    }
  }
}
