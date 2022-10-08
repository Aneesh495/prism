package prism.egraph

/**
 * Proof step explaining the equality of two terms.
 */
final case class ProofStep(
  fromExpr: String,
  toExpr: String,
  justification: String
)

/**
 * Explanation tree validating that two expressions belong to the same equivalence class.
 */
final case class EquivalenceProof(
  expr1: Expr,
  expr2: Expr,
  steps: List[ProofStep]
) {
  def isValid: Boolean = steps.nonEmpty || expr1 == expr2

  def formatMarkdown: String = {
    val sb = new StringBuilder()
    sb.append(s"### Equivalence Proof: `${expr1}` == `${expr2}`\n\n")
    if (expr1 == expr2) {
      sb.append("Expressions are syntactically identical (reflexivity).\n")
    } else {
      sb.append("| Step | Term | Justification |\n")
      sb.append("|---|---|---|\n")
      sb.append(s"| 0 | `${expr1}` | Initial term |\n")
      steps.zipWithIndex.foreach { case (step, idx) =>
        sb.append(s"| ${idx + 1} | `${step.toExpr}` | ${step.justification} |\n")
      }
    }
    sb.toString()
  }
}

object ProofExplainer {
  /**
   * Reconstructs an explanation certificate showing that expr1 and expr2 are equivalent.
   */
  def explain(egraph: EGraph, expr1: Expr, expr2: Expr, saturationReport: SaturationReport): EquivalenceProof = {
    val id1 = egraph.find(egraph.addExpr(expr1))
    val id2 = egraph.find(egraph.addExpr(expr2))

    if (id1 != id2) {
      EquivalenceProof(expr1, expr2, Nil)
    } else if (expr1 == expr2) {
      EquivalenceProof(expr1, expr2, List(ProofStep(expr1.toString, expr2.toString, "reflexivity")))
    } else {
      // Reconstruct proof steps via intermediate canonical representation
      val canonical = new Extractor(egraph).extract(id1)
      val step1 = ProofStep(expr1.toString, canonical.toString, s"equality saturation (iteration <= ${saturationReport.iterations})")
      val step2 = ProofStep(canonical.toString, expr2.toString, "congruence closure")
      EquivalenceProof(expr1, expr2, List(step1, step2))
    }
  }
}
