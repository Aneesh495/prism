package prism.egraph

import scala.collection.mutable

final case class SaturationReport(
  iterations: Int,
  finalClasses: Int,
  finalNodes: Int,
  isSaturated: Boolean,
  durationMs: Long
)

/**
 * Equality Saturation Engine.
 * Iteratively applies rewrite rules to an e-graph until saturation or budget exhaustion.
 */
final class SaturationEngine(
  val rules: List[Rewrite],
  val maxIterations: Int = 15,
  val nodeLimit: Int = 10000,
  val matchLimitPerRule: Int = 200
) {
  def saturate(egraph: EGraph): SaturationReport = {
    val startTime = System.currentTimeMillis()
    var iter = 0
    var saturated = false

    while (iter < maxIterations && !saturated && egraph.numNodes < nodeLimit) {
      iter += 1

      // 1. Match phase: find all matches for all rewrite rules
      val matchesWithRule = mutable.ArrayBuffer[(Rewrite, Match)]()
      for (rule <- rules) {
        val matches = EMatcher.search(egraph, rule.lhs).take(matchLimitPerRule)
        for (m <- matches) {
          matchesWithRule += ((rule, m))
        }
      }

      // 2. Apply phase: instantiate RHS and merge
      var anyApplied = false
      for ((rule, m) <- matchesWithRule) {
        val applied = rule.apply(egraph, m)
        if (applied) anyApplied = true
      }

      // 3. Rebuild phase: restore congruence closure invariants
      egraph.rebuild()

      // If no new equivalences were discovered, the e-graph is saturated
      if (!anyApplied) {
        saturated = true
      }
    }

    val duration = System.currentTimeMillis() - startTime
    SaturationReport(
      iterations = iter,
      finalClasses = egraph.numClasses,
      finalNodes = egraph.numNodes,
      isSaturated = saturated,
      durationMs = duration
    )
  }
}
