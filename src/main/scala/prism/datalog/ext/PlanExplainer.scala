package prism.datalog.ext

import prism.datalog.analysis.Stratifier
import prism.datalog.ast.Program

/**
 * Physical execution plan formatter detailing stratification, operator pipelines,
 * and dependency flow for Datalog programs.
 */
object PlanExplainer {

  def explain(program: Program): String = {
    val sb = new StringBuilder()
    sb.append("=================================================================\n")
    sb.append("                  PRISM PHYSICAL EXECUTION PLAN                  \n")
    sb.append("=================================================================\n\n")

    val strata = Stratifier.stratify(program)
    sb.append(s"Total Strata: ${strata.length}\n\n")

    strata.zipWithIndex.foreach { case (rules, idx) =>
      val heads = rules.map(_.head.predicate).distinct.mkString(", ")
      val isRecursive = rules.exists { r =>
        val headPred = r.head.predicate
        r.body.exists {
          case prism.datalog.ast.PositiveAtom(p, _) => p == headPred
          case _ => false
        }
      }

      val mode = if (isRecursive) "RECURSIVE (IterateOp Feedback Loop)" else "PIPELINED (DAG Feedforward)"
      sb.append(s"--- Stratum $idx: [$heads] ---\n")
      sb.append(s"  Execution Mode : $mode\n")
      sb.append(s"  Rule Count     : ${rules.length}\n")
      sb.append("  Operators      :\n")

      for (r <- rules) {
        val bodyStr = r.body.mkString(", ")
        sb.append(s"    * ${r.head} :- $bodyStr\n")
        val joins = r.body.count(_.isInstanceOf[prism.datalog.ast.PositiveAtom]) - 1
        if (joins > 0) {
          sb.append(s"      -> Bilinear Join Chain ($joins join stages)\n")
        }
        val antijoins = r.body.count(_.isInstanceOf[prism.datalog.ast.NegatedAtom])
        if (antijoins > 0) {
          sb.append(s"      -> Stratified Negation ($antijoins antijoin stages)\n")
        }
      }
      sb.append("\n")
    }

    sb.append("=================================================================\n")
    sb.toString()
  }
}
