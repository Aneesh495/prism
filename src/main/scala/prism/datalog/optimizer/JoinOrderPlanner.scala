package prism.datalog.optimizer

import prism.datalog.ast.{Literal, PositiveAtom, Rule}
import scala.collection.mutable

/**
 * Join ordering planner minimizing intermediate Cartesian cross-products.
 * Reorders positive body atoms using variable connectivity graphs.
 */
object JoinOrderPlanner {

  def plan(rule: Rule, relationSizes: Map[String, Long] = Map.empty): Rule = {
    val positiveAtoms = rule.body.collect { case p: PositiveAtom => p }
    val otherLiterals = rule.body.filterNot(_.isInstanceOf[PositiveAtom])

    if (positiveAtoms.length < 2) return rule

    // Greedy heuristic: start with smallest relation, next pick atom with maximum variable overlap
    val orderedPositive = mutable.ListBuffer[PositiveAtom]()
    val remaining = mutable.ListBuffer[PositiveAtom](positiveAtoms*)

    // Pick first atom: smallest estimated size or first
    val first = remaining.minBy(a => relationSizes.getOrElse(a.predicate, 1000L))
    orderedPositive += first
    remaining -= first

    val boundVars = mutable.Set[String]()
    boundVars ++= first.terms.flatMap(_.variables)

    while (remaining.nonEmpty) {
      // Pick atom sharing the most variables with currently bound set
      val next = remaining.maxBy { a =>
        val shared = a.terms.flatMap(_.variables).toSet.intersect(boundVars).size
        val sizePenalty = relationSizes.getOrElse(a.predicate, 1000L).toDouble
        (shared * 10000.0) - sizePenalty
      }
      orderedPositive += next
      remaining -= next
      boundVars ++= next.terms.flatMap(_.variables)
    }

    val finalBody = (orderedPositive ++ otherLiterals).toList
    Rule(rule.head, finalBody)
  }
}
