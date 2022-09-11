package prism.datalog.analysis

import prism.datalog.ast._

class SafetyException(message: String) extends RuntimeException(s"Safety Error: $message")

/**
 * Validates Datalog safety conditions:
 * In a safe Datalog rule, every variable appearing in the head, in a negated literal,
 * in an arithmetic expression, or in a comparison must be bound by at least one
 * positive relational atom in the rule body.
 */
object Safety {
  def validate(rule: Rule): Unit = {
    // 1. Gather all variables bound by positive atoms in the body
    val positivelyBound = rule.body.collect {
      case PositiveAtom(_, terms) => terms.flatMap(_.variables)
      case AggLiteral(resVar, _, _, innerAtom) =>
        resVar :: innerAtom.terms.flatMap(_.variables)
    }.flatten.toSet

    // 2. Validate head variables
    val headVars = rule.head.variables
    val unboundHeadVars = headVars -- positivelyBound
    if (unboundHeadVars.nonEmpty) {
      throw new SafetyException(
        s"Head variable(s) ${unboundHeadVars.mkString(", ")} in rule '${rule}' are not bound by any positive body atom"
      )
    }

    // 3. Validate negated atom variables
    for (lit <- rule.body) {
      lit match {
        case NegatedAtom(pred, terms) =>
          val negVars = terms.flatMap(_.variables).toSet
          val unboundNegVars = negVars -- positivelyBound
          if (unboundNegVars.nonEmpty) {
            throw new SafetyException(
              s"Negated atom 'not $pred' in rule '${rule}' contains unbound variable(s): ${unboundNegVars.mkString(", ")}"
            )
          }

        case ComparisonLiteral(_, left, right) =>
          val compVars = left.variables ++ right.variables
          val unboundCompVars = compVars -- positivelyBound
          if (unboundCompVars.nonEmpty) {
            throw new SafetyException(
              s"Comparison literal in rule '${rule}' contains unbound variable(s): ${unboundCompVars.mkString(", ")}"
            )
          }

        case _ => ()
      }
    }
  }

  def validateProgram(program: Program): Unit = {
    for (rule <- program.rules) {
      validate(rule)
    }
  }
}
