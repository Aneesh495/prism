package prism.datalog.optimizer

import prism.core.data.Datum
import prism.datalog.ast._
import scala.collection.mutable

/**
 * Ast rule rewriter applying compiler optimizations:
 * 1. Constant folding in arithmetic terms
 * 2. Filter pushdown: places comparisons as early as their variables are bound
 * 3. Deduplication of identical body literals
 */
object RuleRewriter {

  def optimize(rule: Rule): Rule = {
    val foldedHead = foldAtom(rule.head)
    val foldedBody = rule.body.map(foldLiteral)
    val deduplicatedBody = foldedBody.distinct
    val pushedBody = pushdownFilters(deduplicatedBody)
    Rule(foldedHead, pushedBody)
  }

  def optimizeProgram(program: Program): Program = {
    Program(
      facts = program.facts,
      rules = program.rules.map(optimize),
      queries = program.queries
    )
  }

  private def foldAtom(atom: PositiveAtom): PositiveAtom = {
    PositiveAtom(atom.predicate, atom.terms.map(foldTerm))
  }

  private def foldLiteral(lit: Literal): Literal = {
    lit match {
      case p: PositiveAtom => foldAtom(p)
      case NegatedAtom(pred, terms) => NegatedAtom(pred, terms.map(foldTerm))
      case ComparisonLiteral(op, left, right) => ComparisonLiteral(op, foldTerm(left), foldTerm(right))
      case a: AggLiteral => a
    }
  }

  private def foldTerm(term: Term): Term = {
    term match {
      case Term.BinaryExpr(op, left, right) =>
        val fLeft = foldTerm(left)
        val fRight = foldTerm(right)
        (fLeft, fRight) match {
          case (Term.Const(Datum.I64(v1)), Term.Const(Datum.I64(v2))) =>
            val res = op match {
              case BinOp.Add => v1 + v2
              case BinOp.Sub => v1 - v2
              case BinOp.Mul => v1 * v2
              case BinOp.Div => if (v2 != 0L) v1 / v2 else 0L
              case BinOp.Mod => if (v2 != 0L) v1 % v2 else 0L
            }
            Term.Const(Datum.I64(res))
          case _ => Term.BinaryExpr(op, fLeft, fRight)
        }
      case other => other
    }
  }

  /**
   * Reorders body literals to place comparisons immediately after all their variables are bound.
   */
  private def pushdownFilters(literals: List[Literal]): List[Literal] = {
    val positiveAtoms = literals.collect { case p: PositiveAtom => p }
    val comparisons = literals.collect { case c: ComparisonLiteral => c }
    val otherLits = literals.filterNot(l => l.isInstanceOf[PositiveAtom] || l.isInstanceOf[ComparisonLiteral])

    if (positiveAtoms.isEmpty) return literals

    val result = mutable.ListBuffer[Literal]()
    val boundVars = mutable.Set[String]()
    val remainingComparisons = mutable.ListBuffer[ComparisonLiteral](comparisons*)

    for (atom <- positiveAtoms) {
      result += atom
      boundVars ++= atom.terms.flatMap(_.variables)

      // Emit comparisons whose variables are now completely bound
      val readyComps = remainingComparisons.filter(c => c.variables.subsetOf(boundVars))
      result ++= readyComps
      remainingComparisons --= readyComps
    }

    result ++= remainingComparisons
    result ++= otherLits
    result.toList
  }
}
