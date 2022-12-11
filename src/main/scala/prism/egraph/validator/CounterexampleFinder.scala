package prism.egraph.validator

import prism.egraph.Expr

final case class Counterexample(
  valuation: Map[String, Long],
  originalResult: Long,
  optimizedResult: Long
)

/**
 * Counterexample synthesizer searching for input inputs that disprove term equivalence.
 */
object CounterexampleFinder {

  def find(expr1: Expr, expr2: Expr): Option[Counterexample] = {
    val vars = (extractVars(expr1) ++ extractVars(expr2)).toList
    val domain = List(-2L, -1L, 0L, 1L, 2L, 5L, 10L)

    val valuations = generateValuations(vars, domain)

    valuations.view.flatMap { valMap =>
      val r1 = evaluate(expr1, valMap)
      val r2 = evaluate(expr2, valMap)
      if (r1 != r2) Some(Counterexample(valMap, r1, r2)) else None
    }.headOption
  }

  private def extractVars(expr: Expr): Set[String] = expr match {
    case Expr.Var(name) => Set(name)
    case Expr.Const(_) => Set.empty
    case Expr.Op(_, args) => args.flatMap(extractVars).toSet
  }

  private def generateValuations(vars: List[String], domain: List[Long]): List[Map[String, Long]] = {
    vars match {
      case Nil => List(Map.empty)
      case head :: tail =>
        val sub = generateValuations(tail, domain)
        for {
          v <- domain
          m <- sub
        } yield m + (head -> v)
    }
  }

  private def evaluate(expr: Expr, valuation: Map[String, Long]): Long = expr match {
    case Expr.Var(name) => valuation.getOrElse(name, 0L)
    case Expr.Const(v) => v
    case Expr.Op(op, args) =>
      val evaluatedArgs = args.map(a => evaluate(a, valuation))
      op match {
        case "+" => evaluatedArgs.sum
        case "-" => evaluatedArgs(0) - evaluatedArgs(1)
        case "*" => evaluatedArgs.product
        case "/" => if (evaluatedArgs(1) != 0L) evaluatedArgs(0) / evaluatedArgs(1) else 0L
        case _ => 0L
      }
  }
}
