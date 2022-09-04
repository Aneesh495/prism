package prism.datalog.ast

import prism.core.data.Datum

/**
 * Arithmetic binary operators supported in Datalog expressions.
 */
enum BinOp {
  case Add, Sub, Mul, Div, Mod
}

/**
 * Comparison relational operators for filter predicates.
 */
enum CmpOp(val symbol: String) {
  case Eq extends CmpOp("=")
  case Neq extends CmpOp("!=")
  case Lt extends CmpOp("<")
  case Lte extends CmpOp("<=")
  case Gt extends CmpOp(">")
  case Gte extends CmpOp(">=")
}

/**
 * Term in a Datalog atom: variable, constant literal, or arithmetic expression.
 */
sealed trait Term extends Serializable {
  def variables: Set[String]
}

object Term {
  final case class Var(name: String) extends Term {
    def variables: Set[String] = Set(name)
    override def toString: String = name
  }

  final case class Const(datum: Datum) extends Term {
    def variables: Set[String] = Set.empty
    override def toString: String = datum.toString
  }

  final case class BinaryExpr(op: BinOp, left: Term, right: Term) extends Term {
    def variables: Set[String] = left.variables ++ right.variables
    override def toString: String = s"($left ${op.toString.toLowerCase} $right)"
  }
}

/**
 * Body literal in a Datalog rule.
 */
sealed trait Literal extends Serializable {
  def variables: Set[String]
}

final case class PositiveAtom(predicate: String, terms: List[Term]) extends Literal {
  def variables: Set[String] = terms.flatMap(_.variables).toSet
  def arity: Int = terms.length
  override def toString: String = s"$predicate(${terms.mkString(", ")})"
}

final case class NegatedAtom(predicate: String, terms: List[Term]) extends Literal {
  def variables: Set[String] = terms.flatMap(_.variables).toSet
  def arity: Int = terms.length
  override def toString: String = s"not $predicate(${terms.mkString(", ")})"
}

final case class ComparisonLiteral(op: CmpOp, left: Term, right: Term) extends Literal {
  def variables: Set[String] = left.variables ++ right.variables
  override def toString: String = s"$left ${op.symbol} $right"
}

final case class AggLiteral(
  resultVar: String,
  functionName: String,
  targetVar: String,
  atom: PositiveAtom
) extends Literal {
  def variables: Set[String] = Set(resultVar, targetVar) ++ atom.variables
  override def toString: String = s"$resultVar = $functionName<$targetVar>($atom)"
}

/**
 * Horn clause rule: Head :- Body_1, Body_2, ..., Body_k.
 */
final case class Rule(
  head: PositiveAtom,
  body: List[Literal]
) {
  def variables: Set[String] = head.variables ++ body.flatMap(_.variables)
  override def toString: String = {
    if (body.isEmpty) s"$head."
    else s"$head :- ${body.mkString(", ")}."
  }
}

/**
 * Extensional database (EDB) ground fact.
 */
final case class Fact(
  predicate: String,
  values: List[Datum],
  weight: Long = 1L
) {
  override def toString: String = {
    val weightSuffix = if (weight != 1L) s" [weight=$weight]" else ""
    s"$predicate(${values.mkString(", ")})$weightSuffix."
  }
}

/**
 * Complete Datalog Program containing facts, rules, and query targets.
 */
final case class Program(
  facts: List[Fact] = Nil,
  rules: List[Rule] = Nil,
  queries: List[String] = Nil
) {
  def predicates: Set[String] = {
    facts.map(_.predicate).toSet ++ rules.map(_.head.predicate).toSet
  }
}
