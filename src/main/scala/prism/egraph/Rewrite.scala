package prism.egraph

/**
 * Conditional equality rewrite rule: LHS => RHS.
 */
final case class Rewrite(
  name: String,
  lhs: Pattern,
  rhs: Pattern,
  condition: Option[Match => Boolean] = None
) {
  /**
   * Applies this rewrite rule to an e-graph given an identified match.
   * Instantiates the RHS pattern and merges it with the matched e-class.
   */
  def apply(egraph: EGraph, m: Match): Boolean = {
    if (condition.forall(_(m))) {
      val rhsId = instantiate(egraph, rhs, m.subst)
      val before = egraph.find(m.matchedClass)
      val after = egraph.find(rhsId)
      if (before != after) {
        egraph.merge(m.matchedClass, rhsId)
        true
      } else false
    } else false
  }

  private def instantiate(egraph: EGraph, pattern: Pattern, subst: Map[String, EClassId]): EClassId = {
    pattern match {
      case Pattern.Var(vName) =>
        subst.getOrElse(vName, throw new IllegalArgumentException(s"Unbound pattern variable $vName"))
      case Pattern.Const(cVal) =>
        egraph.add(ENode(cVal, Nil))
      case Pattern.Op(opName, patArgs) =>
        val childIds = patArgs.map(instantiate(egraph, _, subst))
        egraph.add(ENode(opName, childIds))
    }
  }

  override def toString: String = s"Rewrite($name: $lhs => $rhs)"
}

object Rewrite {
  def rule(name: String, lhs: Pattern, rhs: Pattern): Rewrite = {
    Rewrite(name, lhs, rhs, None)
  }

  def bidirectional(name: String, p1: Pattern, p2: Pattern): List[Rewrite] = {
    List(
      Rewrite(s"${name}_lr", p1, p2, None),
      Rewrite(s"${name}_rl", p2, p1, None)
    )
  }

  /**
   * Standard library of arithmetic algebraic rewrites.
   */
  def standardArithmeticRules: List[Rewrite] = {
    import Pattern._
    val x = v("x")
    val y = v("y")
    val z = v("z")
    val zero = c("0")
    val one = c("1")

    List(
      // Commutativity
      rule("add_comm", op("+", x, y), op("+", y, x)),
      rule("mul_comm", op("*", x, y), op("*", y, x)),
      // Associativity
      rule("add_assoc", op("+", op("+", x, y), z), op("+", x, op("+", y, z))),
      rule("mul_assoc", op("*", op("*", x, y), z), op("*", x, op("*", y, z))),
      // Distributivity
      rule("mul_distrib", op("*", x, op("+", y, z)), op("+", op("*", x, y), op("*", x, z))),
      // Identity
      rule("add_zero", op("+", x, zero), x),
      rule("mul_one", op("*", x, one), x),
      // Annihilation
      rule("mul_zero", op("*", x, zero), zero),
      // Subtraction and division identities
      rule("sub_self", op("-", x, x), zero),
      rule("sub_zero", op("-", x, zero), x)
    )
  }
}
