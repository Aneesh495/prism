package prism.egraph

/**
 * Structural pattern with pattern variables for e-matching.
 */
sealed trait Pattern extends Serializable {
  def variables: Set[String]
}

object Pattern {
  final case class Var(name: String) extends Pattern {
    def variables: Set[String] = Set(name)
    override def toString: String = s"?$name"
  }

  final case class Const(value: String) extends Pattern {
    def variables: Set[String] = Set.empty
    override def toString: String = value
  }

  final case class Op(name: String, args: List[Pattern]) extends Pattern {
    def variables: Set[String] = args.flatMap(_.variables).toSet
    override def toString: String = {
      if (args.isEmpty) name
      else s"$name(${args.mkString(", ")})"
    }
  }

  def v(name: String): Pattern = Var(name)
  def c(value: String): Pattern = Const(value)
  def op(name: String, args: Pattern*): Pattern = Op(name, args.toList)
}

/**
 * Result of a pattern match over an E-Class, binding pattern variables to EClassIds.
 */
final case class Match(
  subst: Map[String, EClassId],
  matchedClass: EClassId
)

object EMatcher {
  /**
   * Finds all matches for a pattern across the entire e-graph.
   */
  def search(egraph: EGraph, pattern: Pattern): List[Match] = {
    egraph.allClasses.flatMap { eclass =>
      matchClass(egraph, pattern, eclass.id, Map.empty).map { subst =>
        Match(subst, eclass.id)
      }
    }.toList
  }

  private def matchClass(
    egraph: EGraph,
    pattern: Pattern,
    classId: EClassId,
    bindings: Map[String, EClassId]
  ): List[Map[String, EClassId]] = {
    val canonicalId = egraph.find(classId)
    pattern match {
      case Pattern.Var(vName) =>
        bindings.get(vName) match {
          case Some(boundId) =>
            if (egraph.find(boundId) == canonicalId) List(bindings)
            else Nil
          case None =>
            List(bindings + (vName -> canonicalId))
        }

      case Pattern.Const(cVal) =>
        egraph.getClass(canonicalId) match {
          case Some(eclass) if eclass.nodes.exists(n => n.op == cVal && n.children.isEmpty) =>
            List(bindings)
          case _ => Nil
        }

      case Pattern.Op(opName, patArgs) =>
        egraph.getClass(canonicalId) match {
          case Some(eclass) =>
            eclass.nodes.filter(n => n.op == opName && n.children.length == patArgs.length).toList.flatMap { node =>
              matchArgs(egraph, patArgs, node.children, bindings)
            }
          case None => Nil
        }
    }
  }

  private def matchArgs(
    egraph: EGraph,
    patArgs: List[Pattern],
    nodeChildren: List[EClassId],
    currentBindings: Map[String, EClassId]
  ): List[Map[String, EClassId]] = {
    (patArgs, nodeChildren) match {
      case (Nil, Nil) => List(currentBindings)
      case (pHead :: pTail, cHead :: cTail) =>
        val headMatches = matchClass(egraph, pHead, cHead, currentBindings)
        headMatches.flatMap { updatedBindings =>
          matchArgs(egraph, pTail, cTail, updatedBindings)
        }
      case _ => Nil
    }
  }
}
