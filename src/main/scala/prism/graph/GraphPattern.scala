package prism.graph

import prism.datalog.ast._

final case class NodePattern(varName: String, label: Option[String] = None)
final case class EdgePattern(varName: Option[String], label: Option[String] = None)
final case class PathStep(fromNode: NodePattern, edge: EdgePattern, toNode: NodePattern)

/**
 * Graph pattern compiler translating graph path patterns into Datalog rules.
 */
object GraphPattern {

  def compilePath(
    resultPredicate: String,
    steps: List[PathStep]
  ): Rule = {
    require(steps.nonEmpty, "Path pattern must contain at least one step")

    val bodyLiterals = collection.mutable.ListBuffer[Literal]()

    for (step <- steps) {
      val edgePred = step.edge.label.getOrElse("edge")
      val fromTerm = Term.Var(step.fromNode.varName)
      val toTerm = Term.Var(step.toNode.varName)

      bodyLiterals += PositiveAtom(edgePred, List(fromTerm, toTerm))

      step.fromNode.label.foreach { lbl =>
        bodyLiterals += PositiveAtom("hasLabel", List(fromTerm, Term.Const(prism.core.data.Datum.Str(lbl))))
      }
      step.toNode.label.foreach { lbl =>
        bodyLiterals += PositiveAtom("hasLabel", List(toTerm, Term.Const(prism.core.data.Datum.Str(lbl))))
      }
    }

    val headTerms = List(
      Term.Var(steps.head.fromNode.varName),
      Term.Var(steps.last.toNode.varName)
    )

    Rule(PositiveAtom(resultPredicate, headTerms), bodyLiterals.toList)
  }
}
