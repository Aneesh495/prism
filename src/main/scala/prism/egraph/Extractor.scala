package prism.egraph

import scala.collection.mutable

/**
 * Cost function mapping an operator node and child costs to an aggregate numeric cost.
 */
trait CostFunction {
  def cost(node: ENode, childCosts: List[Double]): Double
}

object AstSizeCost extends CostFunction {
  def cost(node: ENode, childCosts: List[Double]): Double = {
    1.0 + childCosts.sum
  }
}

object OperatorLatencyCost extends CostFunction {
  def cost(node: ENode, childCosts: List[Double]): Double = {
    val nodeCost = node.op match {
      case "/" => 25.0
      case "%" => 25.0
      case "*" => 3.0
      case "+" => 1.0
      case "-" => 1.0
      case _ => 0.1
    }
    nodeCost + childCosts.sum
  }
}

/**
 * Cost-based expression extractor utilizing dynamic programming over the e-graph.
 */
final class Extractor(val egraph: EGraph, val costFunction: CostFunction = AstSizeCost) {
  private val costs = mutable.Map[EClassId, Double]()
  private val bestNodes = mutable.Map[EClassId, ENode]()

  computeBestCosts()

  private def computeBestCosts(): Unit = {
    var changed = true
    var iterations = 0
    val maxIter = 1000

    while (changed && iterations < maxIter) {
      changed = false
      iterations += 1

      for (eclass <- egraph.allClasses) {
        val canonicalId = egraph.find(eclass.id)
        for (node <- eclass.nodes) {
          val childClassIds = node.children.map(egraph.find)
          val allChildrenHaveCost = childClassIds.forall(costs.contains)

          if (allChildrenHaveCost) {
            val childCosts = childClassIds.map(costs)
            val currentCost = costFunction.cost(node, childCosts)
            val previousCost = costs.getOrElse(canonicalId, Double.PositiveInfinity)

            if (currentCost < previousCost) {
              costs.put(canonicalId, currentCost)
              bestNodes.put(canonicalId, node)
              changed = true
            }
          }
        }
      }
    }
  }

  /**
   * Extracts the minimum-cost Expr representing the equivalence class.
   */
  def extract(classId: EClassId): Expr = {
    val canonicalId = egraph.find(classId)
    bestNodes.get(canonicalId) match {
      case Some(bestNode) =>
        if (bestNode.children.isEmpty) {
          if (bestNode.op.forall(Character.isDigit) || (bestNode.op.startsWith("-") && bestNode.op.length > 1 && bestNode.op.tail.forall(Character.isDigit))) {
            Expr.Const(bestNode.op.toLong)
          } else {
            Expr.Var(bestNode.op)
          }
        } else {
          val childExprs = bestNode.children.map(extract)
          Expr.Op(bestNode.op, childExprs)
        }
      case None =>
        throw new IllegalStateException(s"No valid finite expression found for class $canonicalId")
    }
  }

  def costOf(classId: EClassId): Double = {
    costs.getOrElse(egraph.find(classId), Double.PositiveInfinity)
  }
}
