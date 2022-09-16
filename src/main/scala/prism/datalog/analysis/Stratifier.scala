package prism.datalog.analysis

import prism.datalog.ast._
import scala.collection.mutable

class StratificationException(message: String)
  extends RuntimeException(s"Stratification Error: $message")

enum EdgeKind {
  case Positive, Negative, Aggregation
}

final case class DependencyEdge(
  from: String,
  to: String,
  kind: EdgeKind
)

/**
 * Computes stratification partitions for Datalog programs with negation and aggregation.
 * Employs Tarjan's Strongly Connected Components (SCC) algorithm to verify that no
 * cycles contain negative or aggregation dependencies.
 */
object Stratifier {

  def stratify(program: Program): List[List[Rule]] = {
    // 1. Collect all predicates
    val allPredicates = program.predicates
    if (program.rules.isEmpty) return Nil

    // 2. Build dependency graph
    val edges = mutable.ArrayBuffer[DependencyEdge]()
    val rulesByHead = mutable.Map[String, mutable.ListBuffer[Rule]]()

    for (rule <- program.rules) {
      val headPred = rule.head.predicate
      rulesByHead.getOrElseUpdate(headPred, mutable.ListBuffer()) += rule

      for (lit <- rule.body) {
        lit match {
          case PositiveAtom(bodyPred, _) =>
            edges += DependencyEdge(headPred, bodyPred, EdgeKind.Positive)
          case NegatedAtom(bodyPred, _) =>
            edges += DependencyEdge(headPred, bodyPred, EdgeKind.Negative)
          case AggLiteral(_, _, _, inner) =>
            edges += DependencyEdge(headPred, inner.predicate, EdgeKind.Aggregation)
          case _ => ()
        }
      }
    }

    // 3. Tarjan SCC Algorithm
    val sccs = tarjanSCC(allPredicates.toSeq, edges.toSeq)

    // 4. Verify no negative or aggregation edge within any SCC
    for (scc <- sccs) {
      val sccSet = scc.toSet
      for (e <- edges if sccSet.contains(e.from) && sccSet.contains(e.to)) {
        if (e.kind == EdgeKind.Negative) {
          throw new StratificationException(
            s"Unstratifiable circular negation detected between '${e.from}' and '${e.to}' in SCC ${scc.mkString("{", ", ", "}")}"
          )
        }
        if (e.kind == EdgeKind.Aggregation) {
          throw new StratificationException(
            s"Unstratifiable circular aggregation detected between '${e.from}' and '${e.to}' in SCC ${scc.mkString("{", ", ", "}")}"
          )
        }
      }
    }

    // 5. Build condensation DAG between SCCs and topologically sort into strata
    val sccIndexMap = mutable.Map[String, Int]()
    for ((scc, idx) <- sccs.zipWithIndex) {
      for (pred <- scc) {
        sccIndexMap.put(pred, idx)
      }
    }

    // Strata assignment by longest path in condensation DAG
    val strataOrder = new Array[Int](sccs.length)
    var changed = true
    var iterations = 0
    val maxIter = sccs.length + 1

    while (changed && iterations < maxIter) {
      changed = false
      iterations += 1
      for (e <- edges) {
        val u = sccIndexMap.getOrElse(e.from, -1)
        val v = sccIndexMap.getOrElse(e.to, -1)
        if (u != -1 && v != -1 && u != v) {
          val requiredRank = if (e.kind == EdgeKind.Negative || e.kind == EdgeKind.Aggregation) {
            strataOrder(v) + 1
          } else {
            strataOrder(v)
          }
          if (strataOrder(u) < requiredRank) {
            strataOrder(u) = requiredRank
            changed = true
          }
        }
      }
    }

    if (iterations >= maxIter) {
      throw new StratificationException("Cycle detected during stratum rank assignment")
    }

    // Group rules by stratum rank
    val maxRank = if (strataOrder.isEmpty) 0 else strataOrder.max
    val strataRules = Array.fill(maxRank + 1)(mutable.ListBuffer[Rule]())

    for ((scc, sccIdx) <- sccs.zipWithIndex) {
      val rank = strataOrder(sccIdx)
      for (pred <- scc) {
        rulesByHead.get(pred).foreach { rList =>
          strataRules(rank) ++= rList
        }
      }
    }

    strataRules.map(_.toList).filter(_.nonEmpty).toList
  }

  private def tarjanSCC(nodes: Seq[String], edges: Seq[DependencyEdge]): List[List[String]] = {
    var index = 0
    val indices = mutable.Map[String, Int]()
    val lowlink = mutable.Map[String, Int]()
    val onStack = mutable.Set[String]()
    val stack = mutable.Stack[String]()
    val result = mutable.ListBuffer[List[String]]()

    val adj = mutable.Map[String, mutable.ListBuffer[String]]()
    for (n <- nodes) adj.put(n, mutable.ListBuffer())
    for (e <- edges) {
      adj.getOrElseUpdate(e.from, mutable.ListBuffer()) += e.to
    }

    def strongConnect(v: String): Unit = {
      indices(v) = index
      lowlink(v) = index
      index += 1
      stack.push(v)
      onStack += v

      for (w <- adj.getOrElse(v, Nil)) {
        if (!indices.contains(w)) {
          strongConnect(w)
          lowlink(v) = math.min(lowlink(v), lowlink(w))
        } else if (onStack.contains(w)) {
          lowlink(v) = math.min(lowlink(v), indices(w))
        }
      }

      if (lowlink(v) == indices(v)) {
        val scc = mutable.ListBuffer[String]()
        var w = ""
        while ({
          w = stack.pop()
          onStack -= w
          scc += w
          w != v
        }) ()
        result += scc.toList
      }
    }

    for (n <- nodes if !indices.contains(n)) {
      strongConnect(n)
    }

    result.toList
  }
}
