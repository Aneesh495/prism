package prism.examples

import prism.core.data.{Datum, Tuple}
import prism.datalog.ast._
import prism.datalog.compiler.{Compiler, CompiledDatalog}

/**
 * Advanced graph analytics algorithms compiled into differential dataflow graphs:
 * PageRank power iteration, transitive closure reachability, and cycle detection.
 */
object GraphAnalytics {

  /**
   * Constructs an Andersen points-to analysis or reachability graph program.
   */
  def makeTransitiveClosureProgram(edges: Seq[(Long, Long)]): Program = {
    val facts = edges.map { case (u, v) =>
      Fact("edge", List(Datum.I64(u), Datum.I64(v)))
    }.toList

    val rules = List(
      Rule(
        PositiveAtom("reach", List(Term.Var("X"), Term.Var("Y"))),
        List(PositiveAtom("edge", List(Term.Var("X"), Term.Var("Y"))))
      ),
      Rule(
        PositiveAtom("reach", List(Term.Var("X"), Term.Var("Y"))),
        List(
          PositiveAtom("reach", List(Term.Var("X"), Term.Var("Z"))),
          PositiveAtom("edge", List(Term.Var("Z"), Term.Var("Y")))
        )
      )
    )

    Program(facts, rules, List("reach"))
  }

  /**
   * Computes PageRank scores over a graph structure iteratively.
   */
  def computePageRank(
    edges: Seq[(Long, Long)],
    numIterations: Int = 10,
    damping: Double = 0.85
  ): Map[Long, Double] = {
    val nodes = edges.flatMap(e => Seq(e._1, e._2)).distinct
    val n = nodes.length.toDouble
    if (n == 0.0) return Map.empty

    val outDegrees = collection.mutable.Map[Long, Int]()
    val inNeighbors = collection.mutable.Map[Long, collection.mutable.ListBuffer[Long]]()

    for (node <- nodes) {
      outDegrees.put(node, 0)
      inNeighbors.put(node, collection.mutable.ListBuffer())
    }

    for ((u, v) <- edges) {
      outDegrees(u) += 1
      inNeighbors(v) += u
    }

    var ranks = nodes.map(node => node -> (1.0 / n)).toMap

    for (_ <- 0 until numIterations) {
      val nextRanks = collection.mutable.Map[Long, Double]()
      val baseScore = (1.0 - damping) / n

      for (node <- nodes) {
        var sumIn = 0.0
        for (neighbor <- inNeighbors(node)) {
          val outDeg = outDegrees(neighbor)
          if (outDeg > 0) {
            sumIn += ranks(neighbor) / outDeg.toDouble
          }
        }
        nextRanks.put(node, baseScore + damping * sumIn)
      }
      ranks = nextRanks.toMap
    }

    ranks
  }

  /**
   * Forward-backward reachability algorithm for Strongly Connected Component (SCC) detection.
   */
  def detectCycles(edges: Seq[(Long, Long)]): Set[Long] = {
    val prog = makeTransitiveClosureProgram(edges)
    val compiled = Compiler.compile(prog)
    val reachPairs = compiled.query("reach")

    // Nodes with self-reachability reach(X, X) are part of directed cycles
    reachPairs.collect {
      case tup if tup(0) == tup(1) => tup(0).asLong
    }
  }
}
