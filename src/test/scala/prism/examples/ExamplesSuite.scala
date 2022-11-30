package prism.examples

import munit.FunSuite
import prism.egraph.Expr

class ExamplesSuite extends FunSuite {

  test("GraphAnalytics computes PageRank distribution") {
    val edges = List(
      (1L, 2L),
      (2L, 3L),
      (3L, 1L),
      (4L, 1L)
    )

    val ranks = GraphAnalytics.computePageRank(edges, numIterations = 20)
    assertEquals(ranks.size, 4)
    // Node 1 receives links from 3 and 4, should have highest PageRank
    val topNode = ranks.maxBy(_._2)._1
    assertEquals(topNode, 1L)
  }

  test("PointerAnalysis computes Andersen points-to relations in Datalog") {
    // a = new O1; b = a;
    val allocs = List(("a", "O1"))
    val assigns = List(("b", "a"))

    val pointsTo = PointerAnalysis.analyze(allocs, assigns)
    assertEquals(pointsTo("a"), Set("O1"))
    assertEquals(pointsTo("b"), Set("O1"))
  }

  test("FinancialAudit traces dirty money taint across shell transactions") {
    val txs = List(
      Transaction("tx1", "illicit_origin", "shell_co_a", 50000.0),
      Transaction("tx2", "shell_co_a", "shell_co_b", 48000.0),
      Transaction("tx3", "shell_co_b", "cleared_account", 45000.0)
    )

    val taints = FinancialAudit.traceTaint(txs, Set("illicit_origin"))
    assert(taints.contains("cleared_account"))

    val finalTaint = taints("cleared_account")
    // Provenance polynomial should track all intermediate hops: source_illicit_origin * tx1 * tx2 * tx3
    assertEquals(finalTaint.variables, Set("source_illicit_origin", "tx1", "tx2", "tx3"))

    val sensitivity = FinancialAudit.sourceSensitivity(finalTaint, "illicit_origin")
    assert(!sensitivity.isZero)
  }

  test("CompilerOptimizations applies strength reduction via equality saturation") {
    import Expr._
    // (x * 2) + 0 ==> x << 1
    val expr = add(mul(Var("x"), Const(2)), Const(0))
    val (optimized, report) = CompilerOptimizations.optimize(expr)

    // Should be simplified to (shl x 1)
    val optStr = optimized.toString
    assert(optStr.contains("shl") || optStr == "(x shl 1)" || optStr == "shl(x, 1)")
  }
}
