package prism.cli

import java.io.File
import scala.io.Source
import prism.datalog.compiler.Compiler
import prism.datalog.parser.Parser
import prism.repl.Repl

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty || args(0) == "repl") {
      new Repl().start()
    } else {
      args(0) match {
        case "run" =>
          if (args.length < 2) {
            println("Usage: prism run <file.dl> [query-predicate]")
            System.exit(1)
          }
          val file = new File(args(1))
          if (!file.exists()) {
            println(s"Error: File not found: ${args(1)}")
            System.exit(1)
          }
          val source = Source.fromFile(file).mkString
          val prog = Parser.parse(source)
          val compiled = Compiler.compile(prog)
          val t0 = System.nanoTime()
          val results = compiled.run()
          val elapsedMs = (System.nanoTime() - t0) / 1000000.0

          println(s"Execution completed in ${elapsedMs} ms.")
          val targetPred = if (args.length >= 3) Some(args(2)) else prog.queries.headOption

          targetPred match {
            case Some(pred) =>
              val tuples = compiled.query(pred)
              println(s"--- Relation '$pred' (${tuples.size} records) ---")
              if (tuples.nonEmpty) {
                val headers = (0 until tuples.head.arity).map(i => s"col_$i")
                val rows = tuples.toSeq.sortBy(_.toString).map(t => (0 until t.arity).map(i => t(i).toString))
                println(TableFormatter.format(headers, rows))
              }
            case None =>
              println(s"Materialized relations: ${results.keys.mkString(", ")}")
              for ((pred, set) <- results) {
                println(s"  $pred: ${set.size} records")
              }
          }

        case "bench" =>
          println("Running Prism Benchmark Suite...")
          prism.bench.BenchmarkRunner.runAll()

        case "examples" =>
          println("=================================================================")
          println("               RUNNING PRISM REAL-WORLD EXAMPLES                 ")
          println("=================================================================")

          println("\n[1/4] Graph Analytics: PageRank Power Iteration")
          val edges = List((1L, 2L), (2L, 3L), (3L, 1L), (4L, 1L))
          val ranks = prism.examples.GraphAnalytics.computePageRank(edges, numIterations = 20)
          for ((node, rank) <- ranks.toSeq.sortBy(-_._2)) {
            println(f"  * Node $node%d -> PageRank: $rank%.4f")
          }

          println("\n[2/4] Program Analysis: Andersen Flow-Insensitive Points-To Analysis")
          val allocs = List(("a", "HeapObj1"), ("c", "HeapObj2"))
          val assigns = List(("b", "a"), ("d", "c"), ("b", "d"))
          val pointsTo = prism.examples.PointerAnalysis.analyze(allocs, assigns)
          for ((variable, targets) <- pointsTo.toSeq.sortBy(_._1)) {
            println(s"  * Variable '$variable' points to: {${targets.mkString(", ")}}")
          }

          println("\n[3/4] Financial AML Audit: Provenance Polynomial Taint Tracking")
          val txs = List(
            prism.examples.Transaction("tx1", "illicit_account", "shell_a", 50000.0),
            prism.examples.Transaction("tx2", "shell_a", "shell_b", 48000.0),
            prism.examples.Transaction("tx3", "shell_b", "merchant_hub", 45000.0)
          )
          val taints = prism.examples.FinancialAudit.traceTaint(txs, Set("illicit_account"))
          for ((acc, poly) <- taints.toSeq.sortBy(_._1)) {
            println(s"  * Account '$acc' -> Lineage: $poly")
          }

          println("\n[4/4] Compiler Optimization: E-Graph Equality Saturation")
          import prism.egraph.Expr._
          val expr = add(mul(Var("x"), Const(2)), Const(0))
          val (optExpr, report) = prism.examples.CompilerOptimizations.optimize(expr)
          println(s"  * Original  : $expr")
          println(s"  * Optimized : $optExpr (converged in ${report.iterations} iterations)")
          println("=================================================================")

        case "sql" =>
          if (args.length < 2) {
            println("Usage: prism sql \"<SELECT query>\"")
            System.exit(1)
          }
          val sqlText = args(1)
          val schemas = Map(
            "users" -> List("id", "name"),
            "orders" -> List("orderId", "userId", "amount")
          )
          val tokens = new prism.sql.SqlLexer(sqlText).tokenize()
          val ast = new prism.sql.SqlParser(tokens).parseQuery()
          val rule = prism.sql.SqlToDatalog.translate("sql_result", ast, schemas)
          println(s"Transpiled Datalog Rule:\n  $rule\n")

        case "--help" | "-h" | "help" =>
          printUsage()

        case other =>
          println(s"Unknown command: $other")
          printUsage()
          System.exit(1)
      }
    }
  }

  private def printUsage(): Unit = {
    println(
      """Prism - Incremental Differential Computation Engine
        |Usage:
        |  prism repl              Launch interactive REPL (default)
        |  prism run <file> [pred] Compile and execute Datalog file
        |  prism bench             Run micro and macro benchmarks
        |  prism examples          Run domain showcase examples
        |  prism sql "<query>"     Transpile SQL query to differential Datalog
        |  prism --help            Show this usage information
      """.stripMargin
    )
  }
}
