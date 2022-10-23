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
        |  prism --help            Show this usage information
      """.stripMargin
    )
  }
}
