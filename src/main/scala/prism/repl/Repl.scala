package prism.repl

import java.io.File
import scala.io.Source
import prism.cli.{TableFormatter, Visualizer}
import prism.core.data.Tuple
import prism.datalog.analysis.Stratifier
import prism.datalog.ast.{Fact, Program, Rule}
import prism.datalog.compiler.{Compiler, CompiledDatalog}
import prism.datalog.parser.Parser
import prism.egraph._

/**
 * Interactive REPL for Prism differential engine and equality saturation lab.
 */
final class Repl {
  private var currentProgram = Program()
  private var compiled: Option[CompiledDatalog] = None

  def start(): Unit = {
    printBanner()
    var running = true

    while (running) {
      print("prism> ")
      val line = scala.io.StdIn.readLine()
      if (line == null || line.trim == ":quit" || line.trim == ":exit") {
        running = false
        println("Exiting Prism.")
      } else {
        val trimmed = line.trim
        if (trimmed.nonEmpty) {
          try {
            handleInput(trimmed)
          } catch {
            case ex: Throwable =>
              println(s"Error: ${ex.getMessage}")
          }
        }
      }
    }
  }

  private def handleInput(input: String): Unit = {
    if (input.startsWith(":")) {
      handleCommand(input)
    } else {
      // Direct Datalog input (fact, rule, or query)
      if (input.startsWith("?")) {
        // Query syntax: ? predicate
        val pred = input.stripPrefix("?").trim.stripSuffix(".").stripSuffix("()")
        executeQuery(pred)
      } else {
        // Parse as Datalog clause and recompile
        val parsed = Parser.parse(input)
        currentProgram = Program(
          facts = currentProgram.facts ++ parsed.facts,
          rules = currentProgram.rules ++ parsed.rules,
          queries = currentProgram.queries ++ parsed.queries
        )
        println(s"Added ${parsed.facts.length} fact(s), ${parsed.rules.length} rule(s). Recompiling...")
        compileCurrent()
      }
    }
  }

  private def handleCommand(cmd: String): Unit = {
    val parts = cmd.split("\\s+", 2)
    val name = parts(0)
    val arg = if (parts.length > 1) parts(1).trim else ""

    name match {
      case ":help" =>
        printHelp()

      case ":load" =>
        if (arg.isEmpty) println("Usage: :load <path-to-datalog-file>")
        else {
          val file = new File(arg)
          if (!file.exists()) println(s"File not found: $arg")
          else {
            val content = Source.fromFile(file).mkString
            val p = Parser.parse(content)
            currentProgram = p
            println(s"Loaded ${p.facts.length} fact(s), ${p.rules.length} rule(s) from $arg.")
            compileCurrent()
          }
        }

      case ":query" =>
        if (arg.isEmpty) println("Usage: :query <predicate-name>")
        else executeQuery(arg)

      case ":rules" =>
        if (currentProgram.rules.isEmpty) println("(no rules loaded)")
        else {
          currentProgram.rules.zipWithIndex.foreach { case (r, i) =>
            println(s"[$i] $r")
          }
        }

      case ":facts" =>
        if (currentProgram.facts.isEmpty) println("(no facts loaded)")
        else {
          currentProgram.facts.take(30).foreach(println)
          if (currentProgram.facts.length > 30) {
            println(s"... and ${currentProgram.facts.length - 30} more")
          }
        }

      case ":strata" =>
        val strata = Stratifier.stratify(currentProgram)
        println(s"Program partitioned into ${strata.length} stratum/strata:")
        strata.zipWithIndex.foreach { case (rules, idx) =>
          val preds = rules.map(_.head.predicate).distinct.mkString(", ")
          println(s"Stratum $idx: predicates {$preds} (${rules.length} rules)")
        }

      case ":mermaid" =>
        compiled match {
          case Some(c) =>
            println(Visualizer.toMermaidDataflow(c.graph))
          case None =>
            println("No compiled program active.")
        }

      case ":egraph" =>
        if (arg.isEmpty) {
          demoEGraph()
        } else {
          println(s"Running equality saturation for: $arg")
          demoEGraph()
        }

      case ":clear" =>
        currentProgram = Program()
        compiled = None
        println("Cleared active program.")

      case other =>
        println(s"Unknown command: $other. Type :help for commands.")
    }
  }

  private def compileCurrent(): Unit = {
    val t0 = System.currentTimeMillis()
    compiled = Some(Compiler.compile(currentProgram))
    val c = compiled.get
    val res = c.run()
    val t1 = System.currentTimeMillis()
    println(s"Program evaluated in ${t1 - t0} ms. Materialized ${res.size} relations.")
  }

  private def executeQuery(pred: String): Unit = {
    compiled match {
      case Some(c) =>
        val tuples = c.query(pred)
        if (tuples.isEmpty) {
          println(s"Relation '$pred' is empty.")
        } else {
          val arity = tuples.head.arity
          val headers = (0 until arity).map(i => s"col_$i")
          val rows = tuples.toSeq.sortBy(_.toString).map { tup =>
            (0 until arity).map(i => tup(i).toString)
          }
          println(TableFormatter.format(headers, rows))
        }
      case None =>
        println("No compiled program. Add facts/rules or :load a file first.")
    }
  }

  private def demoEGraph(): Unit = {
    println("--- E-Graph Equality Saturation Demo ---")
    // (x * 2) / 2 ==> x
    import Expr._
    val x = Var("x")
    val two = Const(2)
    val expr = div(mul(x, two), two)
    println(s"Initial Expression: $expr")

    val egraph = new EGraph()
    val classId = egraph.addExpr(expr)

    val rules = Rewrite.standardArithmeticRules ++ List(
      Rewrite.rule("div_mul_cancel", Pattern.op("/", Pattern.op("*", Pattern.v("a"), Pattern.v("b")), Pattern.v("b")), Pattern.v("a"))
    )

    val engine = new SaturationEngine(rules)
    val report = engine.saturate(egraph)
    println(s"Saturation completed in ${report.durationMs} ms (${report.iterations} iterations, ${report.finalClasses} classes, ${report.finalNodes} nodes).")

    val extractor = new Extractor(egraph)
    val simplified = extractor.extract(classId)
    println(s"Extracted Optimal Expression: $simplified")

    val proof = ProofExplainer.explain(egraph, expr, simplified, report)
    println("\n" + proof.formatMarkdown)
  }

  private def printBanner(): Unit = {
    println(
      """
        |  ██████╗ ██████╗  ██╗███████╗███╗   ███╗
        |  ██╔══██╗██╔══██╗ ██║██╔════╝████╗ ████║
        |  ██████╔╝██████╔╝ ██║███████╗██╔████╔██║
        |  ██╔═══╝ ██╔══██╗ ██║╚════██║██║╚██╔╝██║
        |  ██║     ██║  ██║ ██║███████║██║ ╚═╝ ██║
        |  ╚═╝     ╚═╝  ╚═╝ ╚═╝╚══════╝╚═╝     ╚═╝
        |  Incremental Differential Computation & Equality Saturation
        |  Version 0.1.0 | Type :help for commands
        |""".stripMargin
    )
  }

  private def printHelp(): Unit = {
    println(
      """Commands:
        |  :load <file>      Load and compile a Datalog source file
        |  :query <pred>     Query materialized relation and display as table
        |  ? <pred>          Shorthand query command
        |  :rules            List loaded rules
        |  :facts            List loaded base facts
        |  :strata           Display stratification breakdown
        |  :mermaid          Print dataflow graph in Mermaid syntax
        |  :egraph           Run E-Graph equality saturation demonstration
        |  :clear            Reset active program
        |  :help             Show this reference
        |  :exit, :quit      Exit REPL
      """.stripMargin
    )
  }
}
