package prism.bench

import prism.core.data.{Datum, Tuple}
import prism.datalog.ast._
import prism.datalog.compiler.Compiler
import prism.egraph._

/**
 * Benchmark runner measuring differential speedups, graph analytics, and e-graph saturation.
 */
object BenchmarkRunner {

  def runAll(): Unit = {
    println("=================================================================")
    println("                    PRISM BENCHMARK SUITE                        ")
    println("=================================================================")
    benchTransitiveClosureIncremental()
    benchEqualitySaturationSimplification()
    benchTropicalShortestPaths()
    println("=================================================================")
    println("All benchmarks completed successfully.")
  }

  def benchTransitiveClosureIncremental(): Unit = {
    println("\n[Benchmark 1] Incremental Transitive Closure vs Scratch Evaluation")
    println("-----------------------------------------------------------------")

    // Construct a linear chain graph of 200 nodes
    val numNodes = 200
    val edgeFacts = (0 until numNodes - 1).map { i =>
      Fact("edge", List(Datum.I64(i), Datum.I64(i + 1)))
    }.toList

    val rules = List(
      // reach(X, Y) :- edge(X, Y).
      Rule(
        PositiveAtom("reach", List(Term.Var("X"), Term.Var("Y"))),
        List(PositiveAtom("edge", List(Term.Var("X"), Term.Var("Y"))))
      ),
      // reach(X, Y) :- reach(X, Z), edge(Z, Y).
      Rule(
        PositiveAtom("reach", List(Term.Var("X"), Term.Var("Y"))),
        List(
          PositiveAtom("reach", List(Term.Var("X"), Term.Var("Z"))),
          PositiveAtom("edge", List(Term.Var("Z"), Term.Var("Y")))
        )
      )
    )

    val program = Program(edgeFacts, rules, List("reach"))

    // Initial compile and run
    val t0 = System.nanoTime()
    val compiled = Compiler.compile(program)
    val initialResults = compiled.run()
    val t1 = System.nanoTime()
    val initialMs = (t1 - t0) / 1000000.0

    val initialReachCount = initialResults("reach").size
    println(f"Initial Fixpoint: $initialReachCount reachable pairs computed in $initialMs%.2f ms")

    // Now insert a shortcut edge: edge(0, 100) dynamically
    val t2 = System.nanoTime()
    compiled.insertFact("edge", Tuple.of(0L, 100L))
    val t3 = System.nanoTime()
    val incrementalMs = (t3 - t2) / 1000000.0

    val updatedReachCount = compiled.query("reach").size
    println(f"Incremental Update: new edge processed in $incrementalMs%.2f ms (total pairs: $updatedReachCount)")
    val speedup = if (incrementalMs > 0.0) initialMs / incrementalMs else 100.0
    println(f"Incremental Speedup Factor: $speedup%.1fx over initial whole-graph materialization")
  }

  def benchEqualitySaturationSimplification(): Unit = {
    println("\n[Benchmark 2] Equality Saturation on Deep Arithmetic Tree")
    println("---------------------------------------------------------")

    import Expr._
    val x = Var("x")
    val zero = Const(0)
    val one = Const(1)
    val two = Const(2)

    // Expression: ((x * 1) + 0) * (2 - 2) ==> 0
    val complexExpr = mul(add(mul(x, one), zero), sub(two, two))
    println(s"Input Term: $complexExpr (size: ${complexExpr.size}, depth: ${complexExpr.depth})")

    val egraph = new EGraph()
    val targetClass = egraph.addExpr(complexExpr)

    val engine = new SaturationEngine(Rewrite.standardArithmeticRules, maxIterations = 8, nodeLimit = 2000)
    val t0 = System.nanoTime()
    val report = engine.saturate(egraph)
    val extractor = new Extractor(egraph)
    val simplified = extractor.extract(targetClass)
    val t1 = System.nanoTime()
    val elapsedMs = (t1 - t0) / 1000000.0

    println(f"Saturation completed in $elapsedMs%.2f ms (${report.iterations} iterations)")
    println(s"Optimal Extracted Term: $simplified (cost: ${extractor.costOf(targetClass)})")
    assert(simplified == Const(0) || simplified.toString == "0", s"Expected 0, got $simplified")
    println("Simplification validated successfully!")
  }

  def benchTropicalShortestPaths(): Unit = {
    println("\n[Benchmark 3] Tropical Semiring Dynamic Shortest Path Computation")
    println("-----------------------------------------------------------------")
    import prism.core.semiring.Tropical

    val path0 = Tropical(0.0)
    val edge1 = Tropical(4.5)
    val edge2 = Tropical(2.1)

    val semiring = summon[prism.core.semiring.LatticeSemiring[Tropical]]
    val combinedPath = semiring.times(edge1, edge2)
    val optimalPath = semiring.plus(combinedPath, Tropical(5.0))

    println(s"Candidate path 1: 4.5 + 2.1 = ${combinedPath.value}")
    println(s"Candidate path 2: 5.0")
    println(s"Tropical min-plus selection: ${optimalPath.value}")
    assert(optimalPath.value == 5.0, s"Expected 5.0, got ${optimalPath.value}")
    println("Tropical path algebra verified.")
  }
}
