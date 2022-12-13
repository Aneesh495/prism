package prism.cli

import munit.FunSuite
import prism.cli.tui.{ProfileReport, TerminalDebugger}
import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.DiffInt
import prism.core.semiring.DiffInt.diffIntGroup
import prism.engine.{DataflowGraph, FilterOp, MapOp, Reactor}

class TuiSuite extends FunSuite {

  test("ProfileReport captures graph profile metrics") {
    val graph = new DataflowGraph[Long]()
    val filterOp = new FilterOp[Long]("filter_positive", tup => tup(0) match {
      case Datum.I64(v) => v > 0
      case _ => false
    })
    graph.addOperator(filterOp)
    graph.markInput("filter_positive")

    val reactor = new Reactor[Long](graph)
    val profile = ProfileReport.capture(reactor)

    assertEquals(profile.totalOperators, 1)
    assertEquals(profile.totalSources, 1)
    assert(profile.quiescent)

    val summary = profile.summaryString
    assert(summary.contains("PRISM ENGINE PROFILE REPORT"))
    assert(summary.contains("filter_positive"))
  }

  test("TerminalDebugger runs dataflow to quiescence") {
    val graph = new DataflowGraph[Long]()
    val mapOp = new MapOp[Long]("increment", tup => {
      Tuple(Datum.I64(tup(0).asInstanceOf[Datum.I64].value + 1L))
    })
    graph.addOperator(mapOp)
    graph.markInput("increment")
    graph.markOutput("increment")

    val reactor = new Reactor[Long](graph)
    val debugger = new TerminalDebugger(reactor)

    val delta = Delta(Tuple(Datum.I64(41L)), Timestamp(1L), 1L)
    reactor.insertInput("increment", Batch.fromSeq(List(delta)))

    val steps = debugger.runToQuiescence(maxSteps = 10)
    assert(steps >= 1)
    val snap = reactor.snapshotAt("increment", Timestamp(1L))
    assertEquals(snap(Tuple(Datum.I64(42L))), 1L)
  }
}
