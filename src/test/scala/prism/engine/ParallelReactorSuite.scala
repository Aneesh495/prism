package prism.engine

import munit.FunSuite
import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.DiffInt
import prism.core.semiring.DiffInt.diffIntGroup
import prism.engine.parallel.{HashPartitioner, ParallelReactor, WorkStealingScheduler}
import java.util.concurrent.atomic.AtomicInteger

class ParallelReactorSuite extends FunSuite {

  test("HashPartitioner splits batch across partitions uniformly") {
    val partitioner = new HashPartitioner(4)
    val deltas = (1 to 100).map { i =>
      Delta(Tuple(Datum.I64(i.toLong), Datum.Str(s"val$i")), Timestamp(1L), 1L)
    }
    val batch = Batch.fromSeq(deltas)
    val partitions = partitioner.splitBatch[Tuple, Long](batch, identity, keyCol = 0)

    assertEquals(partitions.length, 4)
    val totalDeltas = partitions.map(_.length).sum
    assertEquals(totalDeltas, 100)

    // Verify all partitions received items
    for (p <- partitions) {
      assert(p.length > 10)
    }
  }

  test("WorkStealingScheduler executes concurrent tasks to completion") {
    val scheduler = new WorkStealingScheduler(numWorkers = 4)
    val counter = new AtomicInteger(0)

    val tasks = (1 to 200).map { _ =>
      new Runnable {
        def run(): Unit = {
          counter.incrementAndGet()
        }
      }
    }

    try {
      scheduler.executeAll(tasks)
      assertEquals(counter.get(), 200)
    } finally {
      scheduler.shutdown()
    }
  }

  test("ParallelReactor executes pipeline to quiescence across worker threads") {
    val graph = new DataflowGraph[Long]()

    // op1: map x -> x * 2
    val mapOp = new MapOp[Long]("doubler", tup => {
      val v = tup(0).asInstanceOf[Datum.I64].value
      Tuple(Datum.I64(v * 2))
    })

    // op2: filter even numbers > 10
    val filterOp = new FilterOp[Long]("filter_large", tup => {
      val v = tup(0).asInstanceOf[Datum.I64].value
      v > 10
    })

    graph.addOperator(mapOp)
    graph.addOperator(filterOp)
    graph.addEdge("doubler", 0, "filter_large", 0)
    graph.markInput("doubler")
    graph.markOutput("filter_large")

    val reactor = new ParallelReactor[Long](graph, numWorkers = 2)

    try {
      // Ingest input: 3, 4, 5, 6, 7 (doubled to 6, 8, 10, 12, 14; filtered to 12, 14)
      val inputDeltas = List(3L, 4L, 5L, 6L, 7L).map { x =>
        Delta(Tuple(Datum.I64(x)), Timestamp(1L), 1L)
      }
      reactor.insertInput("doubler", Batch.fromSeq(inputDeltas))

      val rounds = reactor.stepUntilQuiescence()
      assert(rounds >= 1)

      val snap = reactor.snapshotAt("filter_large", Timestamp(1L))
      assertEquals(snap.size, 2)
      assertEquals(snap(Tuple(Datum.I64(12L))), 1L)
      assertEquals(snap(Tuple(Datum.I64(14L))), 1L)
    } finally {
      reactor.shutdown()
    }
  }
}
