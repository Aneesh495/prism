package prism.engine.parallel

import prism.core.data.{Batch, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.Semiring
import prism.engine.{DataflowGraph, Operator}
import scala.collection.mutable

/**
 * Concurrent multi-worker differential dataflow reactor.
 * Dispatches active operator evaluation tasks to worker threads via work stealing,
 * synchronizing output routing and frontier tracking across epochs.
 */
final class ParallelReactor[R: Semiring](
  val graph: DataflowGraph[R],
  val numWorkers: Int = 4
) {
  private val scheduler = new WorkStealingScheduler(numWorkers)
  private val outputBuffers = mutable.Map[String, mutable.ArrayBuffer[Batch[Tuple, R]]]()

  for (sinkId <- graph.outputs) {
    outputBuffers.put(sinkId, mutable.ArrayBuffer[Batch[Tuple, R]]())
  }

  def insertInput(sourceOpId: String, batch: Batch[Tuple, R], inputPort: Int = 0): Unit = synchronized {
    graph.getOperator(sourceOpId) match {
      case Some(op) =>
        op.receive(inputPort, batch)
      case None =>
        throw new IllegalArgumentException(s"Source operator $sourceOpId not found in graph")
    }
  }

  /**
   * Advances the dataflow across parallel workers until all pending work is drained.
   */
  def stepUntilQuiescence(maxRounds: Int = 1000): Int = {
    var round = 0
    var active = true

    while (active && round < maxRounds) {
      // Find all operators with pending work
      val activeOps = graph.allOperators.filter(_.hasPendingWork)
      if (activeOps.isEmpty) {
        active = false
      } else {
        round += 1
        val emittedBatches = Array.fill(activeOps.length)(Seq.empty[(Int, Batch[Tuple, R])])

        // Build runnable tasks for active operators
        val tasks = activeOps.zipWithIndex.map { case (op, idx) =>
          new Runnable {
            def run(): Unit = {
              val emitted = op.step()
              emittedBatches(idx) = emitted
            }
          }
        }

        // Execute operator steps concurrently on work-stealing pool
        scheduler.executeAll(tasks)

        // Sequentially route emitted deltas along graph edges
        synchronized {
          for ((op, idx) <- activeOps.zipWithIndex) {
            val emitted = emittedBatches(idx)
            for ((outPort, batch) <- emitted if batch.nonEmpty) {
              if (graph.outputs.contains(op.id)) {
                outputBuffers.getOrElseUpdate(op.id, mutable.ArrayBuffer()) += batch
              }
              val edges = graph.outgoingEdges(op.id, outPort)
              for (edge <- edges) {
                graph.getOperator(edge.toOperatorId).foreach { targetOp =>
                  targetOp.receive(edge.toPort, batch)
                }
              }
            }
          }
        }
      }
    }

    round
  }

  /**
   * Reads consolidated output snapshot for a designated sink.
   */
  def snapshotAt(sinkId: String, ts: Timestamp): Map[Tuple, R] = synchronized {
    val batches = outputBuffers.getOrElse(sinkId, Seq.empty)
    if (batches.isEmpty) Map.empty
    else {
      var merged = batches.head
      for (b <- batches.tail) {
        merged = merged.merge(b)
      }
      merged.accumulateAt(ts)
    }
  }

  /**
   * Terminates background worker pool threads.
   */
  def shutdown(): Unit = {
    scheduler.shutdown()
  }
}
