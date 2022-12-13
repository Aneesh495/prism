package prism.engine

import prism.core.data.{Batch, Delta, Tuple}
import prism.core.lattice.{Antichain, Timestamp}
import prism.core.semiring.Semiring
import scala.collection.mutable

/**
 * Concurrent and reactive scheduler for differential dataflow graphs.
 * Propagates differential batches along topology edges, coordinates epoch frontiers,
 * and iterates until fixpoint quiescence is reached.
 */
final class Reactor[R: Semiring](val graph: DataflowGraph[R]) {
  private val outputBuffers = mutable.Map[String, mutable.ArrayBuffer[Batch[Tuple, R]]]()

  // Initialize output buffers for registered sinks
  for (sinkId <- graph.outputs) {
    outputBuffers.put(sinkId, mutable.ArrayBuffer[Batch[Tuple, R]]())
  }

  /**
   * Injects an input batch into a designated source operator.
   */
  def insertInput(sourceOpId: String, batch: Batch[Tuple, R], inputPort: Int = 0): Unit = synchronized {
    graph.getOperator(sourceOpId) match {
      case Some(op) =>
        op.receive(inputPort, batch)
      case None =>
        throw new IllegalArgumentException(s"Source operator $sourceOpId not found in graph")
    }
  }

  /**
   * Executes a single pass over all operators with pending work, routing batches.
   * Returns true if any operator performed work and emitted batches.
   */
  def stepSinglePass(): Boolean = synchronized {
    var workDoneInPass = false
    for (op <- graph.allOperators) {
      if (op.hasPendingWork) {
        val emitted = op.step()
        if (emitted.nonEmpty) {
          workDoneInPass = true
          for ((outPort, batch) <- emitted if batch.nonEmpty) {
            // Record if this operator is marked as an output sink
            if (graph.outputs.contains(op.id)) {
              outputBuffers.getOrElseUpdate(op.id, mutable.ArrayBuffer()) += batch
            }

            // Route along outgoing edges
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
    workDoneInPass
  }

  /**
   * Advances the dataflow until all pending operator queues are empty (quiescence).
   * Returns the total number of evaluation micro-steps performed.
   */
  def stepUntilQuiescence(maxSteps: Int = 100000): Int = synchronized {
    var stepCount = 0
    while (stepSinglePass() && stepCount < maxSteps) {
      stepCount += 1
    }
    stepCount
  }

  /**
   * Broadcasts a frontier advancement to all operators in the graph.
   */
  def advanceFrontier(newFrontier: Antichain): Unit = synchronized {
    for (op <- graph.allOperators) {
      for (port <- 0 until op.numInputs) {
        op.advanceFrontier(port, newFrontier)
      }
    }
  }

  /**
   * Returns accumulated deltas for a sink operator.
   */
  def getOutputDeltas(sinkOpId: String): Seq[Delta[Tuple, R]] = synchronized {
    val batches = outputBuffers.getOrElse(sinkOpId, Seq.empty)
    batches.flatMap(_.entries).toSeq
  }

  /**
   * Evaluates the consolidated relational snapshot for a sink operator at coordinate targetTime.
   */
  def snapshotAt(sinkOpId: String, targetTime: Timestamp): Map[Tuple, R] = synchronized {
    val batches = outputBuffers.getOrElse(sinkOpId, Seq.empty)
    var consolidated = Batch.empty[Tuple, R]
    for (b <- batches) {
      consolidated = consolidated.merge(b)
    }
    consolidated.accumulateAt(targetTime)
  }

  /**
   * Clears accumulated output buffers.
   */
  def clearOutputs(): Unit = synchronized {
    outputBuffers.values.foreach(_.clear())
  }
}
