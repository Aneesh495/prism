package prism.engine

import prism.core.data.{Batch, Delta, Tuple}
import prism.core.lattice.{Antichain, Timestamp}
import prism.core.semiring.Semiring
import scala.collection.mutable.ArrayBuffer

/**
 * Iterative Loop operator for recursive fixpoint computation.
 * Manages loop coordinates by advancing iteration dimensions:
 * Input port 0: Initial base relation arriving from outside the loop.
 * Input port 1: Recursive feedback arriving from within the loop body.
 * Output port 0: Stream recirculated into the recursive loop body (iteration r + 1).
 * Output port 1: Emitted exit stream projecting out of the loop after fixpoint convergence.
 */
final class IterateOp[R: Semiring](
  val id: String,
  val iterationDimension: Int = 1,
  val maxIterations: Int = 1000
) extends Operator[R] {

  val numInputs: Int = 2
  val numOutputs: Int = 2

  private val pendingBase = new ArrayBuffer[Batch[Tuple, R]]()
  private val pendingFeedback = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      if (inputPort == 0) pendingBase += batch
      else if (inputPort == 1) pendingFeedback += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = ()

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    val results = new ArrayBuffer[(Int, Batch[Tuple, R])]()

    // 1. Process initial base batches entering the loop (round 0)
    if (pendingBase.nonEmpty) {
      for (batch <- pendingBase) {
        // Feed into the loop body (output 0) and also emit to exit (output 1)
        results += ((0, batch))
        results += ((1, batch))
      }
      pendingBase.clear()
    }

    // 2. Process recursive feedback batches circulating in the loop
    if (pendingFeedback.nonEmpty) {
      for (batch <- pendingFeedback) {
        val advancedDeltas = new ArrayBuffer[Delta[Tuple, R]](batch.length)
        var i = 0
        while (i < batch.length) {
          val d = batch(i)
          val currentRound = d.timestamp(iterationDimension)
          if (currentRound < maxIterations) {
            val nextTs = d.timestamp.advance(iterationDimension, 1L)
            advancedDeltas += Delta(d.data, nextTs, d.weight)
          }
          i += 1
        }

        if (advancedDeltas.nonEmpty) {
          val advancedBatch = Batch.fromUnsorted(advancedDeltas.toArray)
          // Recirculate to loop body
          results += ((0, advancedBatch))
          // Also record in exit stream
          results += ((1, advancedBatch))
        }
      }
      pendingFeedback.clear()
    }

    results.toSeq
  }

  def hasPendingWork: Boolean = synchronized {
    pendingBase.nonEmpty || pendingFeedback.nonEmpty
  }
}
