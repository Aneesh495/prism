package prism.engine

import prism.core.data.{Batch, Delta, DiffTrace, Tuple}
import prism.core.lattice.{Antichain, Timestamp}
import prism.core.semiring.Semiring
import scala.collection.mutable.ArrayBuffer

/**
 * Bilinear differential join operator: Delta(A x_K B).
 * Implements symmetric incremental join with zero double-counting:
 * Delta(A join B) = (Delta A join B_prior) union (A_current join Delta B).
 */
final class JoinOp[R: Semiring](
  val id: String,
  val keyIndicesA: Array[Int],
  val keyIndicesB: Array[Int],
  val combine: (Tuple, Tuple) => Tuple
) extends Operator[R] {

  val numInputs: Int = 2
  val numOutputs: Int = 1

  private val traceA = DiffTrace.empty[Tuple, Tuple, R]
  private val traceB = DiffTrace.empty[Tuple, Tuple, R]

  private val pendingA = new ArrayBuffer[Batch[Tuple, R]]()
  private val pendingB = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      if (inputPort == 0) pendingA += batch
      else if (inputPort == 1) pendingB += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = synchronized {
    if (newFrontier.nonEmpty) {
      if (inputPort == 0) traceA.compact(newFrontier)
      else if (inputPort == 1) traceB.compact(newFrontier)
    }
  }

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    if (pendingA.isEmpty && pendingB.isEmpty) Nil
    else {
      val sr = summon[Semiring[R]]
      val emitted = new ArrayBuffer[Delta[Tuple, R]]()

      // 1. Process pending A against traceB (B_prior)
      for (batchA <- pendingA) {
        val indexedA = new ArrayBuffer[Delta[(Tuple, Tuple), R]](batchA.length)
        var i = 0
        while (i < batchA.length) {
          val deltaA = batchA(i)
          val key = deltaA.data.project(keyIndicesA)
          indexedA += Delta((key, deltaA.data), deltaA.timestamp, deltaA.weight)

          // Probe traceB
          val matchesB = traceB.queryKey(key)
          var j = 0
          while (j < matchesB.length) {
            val deltaB = matchesB(j)
            val joinedTs = deltaA.timestamp.join(deltaB.timestamp)
            val joinedWeight = sr.times(deltaA.weight, deltaB.weight)
            if (!sr.isZero(joinedWeight)) {
              val combinedTuple = combine(deltaA.data, deltaB.data)
              emitted += Delta(combinedTuple, joinedTs, joinedWeight)
            }
            j += 1
          }
          i += 1
        }
        // Insert A into traceA
        traceA.insert(Batch.fromUnsorted(indexedA.toArray))
      }
      pendingA.clear()

      // 2. Process pending B against traceA (A_current, which now includes newly arrived A)
      for (batchB <- pendingB) {
        val indexedB = new ArrayBuffer[Delta[(Tuple, Tuple), R]](batchB.length)
        var i = 0
        while (i < batchB.length) {
          val deltaB = batchB(i)
          val key = deltaB.data.project(keyIndicesB)
          indexedB += Delta((key, deltaB.data), deltaB.timestamp, deltaB.weight)

          // Probe traceA
          val matchesA = traceA.queryKey(key)
          var j = 0
          while (j < matchesA.length) {
            val deltaA = matchesA(j)
            val joinedTs = deltaA.timestamp.join(deltaB.timestamp)
            val joinedWeight = sr.times(deltaA.weight, deltaB.weight)
            if (!sr.isZero(joinedWeight)) {
              val combinedTuple = combine(deltaA.data, deltaB.data)
              emitted += Delta(combinedTuple, joinedTs, joinedWeight)
            }
            j += 1
          }
          i += 1
        }
        // Insert B into traceB
        traceB.insert(Batch.fromUnsorted(indexedB.toArray))
      }
      pendingB.clear()

      if (emitted.isEmpty) Nil
      else {
        val outBatch = Batch.fromUnsorted(emitted.toArray)
        Seq((0, outBatch))
      }
    }
  }

  def hasPendingWork: Boolean = synchronized {
    pendingA.nonEmpty || pendingB.nonEmpty
  }
}

object JoinOp {
  /**
   * Standard natural join combining key columns followed by non-key columns of B.
   */
  def naturalJoinCombine(keyIndicesB: Array[Int]): (Tuple, Tuple) => Tuple = {
    val keySetB = keyIndicesB.toSet
    (tupA: Tuple, tupB: Tuple) => {
      val bNonKey = (0 until tupB.arity).filterNot(keySetB.contains).toArray
      tupA.concat(tupB.project(bNonKey))
    }
  }
}
