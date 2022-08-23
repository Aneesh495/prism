package prism.engine

import prism.core.data.{Batch, Delta, DiffTrace, Tuple}
import prism.core.lattice.{Antichain, Timestamp}
import prism.core.semiring.{AbelianGroup, Semiring}
import scala.collection.mutable.ArrayBuffer

/**
 * Differential Antijoin operator: Delta(A antijoin_K B).
 * Used for stratified negation: produces tuples from A whose key does NOT exist in B.
 * When B transitions between presence (> 0) and absence (== 0), retractions and
 * assertions are emitted downstream.
 */
final class AntijoinOp[R](
  val id: String,
  val keyIndicesA: Array[Int],
  val keyIndicesB: Array[Int]
)(using val group: AbelianGroup[R]) extends Operator[R] {

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
      val emitted = new ArrayBuffer[Delta[Tuple, R]]()

      // Process pending deltas from A
      for (batchA <- pendingA) {
        val indexedA = new ArrayBuffer[Delta[(Tuple, Tuple), R]](batchA.length)
        var i = 0
        while (i < batchA.length) {
          val deltaA = batchA(i)
          val key = deltaA.data.project(keyIndicesA)
          indexedA += Delta((key, deltaA.data), deltaA.timestamp, deltaA.weight)

          // Check if key is present in B at deltaA.timestamp
          val currentBMap = traceB.accumulateKeyAt(key, deltaA.timestamp)
          val isPresentInB = currentBMap.values.exists(!group.isZero(_))

          if (!isPresentInB) {
            emitted += Delta(deltaA.data, deltaA.timestamp, deltaA.weight)
          }
          i += 1
        }
        traceA.insert(Batch.fromUnsorted(indexedA.toArray))
      }
      pendingA.clear()

      // Process pending deltas from B
      for (batchB <- pendingB) {
        val indexedB = new ArrayBuffer[Delta[(Tuple, Tuple), R]](batchB.length)
        var i = 0
        while (i < batchB.length) {
          val deltaB = batchB(i)
          val key = deltaB.data.project(keyIndicesB)
          indexedB += Delta((key, deltaB.data), deltaB.timestamp, deltaB.weight)

          // Query matching active records in A
          val activeAMap = traceA.accumulateKeyAt(key, deltaB.timestamp)

          // Check state of B before and after this delta
          val bBefore = traceB.accumulateKeyAt(key, deltaB.timestamp).values.exists(!group.isZero(_))
          // Simulate B after delta
          val bCurrent = traceB.accumulateKeyAt(key, deltaB.timestamp)
          val priorWeight = bCurrent.getOrElse(deltaB.data, group.zero)
          val newWeight = group.plus(priorWeight, deltaB.weight)
          val updatedBMap = if (group.isZero(newWeight)) bCurrent - deltaB.data else bCurrent.updated(deltaB.data, newWeight)
          val bAfter = updatedBMap.values.exists(!group.isZero(_))

          if (!bBefore && bAfter) {
            // Key transitioned 0 -> positive: retract all active tuples in A
            for ((tupA, weightA) <- activeAMap if !group.isZero(weightA)) {
              emitted += Delta(tupA, deltaB.timestamp, group.negate(weightA))
            }
          } else if (bBefore && !bAfter) {
            // Key transitioned positive -> 0: restore all active tuples in A
            for ((tupA, weightA) <- activeAMap if !group.isZero(weightA)) {
              emitted += Delta(tupA, deltaB.timestamp, weightA)
            }
          }
          i += 1
        }
        traceB.insert(Batch.fromUnsorted(indexedB.toArray))
      }
      pendingB.clear()

      if (emitted.isEmpty) Nil
      else {
        Seq((0, Batch.fromUnsorted(emitted.toArray)))
      }
    }
  }

  def hasPendingWork: Boolean = synchronized {
    pendingA.nonEmpty || pendingB.nonEmpty
  }
}
