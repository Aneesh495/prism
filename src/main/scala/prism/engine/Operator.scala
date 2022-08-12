package prism.engine

import prism.core.data.{Batch, Tuple}
import prism.core.lattice.{Timestamp, Antichain}
import prism.core.semiring.Semiring

/**
 * Base contract for an operator node within a differential dataflow topology.
 * Operators consume batches of deltas at logical timestamps, maintain internal traces,
 * track progress frontiers, and emit resulting differential batches downstream.
 */
trait Operator[R] {
  def id: String
  def numInputs: Int
  def numOutputs: Int

  /**
   * Receives a batch of differential deltas on the given input port.
   */
  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit

  /**
   * Notifies the operator that the input frontier on inputPort has advanced to newFrontier.
   * Allows the operator to compact internal traces and advance its output frontiers.
   */
  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit

  /**
   * Executes pending incremental work, returning emitted batches directed to output ports.
   */
  def step(): Seq[(Int, Batch[Tuple, R])]

  /**
   * Checks whether the operator has pending internal work to compute.
   */
  def hasPendingWork: Boolean
}
