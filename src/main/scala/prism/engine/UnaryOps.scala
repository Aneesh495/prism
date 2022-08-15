package prism.engine

import prism.core.data.{Batch, Delta, Tuple}
import prism.core.lattice.Antichain
import prism.core.semiring.Semiring
import scala.collection.mutable.ArrayBuffer

/**
 * Element-wise Map operator.
 * Applies a deterministic transformation f: Tuple => Tuple to each incoming delta.
 */
final class MapOp[R: Semiring](
  val id: String,
  val transform: Tuple => Tuple
) extends Operator[R] {

  val numInputs: Int = 1
  val numOutputs: Int = 1

  private val pending = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      pending += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = ()

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    if (pending.isEmpty) Nil
    else {
      val out = pending.map { b =>
        b.mapData(transform)
      }
      pending.clear()
      out.map(b => (0, b)).toSeq
    }
  }

  def hasPendingWork: Boolean = synchronized { pending.nonEmpty }
}

/**
 * Filter operator.
 * Preserves deltas whose tuple data satisfies the boolean predicate.
 */
final class FilterOp[R: Semiring](
  val id: String,
  val predicate: Tuple => Boolean
) extends Operator[R] {

  val numInputs: Int = 1
  val numOutputs: Int = 1

  private val pending = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      pending += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = ()

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    if (pending.isEmpty) Nil
    else {
      val out = pending.flatMap { b =>
        val filtered = b.filter(predicate)
        if (filtered.nonEmpty) Some((0, filtered)) else None
      }
      pending.clear()
      out.toSeq
    }
  }

  def hasPendingWork: Boolean = synchronized { pending.nonEmpty }
}

/**
 * FlatMap operator.
 * Expands each input tuple to 0 or more tuples.
 */
final class FlatMapOp[R: Semiring](
  val id: String,
  val expand: Tuple => Iterable[Tuple]
) extends Operator[R] {

  val numInputs: Int = 1
  val numOutputs: Int = 1

  private val pending = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      pending += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = ()

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    if (pending.isEmpty) Nil
    else {
      val out = pending.map { b =>
        b.flatMapData(expand)
      }
      pending.clear()
      out.filter(_.nonEmpty).map(b => (0, b)).toSeq
    }
  }

  def hasPendingWork: Boolean = synchronized { pending.nonEmpty }
}

/**
 * Projection operator: extracts a specified subset of columns from each tuple.
 */
final class ProjectOp[R: Semiring](
  val id: String,
  val columnIndices: Array[Int]
) extends Operator[R] {

  val numInputs: Int = 1
  val numOutputs: Int = 1

  private val pending = new ArrayBuffer[Batch[Tuple, R]]()

  def receive(inputPort: Int, batch: Batch[Tuple, R]): Unit = synchronized {
    if (batch.nonEmpty) {
      pending += batch
    }
  }

  def advanceFrontier(inputPort: Int, newFrontier: Antichain): Unit = ()

  def step(): Seq[(Int, Batch[Tuple, R])] = synchronized {
    if (pending.isEmpty) Nil
    else {
      val out = pending.map { b =>
        b.mapData(_.project(columnIndices))
      }
      pending.clear()
      out.filter(_.nonEmpty).map(b => (0, b)).toSeq
    }
  }

  def hasPendingWork: Boolean = synchronized { pending.nonEmpty }
}
