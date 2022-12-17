package prism.engine.parallel

import prism.core.data.{Batch, Delta, Tuple}
import prism.core.semiring.Semiring
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * Key-based tuple hash partitioner for multi-worker parallel execution.
 * Distributes differential batches evenly across N worker partitions.
 */
final class HashPartitioner(val numPartitions: Int) {
  require(numPartitions > 0, "Number of partitions must be strictly positive")

  /**
   * Computes a 32-bit positive partition index for a given tuple using the specified key column.
   */
  def partition(tup: Tuple, keyCol: Int): Int = {
    val h = tup(keyCol).hashCode()
    // Smear bits via avalanche multiplier
    val smeared = (h ^ (h >>> 16)) * 0x45d9f3b
    val finalH = (smeared ^ (smeared >>> 16)) & 0x7fffffff
    finalH % numPartitions
  }

  /**
   * Partitions an incoming batch into N sub-batches, one for each worker.
   */
  def splitBatch[D: Ordering: ClassTag, R: Semiring](
    batch: Batch[D, R],
    extractTuple: D => Tuple,
    keyCol: Int
  ): Array[Batch[D, R]] = {
    if (batch.isEmpty || numPartitions == 1) {
      val res = new Array[Batch[D, R]](numPartitions)
      res(0) = batch
      var i = 1
      while (i < numPartitions) {
        res(i) = Batch.empty[D, R]
        i += 1
      }
      return res
    }

    val buckets = Array.fill(numPartitions)(new mutable.ArrayBuffer[Delta[D, R]]())
    for (delta <- batch.iterator) {
      val tup = extractTuple(delta.data)
      val part = partition(tup, keyCol)
      buckets(part) += delta
    }

    val out = new Array[Batch[D, R]](numPartitions)
    var p = 0
    while (p < numPartitions) {
      out(p) = Batch.fromSeq(buckets(p).toSeq)
      p += 1
    }
    out
  }
}
