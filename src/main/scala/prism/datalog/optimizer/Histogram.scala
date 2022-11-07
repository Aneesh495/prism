package prism.datalog.optimizer

/**
 * Equi-width histogram for numeric column selectivity and frequency estimation.
 */
final class EquiWidthHistogram(
  val minVal: Double,
  val maxVal: Double,
  val numBuckets: Int
) extends Serializable {
  require(maxVal >= minVal, "maxVal must be >= minVal")
  require(numBuckets > 0, "numBuckets must be > 0")

  private val bucketWidth: Double = if (maxVal == minVal) 1.0 else (maxVal - minVal) / numBuckets
  private val counts = new Array[Long](numBuckets)
  private var totalCount: Long = 0L

  def add(value: Double): Unit = {
    if (value >= minVal && value <= maxVal) {
      val idx = if (value == maxVal) numBuckets - 1 else math.min(numBuckets - 1, math.max(0, ((value - minVal) / bucketWidth).toInt))
      counts(idx) += 1
      totalCount += 1
    }
  }

  def total: Long = totalCount

  /**
   * Estimates fraction of rows satisfying value <= upper.
   */
  def estimateSelectivityLessOrEqual(upper: Double): Double = {
    if (totalCount == 0L || upper < minVal) 0.0
    else if (upper >= maxVal) 1.0
    else {
      val targetBucket = ((upper - minVal) / bucketWidth).toInt
      var sum = 0.0
      var i = 0
      while (i < targetBucket) {
        sum += counts(i)
        i += 1
      }
      // Interpolate inside target bucket
      val bucketLower = minVal + targetBucket * bucketWidth
      val fractionInBucket = (upper - bucketLower) / bucketWidth
      sum += counts(targetBucket) * math.min(1.0, math.max(0.0, fractionInBucket))
      sum / totalCount.toDouble
    }
  }

  /**
   * Estimates fraction of rows satisfying lower <= value <= upper.
   */
  def estimateSelectivityRange(lower: Double, upper: Double): Double = {
    val selUpper = estimateSelectivityLessOrEqual(upper)
    val selLower = estimateSelectivityLessOrEqual(lower)
    math.max(0.0, selUpper - selLower)
  }
}
