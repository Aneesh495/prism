package prism.datalog.optimizer

/**
 * HyperLogLog probabilistic cardinality sketch for distinct value estimation.
 * Uses 64-bit hashes and 1024 registers (b = 10, precision ~ 3.25%).
 */
final class HyperLogLog(val precision: Int = 10) extends Serializable {
  require(precision >= 4 && precision <= 16, s"Precision must be in [4, 16], got $precision")

  val numRegisters: Int = 1 << precision
  private val registers = new Array[Byte](numRegisters)

  private val alphaMM: Double = {
    val m = numRegisters.toDouble
    val alpha = precision match {
      case 4 => 0.673
      case 5 => 0.697
      case 6 => 0.709
      case _ => 0.7213 / (1.0 + 1.079 / m)
    }
    alpha * m * m
  }

  def add(value: Long): Unit = {
    val hash = hash64(value)
    val index = (hash >>> (64 - precision)).toInt
    val remaining = (hash << precision) | (1L << (precision - 1))
    val rank = (java.lang.Long.numberOfLeadingZeros(remaining) + 1).toByte

    if (rank > registers(index)) {
      registers(index) = rank
    }
  }

  def add(str: String): Unit = {
    add(hashString(str))
  }

  def estimate(): Long = {
    var sum = 0.0
    var zeroRegisters = 0
    var i = 0
    while (i < numRegisters) {
      val r = registers(i)
      sum += 1.0 / (1L << r)
      if (r == 0) zeroRegisters += 1
      i += 1
    }

    var estimate = alphaMM / sum
    val m = numRegisters.toDouble

    if (estimate <= 2.5 * m && zeroRegisters > 0) {
      // Linear counting for small cardinalities
      estimate = m * math.log(m / zeroRegisters.toDouble)
    }

    estimate.toLong
  }

  def merge(other: HyperLogLog): HyperLogLog = {
    require(this.precision == other.precision, "Cannot merge sketches with different precision")
    val merged = new HyperLogLog(precision)
    var i = 0
    while (i < numRegisters) {
      merged.registers(i) = math.max(this.registers(i), other.registers(i)).toByte
      i += 1
    }
    merged
  }

  private def hash64(x: Long): Long = {
    var h = x ^ (x >>> 33)
    h *= 0xff51afd7ed558ccdL
    h ^= (h >>> 33)
    h *= 0xc4ceb9fe1a85ec53L
    h ^= (h >>> 33)
    h
  }

  private def hashString(s: String): Long = {
    var h = 1125899906842597L
    var i = 0
    while (i < s.length) {
      h = 31 * h + s.charAt(i).toLong
      i += 1
    }
    hash64(h)
  }
}
