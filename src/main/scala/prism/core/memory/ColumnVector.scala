package prism.core.memory

import java.util.Arrays

/**
 * Base trait for high-performance contiguous column vectors.
 * Eliminates object allocation overhead for primitive data columns.
 */
sealed trait ColumnVector extends Serializable {
  def capacity: Int
  def size: Int
  def isNull(row: Int): Boolean
  def setNull(row: Int): Unit
}

/**
 * Primitive 64-bit integer column vector with validity bitmask.
 */
final class LongColumnVector(val capacity: Int) extends ColumnVector {
  private val data = new Array[Long](capacity)
  private val nullMask = new Array[Long]((capacity + 63) / 64)
  private var currentSize = 0

  def size: Int = currentSize

  def append(value: Long): Unit = {
    require(currentSize < capacity, "Vector capacity exceeded")
    data(currentSize) = value
    currentSize += 1
  }

  def appendNull(): Unit = {
    require(currentSize < capacity, "Vector capacity exceeded")
    setNull(currentSize)
    currentSize += 1
  }

  def get(row: Int): Long = {
    require(row < currentSize, s"Row index $row out of bounds ($currentSize)")
    data(row)
  }

  def set(row: Int, value: Long): Unit = {
    data(row) = value
    clearNull(row)
  }

  def isNull(row: Int): Boolean = {
    val word = row >>> 6
    val bit = row & 63
    (nullMask(word) & (1L << bit)) != 0L
  }

  def setNull(row: Int): Unit = {
    val word = row >>> 6
    val bit = row & 63
    nullMask(word) |= (1L << bit)
  }

  private def clearNull(row: Int): Unit = {
    val word = row >>> 6
    val bit = row & 63
    nullMask(word) &= ~(1L << bit)
  }

  def sortInPlace(): Unit = {
    Arrays.sort(data, 0, currentSize)
  }

  def binarySearch(target: Long): Int = {
    Arrays.binarySearch(data, 0, currentSize, target)
  }

  def toArray: Array[Long] = {
    val copy = new Array[Long](currentSize)
    System.arraycopy(data, 0, copy, 0, currentSize)
    copy
  }
}

/**
 * Primitive 64-bit floating point column vector.
 */
final class DoubleColumnVector(val capacity: Int) extends ColumnVector {
  private val data = new Array[Double](capacity)
  private val nullMask = new Array[Long]((capacity + 63) / 64)
  private var currentSize = 0

  def size: Int = currentSize

  def append(value: Double): Unit = {
    require(currentSize < capacity, "Vector capacity exceeded")
    data(currentSize) = value
    currentSize += 1
  }

  def appendNull(): Unit = {
    require(currentSize < capacity, "Vector capacity exceeded")
    setNull(currentSize)
    currentSize += 1
  }

  def get(row: Int): Double = {
    require(row < currentSize, s"Row index $row out of bounds ($currentSize)")
    data(row)
  }

  def set(row: Int, value: Double): Unit = {
    data(row) = value
    clearNull(row)
  }

  def isNull(row: Int): Boolean = {
    val word = row >>> 6
    val bit = row & 63
    (nullMask(word) & (1L << bit)) != 0L
  }

  def setNull(row: Int): Unit = {
    val word = row >>> 6
    val bit = row & 63
    nullMask(word) |= (1L << bit)
  }

  private def clearNull(row: Int): Unit = {
    val word = row >>> 6
    val bit = row & 63
    nullMask(word) &= ~(1L << bit)
  }

  def sortInPlace(): Unit = {
    Arrays.sort(data, 0, currentSize)
  }

  def toArray: Array[Double] = {
    val copy = new Array[Double](currentSize)
    System.arraycopy(data, 0, copy, 0, currentSize)
    copy
  }
}

/**
 * Dictionary-encoded string column vector.
 */
final class StringColumnVector(val capacity: Int) extends ColumnVector {
  private val dictionary = collection.mutable.ArrayBuffer[String]()
  private val dictIndex = collection.mutable.Map[String, Int]()
  private val codes = new Array[Int](capacity)
  private val nullMask = new Array[Long]((capacity + 63) / 64)
  private var currentSize = 0

  def size: Int = currentSize

  def append(value: String): Unit = {
    require(currentSize < capacity, "Vector capacity exceeded")
    val code = dictIndex.getOrElseUpdate(value, {
      val idx = dictionary.length
      dictionary += value
      idx
    })
    codes(currentSize) = code
    currentSize += 1
  }

  def appendNull(): Unit = {
    require(currentSize < capacity, "Vector capacity exceeded")
    setNull(currentSize)
    currentSize += 1
  }

  def get(row: Int): String = {
    require(row < currentSize, s"Row index $row out of bounds ($currentSize)")
    if (isNull(row)) null
    else dictionary(codes(row))
  }

  def isNull(row: Int): Boolean = {
    val word = row >>> 6
    val bit = row & 63
    (nullMask(word) & (1L << bit)) != 0L
  }

  def setNull(row: Int): Unit = {
    val word = row >>> 6
    val bit = row & 63
    nullMask(word) |= (1L << bit)
  }

  def dictionarySize: Int = dictionary.length
}
