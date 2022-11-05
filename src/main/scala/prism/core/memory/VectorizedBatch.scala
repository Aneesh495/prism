package prism.core.memory

import prism.core.data.{Datum, Tuple}

/**
 * Columnar record batch supporting vectorized memory layout and operations.
 */
final class VectorizedBatch(
  val columns: Array[ColumnVector],
  val capacity: Int
) extends Serializable {

  def numColumns: Int = columns.length
  def numRows: Int = if (columns.isEmpty) 0 else columns(0).size

  def rowAsTuple(row: Int): Tuple = {
    val data = new Array[Datum](numColumns)
    var i = 0
    while (i < numColumns) {
      val col = columns(i)
      data(i) = if (col.isNull(row)) {
        Datum.NullVal
      } else {
        col match {
          case lc: LongColumnVector => Datum.I64(lc.get(row))
          case dc: DoubleColumnVector => Datum.F64(dc.get(row))
          case sc: StringColumnVector => Datum.Str(sc.get(row))
        }
      }
      i += 1
    }
    Tuple.fromArray(data)
  }

  def project(colIndices: Array[Int]): VectorizedBatch = {
    val projectedCols = colIndices.map(columns)
    new VectorizedBatch(projectedCols, capacity)
  }

  def filterIndices(predicate: Int => Boolean): Array[Int] = {
    val indices = collection.mutable.ArrayBuffer[Int]()
    var row = 0
    val total = numRows
    while (row < total) {
      if (predicate(row)) {
        indices += row
      }
      row += 1
    }
    indices.toArray
  }
}

object VectorizedBatch {
  def fromTuples(tuples: Seq[Tuple], capacity: Int): VectorizedBatch = {
    if (tuples.isEmpty) {
      return new VectorizedBatch(Array.empty, capacity)
    }

    val arity = tuples.head.arity
    val cols = new Array[ColumnVector](arity)

    // Determine column types from first non-null values
    for (c <- 0 until arity) {
      val sample = tuples.iterator.map(_(c)).find(_ != Datum.NullVal).getOrElse(Datum.I64(0L))
      cols(c) = sample match {
        case _: Datum.I64 => new LongColumnVector(capacity)
        case _: Datum.F64 => new DoubleColumnVector(capacity)
        case _: Datum.Str => new StringColumnVector(capacity)
        case _ => new StringColumnVector(capacity)
      }
    }

    for (tup <- tuples) {
      for (c <- 0 until arity) {
        val d = tup(c)
        val col = cols(c)
        d match {
          case Datum.NullVal => col.setNull(col.size)
          case Datum.I64(v) => col.asInstanceOf[LongColumnVector].append(v)
          case Datum.F64(v) => col.asInstanceOf[DoubleColumnVector].append(v)
          case Datum.Str(v) => col.asInstanceOf[StringColumnVector].append(v)
          case other => col.asInstanceOf[StringColumnVector].append(other.toString)
        }
      }
    }

    new VectorizedBatch(cols, capacity)
  }
}
