package prism.core.memory

import munit.FunSuite
import prism.core.data.{Datum, Tuple}

class MemorySuite extends FunSuite {

  test("LongColumnVector stores primitives and manages null bitmask") {
    val vec = new LongColumnVector(10)
    vec.append(100L)
    vec.appendNull()
    vec.append(200L)

    assertEquals(vec.size, 3)
    assertEquals(vec.get(0), 100L)
    assert(!vec.isNull(0))
    assert(vec.isNull(1))
    assertEquals(vec.get(2), 200L)
    assert(!vec.isNull(2))

    vec.sortInPlace()
    val search = vec.binarySearch(200L)
    assert(search >= 0)
  }

  test("DoubleColumnVector stores floating point data") {
    val vec = new DoubleColumnVector(5)
    vec.append(3.14)
    vec.append(2.71)

    assertEquals(vec.size, 2)
    assertEquals(vec.get(0), 3.14)
    assertEquals(vec.get(1), 2.71)
  }

  test("StringColumnVector performs dictionary encoding") {
    val vec = new StringColumnVector(10)
    vec.append("alpha")
    vec.append("beta")
    vec.append("alpha") // Reuses dictionary code

    assertEquals(vec.size, 3)
    assertEquals(vec.get(0), "alpha")
    assertEquals(vec.get(1), "beta")
    assertEquals(vec.get(2), "alpha")
    assertEquals(vec.dictionarySize, 2)
  }

  test("VectorizedBatch extracts tuples and projects columns") {
    val t1 = Tuple.of(1L, 10.5, "eventA")
    val t2 = Tuple.of(2L, 20.5, "eventB")

    val batch = VectorizedBatch.fromTuples(List(t1, t2), 10)
    assertEquals(batch.numRows, 2)
    assertEquals(batch.numColumns, 3)

    val row0 = batch.rowAsTuple(0)
    assertEquals(row0, t1)

    val proj = batch.project(Array(0, 2))
    assertEquals(proj.numColumns, 2)
    assertEquals(proj.rowAsTuple(0), Tuple.of(1L, "eventA"))
  }
}
