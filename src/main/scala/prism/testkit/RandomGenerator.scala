package prism.testkit

import java.util.Random
import prism.core.data.{Datum, Tuple}
import prism.egraph.Expr

/**
 * Deterministic pseudo-random generator for property-based fuzzing and verification.
 */
final class RandomGenerator(val seed: Long = 42L) {
  private val rng = new Random(seed)

  def nextLong(bound: Long): Long = math.abs(rng.nextLong()) % bound
  def nextInt(bound: Int): Int = rng.nextInt(bound)
  def nextDouble(): Double = rng.nextDouble()

  def nextString(len: Int = 5): String = {
    val chars = "abcdefghijklmnopqrstuvwxyz"
    val sb = new StringBuilder()
    for (_ <- 0 until len) {
      sb.append(chars.charAt(rng.nextInt(chars.length)))
    }
    sb.toString()
  }

  def nextTuple(arity: Int): Tuple = {
    val arr = new Array[Datum](arity)
    for (i <- 0 until arity) {
      val choice = rng.nextInt(3)
      arr(i) = choice match {
        case 0 => Datum.I64(nextLong(100L))
        case 1 => Datum.Str(nextString(4))
        case 2 => Datum.Bool(rng.nextBoolean())
      }
    }
    Tuple.fromArray(arr)
  }

  def nextGraph(numNodes: Int, numEdges: Int): List[(Long, Long)] = {
    val edges = collection.mutable.Set[(Long, Long)]()
    for (_ <- 0 until numEdges) {
      val u = nextLong(numNodes)
      val v = nextLong(numNodes)
      if (u != v) edges += ((u, v))
    }
    edges.toList
  }

  def nextExpr(depth: Int): Expr = {
    if (depth <= 1) {
      if (rng.nextBoolean()) Expr.Var(s"v_${nextInt(4)}")
      else Expr.Const(nextLong(10L))
    } else {
      val op = rng.nextInt(4) match {
        case 0 => "+"
        case 1 => "-"
        case 2 => "*"
        case 3 => "/"
      }
      val left = nextExpr(depth - 1)
      val right = nextExpr(depth - 1)
      Expr.Op(op, List(left, right))
    }
  }
}
