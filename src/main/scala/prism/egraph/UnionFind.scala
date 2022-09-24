package prism.egraph

import scala.collection.mutable.ArrayBuffer

/**
 * Type-safe identifier for an equivalence class in the E-Graph.
 */
final case class EClassId(value: Int) extends Ordered[EClassId] {
  def compare(that: EClassId): Int = java.lang.Integer.compare(this.value, that.value)
  override def toString: String = s"e$value"
}

/**
 * Disjoint-set union-find data structure with path compression and union-by-rank.
 * Maintains equivalence partitions among EClassIds.
 */
final class UnionFind extends Serializable {
  private val parents = new ArrayBuffer[Int]()
  private val ranks = new ArrayBuffer[Int]()

  def makeSet(): EClassId = {
    val id = parents.length
    parents += id
    ranks += 0
    EClassId(id)
  }

  def size: Int = parents.length

  /**
   * Finds the canonical representative of the equivalence class with two-pass path compression.
   */
  def find(id: EClassId): EClassId = {
    var root = id.value
    while (root != parents(root)) {
      root = parents(root)
    }
    // Path compression
    var curr = id.value
    while (curr != root) {
      val nxt = parents(curr)
      parents(curr) = root
      curr = nxt
    }
    EClassId(root)
  }

  /**
   * Merges two equivalence classes by rank.
   * Returns the new canonical representative.
   */
  def union(id1: EClassId, id2: EClassId): EClassId = {
    val root1 = find(id1).value
    val root2 = find(id2).value
    if (root1 == root2) {
      EClassId(root1)
    } else {
      val r1 = ranks(root1)
      val r2 = ranks(root2)
      if (r1 < r2) {
        parents(root1) = root2
        EClassId(root2)
      } else if (r1 > r2) {
        parents(root2) = root1
        EClassId(root1)
      } else {
        parents(root2) = root1
        ranks(root1) = r1 + 1
        EClassId(root1)
      }
    }
  }

  def isEquivalent(id1: EClassId, id2: EClassId): Boolean = {
    find(id1) == find(id2)
  }
}
