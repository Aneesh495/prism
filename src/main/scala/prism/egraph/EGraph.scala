package prism.egraph

import scala.collection.mutable

/**
 * An equivalence class containing mutually equivalent ENodes.
 */
final class EClass(
  val id: EClassId,
  val nodes: mutable.Set[ENode] = mutable.Set.empty,
  val parents: mutable.ArrayBuffer[(ENode, EClassId)] = mutable.ArrayBuffer.empty
) {
  override def toString: String = s"EClass($id, {${nodes.mkString(", ")}})"
}

/**
 * Equivalence Graph (E-Graph) supporting hashconsing, congruence closure,
 * and equality saturation.
 */
final class EGraph extends Serializable {
  val unionFind = new UnionFind()
  private val memo = mutable.Map[ENode, EClassId]()
  private val classesMap = mutable.Map[EClassId, EClass]()
  private val worklist = mutable.Queue[EClassId]()

  def numClasses: Int = classesMap.size
  def numNodes: Int = classesMap.values.map(_.nodes.size).sum

  def find(id: EClassId): EClassId = unionFind.find(id)

  def getClass(id: EClassId): Option[EClass] = {
    classesMap.get(find(id))
  }

  def allClasses: Iterable[EClass] = classesMap.values

  /**
   * Adds an ENode into the e-graph with hashconsing deduplication.
   */
  def add(node: ENode): EClassId = {
    val canonicalNode = node.canonicalize(unionFind)
    memo.get(canonicalNode) match {
      case Some(existingId) =>
        find(existingId)
      case None =>
        val newId = unionFind.makeSet()
        val eclass = new EClass(newId)
        eclass.nodes += canonicalNode
        classesMap.put(newId, eclass)
        memo.put(canonicalNode, newId)

        // Register this node as a parent in all child classes
        for (childId <- canonicalNode.children) {
          getClass(childId).foreach { childClass =>
            childClass.parents += ((canonicalNode, newId))
          }
        }
        newId
    }
  }

  /**
   * Recursively adds a high-level Expr AST into the e-graph.
   */
  def addExpr(expr: Expr): EClassId = {
    expr match {
      case Expr.Var(name) =>
        add(ENode(name, Nil))
      case Expr.Const(v) =>
        add(ENode(v.toString, Nil))
      case Expr.Op(name, args) =>
        val childIds = args.map(addExpr)
        add(ENode(name, childIds))
    }
  }

  /**
   * Merges two equivalence classes, placing the result onto the rebuild worklist.
   */
  def merge(id1: EClassId, id2: EClassId): EClassId = {
    val root1 = find(id1)
    val root2 = find(id2)
    if (root1 == root2) root1
    else {
      val newRoot = unionFind.union(root1, root2)
      val otherRoot = if (newRoot == root1) root2 else root1

      worklist.enqueue(newRoot)

      // Merge node sets and parent lists
      val targetClass = classesMap(newRoot)
      classesMap.remove(otherRoot).foreach { oldClass =>
        targetClass.nodes ++= oldClass.nodes
        targetClass.parents ++= oldClass.parents
      }

      newRoot
    }
  }

  /**
   * Restores the congruence closure invariant across all equivalence classes.
   * Merges classes whose nodes have become identical due to child canonicalization.
   */
  def rebuild(): Unit = {
    while (worklist.nonEmpty) {
      val todo = worklist.dequeueAll(_ => true)
      for (classId <- todo) {
        val canonicalId = find(classId)
        getClass(canonicalId).foreach { eclass =>
          // 1. Canonicalize all parent nodes
          val newParents = mutable.ArrayBuffer[(ENode, EClassId)]()
          val parentsSnapshot = eclass.parents.toList
          for ((pNode, pClass) <- parentsSnapshot) {
            memo.remove(pNode)
            val canonicalParent = pNode.canonicalize(unionFind)
            val canonicalPClass = find(pClass)

            memo.get(canonicalParent) match {
              case Some(existingClass) =>
                if (find(existingClass) != canonicalPClass) {
                  merge(existingClass, canonicalPClass)
                }
              case None =>
                memo.put(canonicalParent, canonicalPClass)
            }
            newParents += ((canonicalParent, canonicalPClass))
          }
          eclass.parents.clear()
          eclass.parents ++= newParents.distinct

          // 2. Canonicalize nodes inside this eclass
          val canonicalNodes = eclass.nodes.toList.map(_.canonicalize(unionFind))
          eclass.nodes.clear()
          eclass.nodes ++= canonicalNodes
        }
      }
    }
  }
}
