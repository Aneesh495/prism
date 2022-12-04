package prism.graph

import prism.core.data.{Datum, Tuple}
import scala.collection.mutable

final case class Vertex(
  id: Long,
  labels: Set[String],
  properties: Map[String, Datum]
)

final case class Edge(
  id: Long,
  from: Long,
  to: Long,
  label: String,
  properties: Map[String, Datum]
)

/**
 * In-memory Property Graph with label indexing and property lookups.
 * Interoperates directly with Prism differential Datalog relations.
 */
final class PropertyGraph extends Serializable {
  private val vertices = mutable.Map[Long, Vertex]()
  private val edges = mutable.Map[Long, Edge]()
  private val outEdges = mutable.Map[Long, mutable.ListBuffer[Edge]]()
  private val inEdges = mutable.Map[Long, mutable.ListBuffer[Edge]]()
  private val labelIndex = mutable.Map[String, mutable.Set[Long]]()

  def addVertex(id: Long, labels: Set[String] = Set.empty, properties: Map[String, Datum] = Map.empty): Vertex = synchronized {
    val v = Vertex(id, labels, properties)
    vertices.put(id, v)
    outEdges.getOrElseUpdate(id, mutable.ListBuffer())
    inEdges.getOrElseUpdate(id, mutable.ListBuffer())
    for (lbl <- labels) {
      labelIndex.getOrElseUpdate(lbl, mutable.Set()) += id
    }
    v
  }

  def addEdge(id: Long, from: Long, to: Long, label: String, properties: Map[String, Datum] = Map.empty): Edge = synchronized {
    require(vertices.contains(from), s"Source vertex $from does not exist")
    require(vertices.contains(to), s"Target vertex $to does not exist")
    val e = Edge(id, from, to, label, properties)
    edges.put(id, e)
    outEdges.getOrElseUpdate(from, mutable.ListBuffer()) += e
    inEdges.getOrElseUpdate(to, mutable.ListBuffer()) += e
    e
  }

  def getVertex(id: Long): Option[Vertex] = synchronized { vertices.get(id) }
  def getEdge(id: Long): Option[Edge] = synchronized { edges.get(id) }

  def getOutEdges(vertexId: Long): Seq[Edge] = synchronized {
    outEdges.getOrElse(vertexId, Seq.empty).toSeq
  }

  def getInEdges(vertexId: Long): Seq[Edge] = synchronized {
    inEdges.getOrElse(vertexId, Seq.empty).toSeq
  }

  def findByLabel(label: String): Set[Long] = synchronized {
    labelIndex.getOrElse(label, Set.empty).toSet
  }

  def getVerticesByLabel(label: String): Set[Long] = findByLabel(label)

  def numVertices: Int = synchronized { vertices.size }
  def numEdges: Int = synchronized { edges.size }

  /**
   * Exports graph edges as relational tuples: (from, to, label).
   */
  def toRelationTuples: Seq[Tuple] = synchronized {
    edges.values.map { e =>
      Tuple.of(e.from, e.to, e.label)
    }.toSeq
  }
}
