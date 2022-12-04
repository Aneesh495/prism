package prism.graph

import prism.core.semiring.Tropical
import scala.collection.mutable

final case class WeightedPath(
  nodes: List[Long],
  totalWeight: Double
)

/**
 * Path finding algorithms: Dijkstra SSSP via Tropical algebra, All-Pairs Shortest Path,
 * and k-hop neighborhood traversals.
 */
object PathFinder {

  def dijkstra(graph: PropertyGraph, source: Long, weightProperty: String = "weight"): Map[Long, Double] = {
    val distances = mutable.Map[Long, Double]()
    val pq = mutable.PriorityQueue[(Double, Long)]()(Ordering.by(-_._1))

    distances.put(source, 0.0)
    pq.enqueue((0.0, source))

    val visited = mutable.Set[Long]()

    while (pq.nonEmpty) {
      val (dist, u) = pq.dequeue()
      if (!visited.contains(u)) {
        visited += u

        for (edge <- graph.getOutEdges(u)) {
          val v = edge.to
          val w = edge.properties.get(weightProperty) match {
            case Some(prism.core.data.Datum.F64(valD)) => valD
            case Some(prism.core.data.Datum.I64(valL)) => valL.toDouble
            case _ => 1.0
          }

          val alt = dist + w
          if (alt < distances.getOrElse(v, Double.PositiveInfinity)) {
            distances.put(v, alt)
            pq.enqueue((alt, v))
          }
        }
      }
    }

    distances.toMap
  }

  def kHopNeighborhood(graph: PropertyGraph, source: Long, maxHops: Int): Set[Long] = {
    val visited = mutable.Set[Long](source)
    var currentFrontier = Set[Long](source)

    var hop = 0
    while (hop < maxHops && currentFrontier.nonEmpty) {
      hop += 1
      val nextFrontier = mutable.Set[Long]()
      for (u <- currentFrontier) {
        for (edge <- graph.getOutEdges(u)) {
          val v = edge.to
          if (!visited.contains(v)) {
            visited += v
            nextFrontier += v
          }
        }
      }
      currentFrontier = nextFrontier.toSet
    }

    visited.toSet
  }

  def allPairsShortestPath(graph: PropertyGraph, numNodes: Int): Array[Array[Double]] = {
    val dist = Array.fill(numNodes, numNodes)(Double.PositiveInfinity)
    for (i <- 0 until numNodes) dist(i)(i) = 0.0

    for (u <- 0 until numNodes) {
      for (edge <- graph.getOutEdges(u)) {
        val v = edge.to.toInt
        if (v < numNodes) {
          dist(u)(v) = math.min(dist(u)(v), 1.0)
        }
      }
    }

    // Floyd-Warshall
    for (k <- 0 until numNodes) {
      for (i <- 0 until numNodes) {
        for (j <- 0 until numNodes) {
          if (dist(i)(k) + dist(k)(j) < dist(i)(j)) {
            dist(i)(j) = dist(i)(k) + dist(k)(j)
          }
        }
      }
    }

    dist
  }
}
