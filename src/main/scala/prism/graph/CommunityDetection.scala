package prism.graph

import scala.collection.mutable

/**
 * Community detection via Label Propagation Algorithm (LPA).
 * Groups graph vertices into densely connected clusters.
 */
object CommunityDetection {

  def detectCommunities(
    graph: PropertyGraph,
    nodes: Seq[Long],
    maxIterations: Int = 20
  ): Map[Long, Long] = {
    // Initialize each node with its own unique community label
    val labels = mutable.Map[Long, Long]()
    for (node <- nodes) {
      labels.put(node, node)
    }

    var iter = 0
    var changed = true

    while (changed && iter < maxIterations) {
      iter += 1
      changed = false

      for (node <- nodes) {
        val neighbors = graph.getOutEdges(node).map(_.to) ++ graph.getInEdges(node).map(_.from)
        if (neighbors.nonEmpty) {
          // Count neighbor community frequencies
          val freq = mutable.Map[Long, Int]()
          for (nbr <- neighbors) {
            labels.get(nbr).foreach { lbl =>
              freq.put(lbl, freq.getOrElse(lbl, 0) + 1)
            }
          }

          if (freq.nonEmpty) {
            val maxCount = freq.values.max
            val dominantLabel = freq.filter(_._2 == maxCount).keys.min

            if (labels(node) != dominantLabel) {
              labels.put(node, dominantLabel)
              changed = true
            }
          }
        }
      }
    }

    labels.toMap
  }
}
