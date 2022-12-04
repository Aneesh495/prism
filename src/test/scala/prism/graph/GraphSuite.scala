package prism.graph

import munit.FunSuite
import prism.core.data.Datum

class GraphSuite extends FunSuite {

  test("PropertyGraph supports vertices, edges, and label indices") {
    val graph = new PropertyGraph()
    val v1 = graph.addVertex(1L, Set("User", "Admin"), Map("name" -> Datum.Str("Alice")))
    val v2 = graph.addVertex(2L, Set("User"), Map("name" -> Datum.Str("Bob")))
    val v3 = graph.addVertex(3L, Set("Server"), Map("host" -> Datum.Str("prod-1")))

    val e1 = graph.addEdge(10L, 1L, 2L, "KNOWS", Map("weight" -> Datum.F64(1.5)))
    val e2 = graph.addEdge(11L, 2L, 3L, "CONNECTS_TO", Map("weight" -> Datum.F64(3.0)))

    assertEquals(graph.getVertex(1L), Some(v1))
    assertEquals(graph.getVertex(99L), None)
    assertEquals(graph.getEdge(10L), Some(e1))

    val users = graph.getVerticesByLabel("User")
    assertEquals(users, Set(1L, 2L))

    val v1Out = graph.getOutEdges(1L)
    assertEquals(v1Out.map(_.to), List(2L))

    val v3In = graph.getInEdges(3L)
    assertEquals(v3In.map(_.from), List(2L))
  }

  test("PathFinder computes Dijkstra shortest paths on weighted graphs") {
    val graph = new PropertyGraph()
    // Nodes 1 to 4
    for (i <- 1L to 4L) {
      graph.addVertex(i)
    }

    // Edges: 1->2 (w=1.0), 2->4 (w=2.0) [path 1-2-4 total 3.0]
    // 1->3 (w=4.0), 3->4 (w=1.0) [path 1-3-4 total 5.0]
    graph.addEdge(101L, 1L, 2L, "LINK", Map("weight" -> Datum.F64(1.0)))
    graph.addEdge(102L, 2L, 4L, "LINK", Map("weight" -> Datum.F64(2.0)))
    graph.addEdge(103L, 1L, 3L, "LINK", Map("weight" -> Datum.F64(4.0)))
    graph.addEdge(104L, 3L, 4L, "LINK", Map("weight" -> Datum.F64(1.0)))

    val dists = PathFinder.dijkstra(graph, 1L)
    assertEquals(dists(1L), 0.0)
    assertEquals(dists(2L), 1.0)
    assertEquals(dists(3L), 4.0)
    assertEquals(dists(4L), 3.0)
  }

  test("CommunityDetection groups connected clusters via label propagation") {
    val graph = new PropertyGraph()
    // Cluster A: 1, 2, 3
    for (i <- 1L to 6L) graph.addVertex(i)

    // Complete triangle 1-2-3
    graph.addEdge(1L, 1L, 2L, "E")
    graph.addEdge(2L, 2L, 1L, "E")
    graph.addEdge(3L, 2L, 3L, "E")
    graph.addEdge(4L, 3L, 2L, "E")
    graph.addEdge(5L, 3L, 1L, "E")
    graph.addEdge(6L, 1L, 3L, "E")

    // Complete triangle 4-5-6
    graph.addEdge(7L, 4L, 5L, "E")
    graph.addEdge(8L, 5L, 4L, "E")
    graph.addEdge(9L, 5L, 6L, "E")
    graph.addEdge(10L, 6L, 5L, "E")
    graph.addEdge(11L, 6L, 4L, "E")
    graph.addEdge(12L, 4L, 6L, "E")

    // Single weak bridge between 3 and 4
    graph.addEdge(13L, 3L, 4L, "E")
    graph.addEdge(14L, 4L, 3L, "E")

    val communities = CommunityDetection.detectCommunities(graph, 1L to 6L, maxIterations = 25)
    // 1, 2, 3 should belong to the same community
    assertEquals(communities(1L), communities(2L))
    assertEquals(communities(2L), communities(3L))

    // 4, 5, 6 should belong to the same community
    assertEquals(communities(4L), communities(5L))
    assertEquals(communities(5L), communities(6L))
  }

  test("GraphPattern compiles multi-step graph path into Datalog rule") {
    val step1 = PathStep(
      fromNode = NodePattern("u", Some("Person")),
      edge = EdgePattern(Some("e1"), Some("FRIEND")),
      toNode = NodePattern("v")
    )
    val step2 = PathStep(
      fromNode = NodePattern("v"),
      edge = EdgePattern(Some("e2"), Some("FRIEND")),
      toNode = NodePattern("w", Some("Person"))
    )

    val rule = GraphPattern.compilePath("friendOfFriend", List(step1, step2))
    assertEquals(rule.head.predicate, "friendOfFriend")
    assertEquals(rule.head.terms.length, 2)
    assert(rule.body.exists {
      case prism.datalog.ast.PositiveAtom("FRIEND", _) => true
      case _ => false
    })
  }
}
