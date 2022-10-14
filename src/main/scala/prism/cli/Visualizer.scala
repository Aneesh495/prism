package prism.cli

import prism.engine.{DataflowGraph, Edge}
import prism.egraph.EGraph

/**
 * Visualizer exporting dataflow topologies and e-graphs to Mermaid and Graphviz formats.
 */
object Visualizer {

  def toMermaidDataflow(graph: DataflowGraph[?]): String = {
    val sb = new StringBuilder()
    sb.append("flowchart TD\n")

    for (op <- graph.allOperators) {
      val isInput = graph.inputs.contains(op.id)
      val isOutput = graph.outputs.contains(op.id)
      val shape = if (isInput) s"""["[IN] ${op.id}"]"""
                  else if (isOutput) s"""(["[OUT] ${op.id}"])"""
                  else s"""["${op.id}"]"""
      sb.append(s"  ${sanitizeId(op.id)}$shape\n")
    }

    for (edge <- graph.allEdges) {
      val fromId = sanitizeId(edge.fromOperatorId)
      val toId = sanitizeId(edge.toOperatorId)
      sb.append(s"  $fromId -->|p${edge.fromPort}:${edge.toPort}| $toId\n")
    }

    sb.toString()
  }

  def toDotDataflow(graph: DataflowGraph[?]): String = {
    val sb = new StringBuilder()
    sb.append("digraph Dataflow {\n")
    sb.append("  rankdir=LR;\n")
    sb.append("  node [shape=box, style=rounded, fontname=\"Helvetica\"];\n")

    for (op <- graph.allOperators) {
      val isInput = graph.inputs.contains(op.id)
      val isOutput = graph.outputs.contains(op.id)
      val color = if (isInput) "lightblue" else if (isOutput) "lightgreen" else "white"
      sb.append(s"""  "${op.id}" [style="filled,rounded", fillcolor="$color"];\n""")
    }

    for (edge <- graph.allEdges) {
      sb.append(s"""  "${edge.fromOperatorId}" -> "${edge.toOperatorId}" [label="p${edge.fromPort}:${edge.toPort}"];\n""")
    }

    sb.append("}\n")
    sb.toString()
  }

  def toMermaidEGraph(egraph: EGraph): String = {
    val sb = new StringBuilder()
    sb.append("flowchart TD\n")

    for (eclass <- egraph.allClasses) {
      val classIdStr = egraph.find(eclass.id).toString
      sb.append(s"  subgraph $classIdStr [Class $classIdStr]\n")
      for ((node, idx) <- eclass.nodes.zipWithIndex) {
        val nodeId = s"${classIdStr}_node_$idx"
        sb.append(s"""    $nodeId["${node.op}"]\n""")
      }
      sb.append("  end\n")
    }

    for (eclass <- egraph.allClasses) {
      val classIdStr = egraph.find(eclass.id).toString
      for ((node, idx) <- eclass.nodes.zipWithIndex) {
        val nodeId = s"${classIdStr}_node_$idx"
        for ((childId, argIdx) <- node.children.zipWithIndex) {
          val canonicalChild = egraph.find(childId).toString
          sb.append(s"  $nodeId -.->|arg$argIdx| $canonicalChild\n")
        }
      }
    }

    sb.toString()
  }

  private def sanitizeId(raw: String): String = {
    raw.replaceAll("[^a-zA-Z0-9_]", "_")
  }
}

/**
 * Pretty table formatter for relational tuples and query outputs.
 */
object TableFormatter {

  def format(headers: Seq[String], rows: Seq[Seq[String]]): String = {
    if (headers.isEmpty && rows.isEmpty) return "(empty table)"

    val colCount = if (headers.nonEmpty) headers.length else rows.head.length
    val colWidths = new Array[Int](colCount)

    for ((h, i) <- headers.zipWithIndex) {
      colWidths(i) = math.max(colWidths(i), h.length)
    }

    for (row <- rows) {
      for ((cell, i) <- row.zipWithIndex if i < colCount) {
        colWidths(i) = math.max(colWidths(i), cell.length)
      }
    }

    val sb = new StringBuilder()

    // Top border
    sb.append("+-").append(colWidths.map("-" * _).mkString("-+-")).append("-+\n")

    // Headers
    if (headers.nonEmpty) {
      sb.append("| ")
      for (i <- 0 until colCount) {
        val h = headers(i)
        sb.append(h.padTo(colWidths(i), ' ')).append(" | ")
      }
      sb.setLength(sb.length - 1)
      sb.append("\n")
      sb.append("+-").append(colWidths.map("=" * _).mkString("-+-")).append("-+\n")
    }

    // Rows
    for (row <- rows) {
      sb.append("| ")
      for (i <- 0 until colCount) {
        val cell = if (i < row.length) row(i) else ""
        sb.append(cell.padTo(colWidths(i), ' ')).append(" | ")
      }
      sb.setLength(sb.length - 1)
      sb.append("\n")
    }

    // Bottom border
    sb.append("+-").append(colWidths.map("-" * _).mkString("-+-")).append("-+\n")
    sb.append(s"(${rows.length} rows)\n")
    sb.toString()
  }
}
