package prism.cli.tui

import prism.engine.{DataflowGraph, Operator, Reactor}

final case class OperatorStats(
  id: String,
  totalSteps: Long,
  deltasProcessed: Long,
  deltasEmitted: Long,
  pendingWorkItems: Int
)

final case class GraphProfile(
  totalOperators: Int,
  totalSources: Int,
  operatorStats: List[OperatorStats],
  quiescent: Boolean
) {
  def summaryString: String = {
    val sb = new StringBuilder()
    sb.append("=================================================================\n")
    sb.append("                   PRISM ENGINE PROFILE REPORT                   \n")
    sb.append("=================================================================\n")
    sb.append(f"Operators Count    : $totalOperators%d\n")
    sb.append(f"Source Streams     : $totalSources%d\n")
    sb.append(f"Engine Status      : ${if (quiescent) "QUIESCENT (Drained)" else "ACTIVE (Pending Work)"}%s\n")
    sb.append("-----------------------------------------------------------------\n")
    sb.append(f"${"Operator ID"}%-24s | ${"Steps"}%8s | ${"Pending"}%8s\n")
    sb.append("-----------------------------------------------------------------\n")
    for (stat <- operatorStats) {
      sb.append(f"${stat.id}%-24s | ${stat.totalSteps}%8d | ${stat.pendingWorkItems}%8d\n")
    }
    sb.append("=================================================================\n")
    sb.toString()
  }
}

/**
 * Profiling utility for inspecting dataflow graph execution performance and queue metrics.
 */
object ProfileReport {

  def capture[R](reactor: Reactor[R]): GraphProfile = {
    val ops = reactor.graph.allOperators
    val sources = reactor.graph.inputs
    val stats = ops.map { op =>
      OperatorStats(
        id = op.id,
        totalSteps = 0L,
        deltasProcessed = 0L,
        deltasEmitted = 0L,
        pendingWorkItems = if (op.hasPendingWork) 1 else 0
      )
    }.toList

    GraphProfile(
      totalOperators = ops.size,
      totalSources = sources.size,
      operatorStats = stats,
      quiescent = !ops.exists(_.hasPendingWork)
    )
  }
}
