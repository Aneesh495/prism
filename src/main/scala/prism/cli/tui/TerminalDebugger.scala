package prism.cli.tui

import prism.engine.{DataflowGraph, Operator, Reactor}

/**
 * Terminal-based visual debugger for inspecting differential dataflows step-by-step.
 */
final class TerminalDebugger[R](val reactor: Reactor[R]) {
  private var currentStep = 0

  def stepOnce(): Boolean = {
    currentStep += 1
    println(s"\n--- [Step $currentStep] Stepping Operators ---")
    val anyWork = reactor.stepSinglePass()
    if (!anyWork) {
      println("  * Quiescence reached: all operator work queues are drained.")
    } else {
      println("  * Dataflow pass executed successfully.")
    }
    anyWork
  }

  def runToQuiescence(maxSteps: Int = 100): Int = {
    var steps = 0
    while (stepOnce() && steps < maxSteps) {
      steps += 1
    }
    steps
  }
}
