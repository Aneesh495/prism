package prism.engine

import prism.core.data.{Batch, Tuple}
import prism.core.semiring.Semiring
import scala.collection.mutable

/**
 * Directed edge connecting an operator's output port to another operator's input port.
 */
final case class Edge(
  fromOperatorId: String,
  fromPort: Int,
  toOperatorId: String,
  toPort: Int
)

/**
 * Dataflow topology representing a network of differential operators.
 */
final class DataflowGraph[R: Semiring] {
  private val operators = mutable.LinkedHashMap[String, Operator[R]]()
  private val edges = mutable.ArrayBuffer[Edge]()
  private val inputSources = mutable.Set[String]()
  private val outputSinks = mutable.Set[String]()

  def addOperator(op: Operator[R]): DataflowGraph[R] = {
    operators.put(op.id, op)
    this
  }

  def addEdge(fromOpId: String, fromPort: Int, toOpId: String, toPort: Int): DataflowGraph[R] = {
    require(operators.contains(fromOpId), s"Source operator $fromOpId not registered")
    require(operators.contains(toOpId), s"Target operator $toOpId not registered")
    edges += Edge(fromOpId, fromPort, toOpId, toPort)
    this
  }

  def markInput(operatorId: String): DataflowGraph[R] = {
    inputSources += operatorId
    this
  }

  def markOutput(operatorId: String): DataflowGraph[R] = {
    outputSinks += operatorId
    this
  }

  def getOperator(id: String): Option[Operator[R]] = operators.get(id)

  def allOperators: Seq[Operator[R]] = operators.values.toSeq

  def outgoingEdges(fromOpId: String, fromPort: Int): Seq[Edge] = {
    edges.filter(e => e.fromOperatorId == fromOpId && e.fromPort == fromPort).toSeq
  }

  def allEdges: Seq[Edge] = edges.toSeq
  def inputs: Set[String] = inputSources.toSet
  def outputs: Set[String] = outputSinks.toSet
}
