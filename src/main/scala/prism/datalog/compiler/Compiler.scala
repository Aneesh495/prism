package prism.datalog.compiler

import prism.core.data.{Batch, Datum, Delta, Tuple}
import prism.core.lattice.Timestamp
import prism.core.semiring.{AbelianGroup, DiffInt, Semiring}
import prism.datalog.analysis.{Safety, Stratifier}
import prism.datalog.ast._
import prism.engine._
import scala.collection.mutable

/**
 * Result of compiling a Datalog program into an incremental differential execution pipeline.
 */
final class CompiledDatalog(
  val graph: DataflowGraph[Long],
  val reactor: Reactor[Long],
  val edbPredicates: Set[String],
  val idbPredicates: Set[String]
) {
  private var currentEpoch: Long = 0L

  /**
   * Evaluates the program to fixpoint and returns snapshot contents of all IDB relations.
   */
  def run(): Map[String, Set[Tuple]] = synchronized {
    reactor.stepUntilQuiescence()
    idbPredicates.map { pred =>
      val snap = reactor.snapshotAt(pred, Timestamp(currentEpoch))
      val tuples = snap.filter(_._2 > 0L).keySet
      pred -> tuples
    }.toMap
  }

  /**
   * Queries a specific relation snapshot at the current epoch.
   */
  def query(predicate: String): Set[Tuple] = synchronized {
    reactor.stepUntilQuiescence()
    val snap = reactor.snapshotAt(predicate, Timestamp(currentEpoch))
    snap.filter(_._2 > 0L).keySet
  }

  /**
   * Dynamically inserts a fact at the next streaming epoch, triggering incremental updates.
   */
  def insertFact(predicate: String, values: Tuple, weight: Long = 1L): Unit = synchronized {
    currentEpoch += 1
    val delta = Delta(values, Timestamp(currentEpoch), weight)
    val batch = Batch.single[Tuple, Long](values, Timestamp(currentEpoch), weight)(using summon[Ordering[Tuple]], DiffInt.diffIntGroup)
    reactor.insertInput(s"src_$predicate", batch)
    reactor.stepUntilQuiescence()
  }

  /**
   * Dynamically retracts a fact at the next streaming epoch with negative weight.
   */
  def retractFact(predicate: String, values: Tuple, weight: Long = 1L): Unit = synchronized {
    currentEpoch += 1
    val batch = Batch.single[Tuple, Long](values, Timestamp(currentEpoch), -weight)(using summon[Ordering[Tuple]], DiffInt.diffIntGroup)
    reactor.insertInput(s"src_$predicate", batch)
    reactor.stepUntilQuiescence()
  }
}

/**
 * Compiler translating stratified Horn clause programs into differential dataflow graphs.
 */
object Compiler {
  given AbelianGroup[Long] = DiffInt.diffIntGroup

  def compile(program: Program): CompiledDatalog = {
    // 1. Validate rule safety
    Safety.validateProgram(program)

    // 2. Identify EDB and IDB predicates
    val ruleHeadPredicates = program.rules.map(_.head.predicate).toSet
    val edbPredicates = program.facts.map(_.predicate).toSet -- ruleHeadPredicates
    val allPredicates = edbPredicates ++ ruleHeadPredicates

    val graph = new DataflowGraph[Long]()

    // Register input source for each predicate (both EDB and IDB can receive initial input)
    for (pred <- allPredicates) {
      val sourceOp = new MapOp[Long](s"src_$pred", identity)
      graph.addOperator(sourceOp)
      graph.markInput(s"src_$pred")

      // Target sink for reading outputs
      val sinkOp = new MapOp[Long](pred, identity)
      graph.addOperator(sinkOp)
      graph.addEdge(sourceOp.id, 0, sinkOp.id, 0)
      graph.markOutput(pred)
    }

    // 3. Stratify rules
    val strata = Stratifier.stratify(program)

    var opCounter = 0
    def nextOpId(prefix: String): String = {
      opCounter += 1
      s"${prefix}_$opCounter"
    }

    // 4. Compile rules stratum by stratum
    for ((stratumRules, stratumIdx) <- strata.zipWithIndex) {
      val stratumHeads = stratumRules.map(_.head.predicate).toSet
      val isRecursive = stratumRules.exists { r =>
        r.body.exists {
          case PositiveAtom(p, _) => stratumHeads.contains(p)
          case _ => false
        }
      }

      if (!isRecursive) {
        // Non-recursive stratum: direct operator pipeline
        for (rule <- stratumRules) {
          compileRule(rule, graph, nextOpId)
        }
      } else {
        // Recursive stratum: compile with IterateOp feedback loop
        for (headPred <- stratumHeads) {
          val loopOp = new IterateOp[Long](nextOpId(s"loop_$headPred"))
          graph.addOperator(loopOp)
          graph.addEdge(loopOp.id, 1, headPred, 0)

          val rulesForPred = stratumRules.filter(_.head.predicate == headPred)
          for (rule <- rulesForPred) {
            val isRuleRec = rule.body.exists {
              case PositiveAtom(p, _) => stratumHeads.contains(p)
              case _ => false
            }

            if (!isRuleRec) {
              // Base rule: route into loop entry port 0
              compileRuleToTarget(rule, graph, nextOpId, loopOp.id, 0, None)
            } else {
              // Recursive rule: route into loop feedback port 1, reading headPred from loopOp
              compileRuleToTarget(rule, graph, nextOpId, loopOp.id, 1, Some(loopOp.id))
            }
          }
        }
      }
    }

    val reactor = new Reactor[Long](graph)

    // 5. Preload EDB facts at Timestamp.Zero
    val factsByPred = program.facts.groupBy(_.predicate)
    for ((pred, pFacts) <- factsByPred) {
      val deltas = pFacts.map { f =>
        Delta(Tuple.fromSeq(f.values), Timestamp.Zero, f.weight)
      }.toArray
      val batch = Batch.fromUnsorted(deltas)
      reactor.insertInput(s"src_$pred", batch)
    }

    new CompiledDatalog(graph, reactor, edbPredicates, ruleHeadPredicates)
  }

  private def compileRule(
    rule: Rule,
    graph: DataflowGraph[Long],
    nextOpId: String => String
  ): Unit = {
    compileRuleToTarget(rule, graph, nextOpId, rule.head.predicate, 0, None)
  }

  private def compileRuleToTarget(
    rule: Rule,
    graph: DataflowGraph[Long],
    nextOpId: String => String,
    targetOpId: String,
    targetPort: Int,
    loopOpId: Option[String]
  ): Unit = {
    val positiveAtoms = rule.body.collect { case p: PositiveAtom => p }
    val negatedAtoms = rule.body.collect { case n: NegatedAtom => n }
    val comparisons = rule.body.collect { case c: ComparisonLiteral => c }

    if (positiveAtoms.isEmpty) {
      // Ground rule or empty body
      return
    }

    // Pipeline starts from first positive atom
    val firstAtom = positiveAtoms.head
    var currentVarSchema = firstAtom.terms.map {
      case Term.Var(name) => name
      case Term.Const(_) => "_"
      case other => "_"
    }.toArray

    var (currentOpId, currentPort) = if (loopOpId.isDefined && firstAtom.predicate == rule.head.predicate) {
      (loopOpId.get, 0)
    } else {
      (s"src_${firstAtom.predicate}", 0)
    }

    // Filter constants in first atom if any
    val constFilters = firstAtom.terms.zipWithIndex.collect {
      case (Term.Const(expected), idx) => (idx, expected)
    }
    if (constFilters.nonEmpty) {
      val filterOp = new FilterOp[Long](
        nextOpId("filter_const"),
        tup => constFilters.forall { case (idx, expected) => tup(idx) == expected }
      )
      graph.addOperator(filterOp)
      graph.addEdge(currentOpId, currentPort, filterOp.id, 0)
      currentOpId = filterOp.id
      currentPort = 0
    }

    // Join remaining positive atoms
    for (atom <- positiveAtoms.tail) {
      val atomVarSchema = atom.terms.map {
        case Term.Var(name) => name
        case _ => "_"
      }.toArray

      val commonVars = currentVarSchema.intersect(atomVarSchema).filterNot(_ == "_")
      val keyIndicesA = commonVars.map(v => currentVarSchema.indexOf(v))
      val keyIndicesB = commonVars.map(v => atomVarSchema.indexOf(v))

      val joinOp = new JoinOp[Long](
        nextOpId(s"join_${atom.predicate}"),
        keyIndicesA,
        keyIndicesB,
        JoinOp.naturalJoinCombine(keyIndicesB)
      )
      graph.addOperator(joinOp)
      graph.addEdge(currentOpId, currentPort, joinOp.id, 0)

      val (atomSourceId, atomSourcePort) = if (loopOpId.isDefined && atom.predicate == rule.head.predicate) {
        (loopOpId.get, 0)
      } else {
        (s"src_${atom.predicate}", 0)
      }
      graph.addEdge(atomSourceId, atomSourcePort, joinOp.id, 1)

      // Update schema: current schema + non-key columns of B
      val bNonKeyVars = atomVarSchema.zipWithIndex.filterNot { case (_, idx) => keyIndicesB.contains(idx) }.map(_._1)
      currentVarSchema = currentVarSchema ++ bNonKeyVars
      currentOpId = joinOp.id
      currentPort = 0
    }

    // Apply comparisons
    for (comp <- comparisons) {
      val compOp = new FilterOp[Long](
        nextOpId("filter_comp"),
        tup => evaluateComparison(comp, tup, currentVarSchema)
      )
      graph.addOperator(compOp)
      graph.addEdge(currentOpId, currentPort, compOp.id, 0)
      currentOpId = compOp.id
      currentPort = 0
    }

    // Apply antijoins for negated atoms
    for (neg <- negatedAtoms) {
      val negVarSchema = neg.terms.map {
        case Term.Var(name) => name
        case _ => "_"
      }.toArray
      val commonVars = currentVarSchema.intersect(negVarSchema).filterNot(_ == "_")
      val keyIndicesA = commonVars.map(v => currentVarSchema.indexOf(v))
      val keyIndicesB = commonVars.map(v => negVarSchema.indexOf(v))

      val antijoinOp = new AntijoinOp[Long](
        nextOpId(s"antijoin_${neg.predicate}"),
        keyIndicesA,
        keyIndicesB
      )
      graph.addOperator(antijoinOp)
      graph.addEdge(currentOpId, currentPort, antijoinOp.id, 0)
      graph.addEdge(s"src_${neg.predicate}", 0, antijoinOp.id, 1)

      currentOpId = antijoinOp.id
      currentPort = 0
    }

    // Project onto head variables
    val headIndices = rule.head.terms.map {
      case Term.Var(name) => currentVarSchema.indexOf(name)
      case _ => -1
    }.toArray

    val projectOp = new MapOp[Long](
      nextOpId("project_head"),
      tup => {
        val outValues = headIndices.map { idx =>
          if (idx >= 0 && idx < tup.arity) tup(idx)
          else Datum.NullVal
        }
        Tuple.fromArray(outValues)
      }
    )
    graph.addOperator(projectOp)
    graph.addEdge(currentOpId, currentPort, projectOp.id, 0)

    // Route to target operator port
    graph.addEdge(projectOp.id, 0, targetOpId, targetPort)
  }

  private def evaluateComparison(comp: ComparisonLiteral, tup: Tuple, schema: Array[String]): Boolean = {
    val leftVal = evaluateTerm(comp.left, tup, schema)
    val rightVal = evaluateTerm(comp.right, tup, schema)
    comp.op match {
      case CmpOp.Eq => leftVal == rightVal
      case CmpOp.Neq => leftVal != rightVal
      case CmpOp.Lt => leftVal.compare(rightVal) < 0
      case CmpOp.Lte => leftVal.compare(rightVal) <= 0
      case CmpOp.Gt => leftVal.compare(rightVal) > 0
      case CmpOp.Gte => leftVal.compare(rightVal) >= 0
    }
  }

  private def evaluateTerm(term: Term, tup: Tuple, schema: Array[String]): Datum = {
    term match {
      case Term.Var(name) =>
        val idx = schema.indexOf(name)
        if (idx >= 0 && idx < tup.arity) tup(idx) else Datum.NullVal
      case Term.Const(d) => d
      case Term.BinaryExpr(op, left, right) =>
        val lv = evaluateTerm(left, tup, schema).asLong
        val rv = evaluateTerm(right, tup, schema).asLong
        val res = op match {
          case BinOp.Add => lv + rv
          case BinOp.Sub => lv - rv
          case BinOp.Mul => lv * rv
          case BinOp.Div => if (rv != 0L) lv / rv else 0L
          case BinOp.Mod => if (rv != 0L) lv % rv else 0L
        }
        Datum.I64(res)
    }
  }
}
