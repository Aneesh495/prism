package prism.datalog.analysis

import prism.datalog.ast._
import scala.collection.mutable

/**
 * Adornment representing bound ('b') vs free ('f') status of atom arguments.
 */
final case class Adornment(pattern: String) {
  def length: Int = pattern.length
  def isBound(idx: Int): Boolean = idx < pattern.length && pattern.charAt(idx) == 'b'
  def isFree(idx: Int): Boolean = !isBound(idx)
  override def toString: String = pattern
}

/**
 * Magic Sets query rewriting engine for goal-directed recursive query optimization.
 * Transforms Datalog programs using Sideways Information Passing (SIP) to prune
 * search spaces and avoid redundant relation materialization.
 */
object MagicSets {

  def rewrite(program: Program, queryAtom: PositiveAtom, boundIndices: Set[Int]): Program = {
    // 1. Compute adornment for the query
    val queryPattern = (0 until queryAtom.arity).map { i =>
      if (boundIndices.contains(i)) 'b' else 'f'
    }.mkString
    val queryAdornment = Adornment(queryPattern)
    val adornedQueryPred = s"${queryAtom.predicate}_$queryAdornment"

    val adornedRules = mutable.ListBuffer[Rule]()
    val magicRules = mutable.ListBuffer[Rule]()
    val processedAdornments = mutable.Set[(String, Adornment)]()
    val queue = mutable.Queue[(String, Adornment)]()

    queue.enqueue((queryAtom.predicate, queryAdornment))
    processedAdornments += ((queryAtom.predicate, queryAdornment))

    // 2. Process rules breadth-first by adornment
    while (queue.nonEmpty) {
      val (pred, adornment) = queue.dequeue()
      val rulesForPred = program.rules.filter(_.head.predicate == pred)

      for (rule <- rulesForPred) {
        val (adornedRule, generatedMagicRules, nextGoals) = adornRule(rule, adornment)
        adornedRules += adornedRule
        magicRules ++= generatedMagicRules

        for (goal <- nextGoals) {
          if (!processedAdornments.contains(goal)) {
            processedAdornments += goal
            queue.enqueue(goal)
          }
        }
      }
    }

    // 3. Create seed magic fact from query constants
    val magicPredName = s"magic_${queryAtom.predicate}_$queryAdornment"
    val seedTerms = queryAtom.terms.zipWithIndex.collect {
      case (Term.Const(d), idx) if boundIndices.contains(idx) => d
    }
    val seedFacts = if (seedTerms.nonEmpty) {
      List(Fact(magicPredName, seedTerms))
    } else {
      Nil
    }

    // 4. Assemble rewritten program
    Program(
      facts = program.facts ++ seedFacts,
      rules = (magicRules ++ adornedRules).toList,
      queries = List(adornedQueryPred)
    )
  }

  private def adornRule(
    rule: Rule,
    headAdornment: Adornment
  ): (Rule, List[Rule], List[(String, Adornment)]) = {
    val boundInHead = rule.head.terms.zipWithIndex.collect {
      case (Term.Var(name), idx) if headAdornment.isBound(idx) => name
    }.toSet

    var currentlyBound = boundInHead
    val adornedBody = mutable.ListBuffer[Literal]()
    val magicRules = mutable.ListBuffer[Rule]()
    val nextGoals = mutable.ListBuffer[(String, Adornment)]()

    // Prefix with magic predicate if head has bound arguments
    val magicHeadPred = s"magic_${rule.head.predicate}_$headAdornment"
    val magicHeadArgs = rule.head.terms.zipWithIndex.collect {
      case (term, idx) if headAdornment.isBound(idx) => term
    }
    if (magicHeadArgs.nonEmpty) {
      adornedBody += PositiveAtom(magicHeadPred, magicHeadArgs)
    }

    for (lit <- rule.body) {
      lit match {
        case PositiveAtom(bodyPred, terms) =>
          // Determine adornment for body atom based on currently bound variables
          val pattern = terms.map {
            case Term.Var(v) if currentlyBound.contains(v) => 'b'
            case Term.Const(_) => 'b'
            case _ => 'f'
          }.mkString
          val bodyAdornment = Adornment(pattern)
          val adornedBodyPred = s"${bodyPred}_$bodyAdornment"
          adornedBody += PositiveAtom(adornedBodyPred, terms)
          nextGoals += ((bodyPred, bodyAdornment))

          // Generate magic rule for recursive dependencies
          val magicTargetArgs = terms.zipWithIndex.collect {
            case (term, idx) if bodyAdornment.isBound(idx) => term
          }
          if (magicTargetArgs.nonEmpty) {
            val magicTargetPred = s"magic_${bodyPred}_$bodyAdornment"
            val magicRuleHead = PositiveAtom(magicTargetPred, magicTargetArgs)
            val magicRuleBody = adornedBody.init.toList
            if (magicRuleBody.nonEmpty) {
              magicRules += Rule(magicRuleHead, magicRuleBody)
            }
          }

          // Variables from this atom now become bound for subsequent atoms
          currentlyBound ++= terms.flatMap(_.variables)

        case other =>
          adornedBody += other
          currentlyBound ++= other.variables
      }
    }

    val adornedHeadPred = s"${rule.head.predicate}_$headAdornment"
    val finalAdornedRule = Rule(PositiveAtom(adornedHeadPred, rule.head.terms), adornedBody.toList)
    (finalAdornedRule, magicRules.toList, nextGoals.toList)
  }
}
