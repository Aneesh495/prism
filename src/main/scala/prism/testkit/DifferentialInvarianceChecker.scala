package prism.testkit

import prism.core.data.{Datum, Tuple}
import prism.datalog.ast.{Fact, Program, Rule}
import prism.datalog.compiler.Compiler

/**
 * Property-based differential invariance verification:
 * Validates that for any base database D and mutation stream Delta:
 * Q(D union Delta) evaluated from scratch exactly matches
 * Q(D) oplus Delta Q evaluated differentially!
 */
object DifferentialInvarianceChecker {

  def verifyInvariance(
    rules: List[Rule],
    initialFacts: List[Fact],
    mutationFacts: List[Fact],
    targetPredicate: String
  ): Boolean = {
    // 1. Differential execution: run initial, then insert mutation
    val diffProg = Program(initialFacts, rules, List(targetPredicate))
    val diffCompiled = Compiler.compile(diffProg)
    diffCompiled.run()

    for (mf <- mutationFacts) {
      diffCompiled.insertFact(mf.predicate, Tuple.fromSeq(mf.values), mf.weight)
    }
    val diffResult = diffCompiled.query(targetPredicate)

    // 2. Scratch execution: compile combined program from scratch
    val scratchProg = Program(initialFacts ++ mutationFacts, rules, List(targetPredicate))
    val scratchCompiled = Compiler.compile(scratchProg)
    val scratchResults = scratchCompiled.run()
    val scratchResult = scratchResults.getOrElse(targetPredicate, Set.empty)

    // 3. Invariance assert
    diffResult == scratchResult
  }
}
