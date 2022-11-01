package prism.datalog

import munit.FunSuite
import prism.core.data.{Datum, Tuple}
import prism.datalog.analysis.{Safety, SafetyException, Stratifier, StratificationException, MagicSets}
import prism.datalog.ast._
import prism.datalog.compiler.Compiler
import prism.datalog.parser.{Lexer, Parser, TokenType}

class DatalogSuite extends FunSuite {

  test("Lexer tokenizes identifiers, variables, numbers, strings, and operators") {
    val src =
      """
        |% Comment line
        |reach(X, Y) :- edge(X, Y).
        |path(A, B, 10.5) :- cost(A, B, "fast"), A != B.
      """.stripMargin

    val tokens = new Lexer(src).tokenizeAll()
    val tokenTypes = tokens.map(_.tokenType)

    assert(tokenTypes.contains(TokenType.Identifier("reach")))
    assert(tokenTypes.contains(TokenType.Variable("X")))
    assert(tokenTypes.contains(TokenType.Variable("Y")))
    assert(tokenTypes.contains(TokenType.Turnstile))
    assert(tokenTypes.contains(TokenType.Dot))
    assert(tokenTypes.contains(TokenType.Decimal(10.5)))
    assert(tokenTypes.contains(TokenType.StrLit("fast")))
    assert(tokenTypes.contains(TokenType.NotEquals))
  }

  test("Parser parses facts, rules, and queries") {
    val progText =
      """
        |edge(1, 2).
        |edge(2, 3).
        |edge(3, 4).
        |path(X, Y) :- edge(X, Y).
        |path(X, Y) :- path(X, Z), edge(Z, Y).
        |unreachable(X, Y) :- node(X), node(Y), not path(X, Y).
      """.stripMargin

    val program = Parser.parse(progText)
    assertEquals(program.facts.length, 3)
    assertEquals(program.rules.length, 3)
    assertEquals(program.rules.head.head.predicate, "path")
  }

  test("Safety checker rejects unbound head and negated variables") {
    // Head variable Z is unbound
    val unsafeHead = Rule(
      PositiveAtom("p", List(Term.Var("X"), Term.Var("Z"))),
      List(PositiveAtom("q", List(Term.Var("X"))))
    )
    intercept[SafetyException] {
      Safety.validate(unsafeHead)
    }

    // Negated variable Y is unbound
    val unsafeNeg = Rule(
      PositiveAtom("p", List(Term.Var("X"))),
      List(PositiveAtom("q", List(Term.Var("X"))), NegatedAtom("r", List(Term.Var("Y"))))
    )
    intercept[SafetyException] {
      Safety.validate(unsafeNeg)
    }
  }

  test("Stratifier detects unstratifiable circular negation") {
    // p(X) :- not q(X).
    // q(X) :- not p(X).
    val circularProg = Program(
      facts = Nil,
      rules = List(
        Rule(PositiveAtom("p", List(Term.Var("X"))), List(PositiveAtom("base", List(Term.Var("X"))), NegatedAtom("q", List(Term.Var("X"))))),
        Rule(PositiveAtom("q", List(Term.Var("X"))), List(PositiveAtom("base", List(Term.Var("X"))), NegatedAtom("p", List(Term.Var("X")))))
      )
    )

    intercept[StratificationException] {
      Stratifier.stratify(circularProg)
    }
  }

  test("Stratifier correctly partitions stratified negation into sequential strata") {
    // stratum 0: edge, path
    // stratum 1: non_path(X, Y) :- node(X), node(Y), not path(X, Y).
    val stratifiedProg = Program(
      facts = List(Fact("edge", List(Datum.I64(1), Datum.I64(2)))),
      rules = List(
        Rule(PositiveAtom("path", List(Term.Var("X"), Term.Var("Y"))), List(PositiveAtom("edge", List(Term.Var("X"), Term.Var("Y"))))),
        Rule(PositiveAtom("non_path", List(Term.Var("X"), Term.Var("Y"))), List(
          PositiveAtom("node", List(Term.Var("X"))),
          PositiveAtom("node", List(Term.Var("Y"))),
          NegatedAtom("path", List(Term.Var("X"), Term.Var("Y")))
        ))
      )
    )

    val strata = Stratifier.stratify(stratifiedProg)
    assertEquals(strata.length, 2)
    assertEquals(strata(0).head.head.predicate, "path")
    assertEquals(strata(1).head.head.predicate, "non_path")
  }

  test("Compiler executes transitive closure with dynamic fact additions and retractions") {
    val src =
      """
        |edge(1, 2).
        |edge(2, 3).
        |edge(3, 4).
        |tc(X, Y) :- edge(X, Y).
        |tc(X, Y) :- tc(X, Z), edge(Z, Y).
      """.stripMargin

    val prog = Parser.parse(src)
    val compiled = Compiler.compile(prog)
    val initial = compiled.run()

    val tcSet = initial("tc")
    // Pairs: (1,2), (2,3), (3,4), (1,3), (2,4), (1,4) = 6 pairs
    assertEquals(tcSet.size, 6)
    assert(tcSet.contains(Tuple.of(1L, 4L)))

    // Dynamically insert an edge: 4 -> 5
    compiled.insertFact("edge", Tuple.of(4L, 5L))
    val updatedTc = compiled.query("tc")
    // Now reachable includes (1,5), (2,5), (3,5), (4,5) = total 10 pairs
    assertEquals(updatedTc.size, 10)
    assert(updatedTc.contains(Tuple.of(1L, 5L)))

    // Retract edge 4 -> 5
    compiled.retractFact("edge", Tuple.of(4L, 5L))
    val retractedTc = compiled.query("tc")
    assertEquals(retractedTc.size, 6)
    assert(!retractedTc.contains(Tuple.of(1L, 5L)))
  }
}
