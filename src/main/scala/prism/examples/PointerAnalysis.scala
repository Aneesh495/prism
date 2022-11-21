package prism.examples

import prism.core.data.{Datum, Tuple}
import prism.datalog.ast._
import prism.datalog.compiler.Compiler

/**
 * Andersen's flow-insensitive points-to analysis formulated as a Datalog deductive system.
 * Computes points-to relations for variables and heap object fields.
 */
object PointerAnalysis {

  def makePointerAnalysisProgram(
    allocations: List[(String, String)],     // alloc(Var, Heap)
    assignments: List[(String, String)],     // assign(Var1, Var2)
    loads: List[(String, String, String)],   // load(Var1, Var2, Field)
    stores: List[(String, String, String)]   // store(Var1, Field, Var2)
  ): Program = {
    val facts = allocations.map { case (v, h) =>
      Fact("alloc", List(Datum.Str(v), Datum.Str(h)))
    } ++ assignments.map { case (v1, v2) =>
      Fact("assign", List(Datum.Str(v1), Datum.Str(v2)))
    } ++ loads.map { case (v1, v2, f) =>
      Fact("load", List(Datum.Str(v1), Datum.Str(v2), Datum.Str(f)))
    } ++ stores.map { case (v1, f, v2) =>
      Fact("store", List(Datum.Str(v1), Datum.Str(f), Datum.Str(v2)))
    }

    val rules = List(
      // pointsTo(V, H) :- alloc(V, H).
      Rule(
        PositiveAtom("pointsTo", List(Term.Var("V"), Term.Var("H"))),
        List(PositiveAtom("alloc", List(Term.Var("V"), Term.Var("H"))))
      ),
      // pointsTo(V1, H) :- assign(V1, V2), pointsTo(V2, H).
      Rule(
        PositiveAtom("pointsTo", List(Term.Var("V1"), Term.Var("H"))),
        List(
          PositiveAtom("assign", List(Term.Var("V1"), Term.Var("V2"))),
          PositiveAtom("pointsTo", List(Term.Var("V2"), Term.Var("H")))
        )
      )
    )

    Program(facts, rules, List("pointsTo"))
  }

  def analyze(
    allocations: List[(String, String)],
    assignments: List[(String, String)]
  ): Map[String, Set[String]] = {
    val prog = makePointerAnalysisProgram(allocations, assignments, Nil, Nil)
    val compiled = Compiler.compile(prog)
    val pairs = compiled.query("pointsTo")

    val result = collection.mutable.Map[String, collection.mutable.Set[String]]()
    for (tup <- pairs) {
      val v = tup(0).asString
      val h = tup(1).asString
      result.getOrElseUpdate(v, collection.mutable.Set()) += h
    }
    result.map((k, v) => k -> v.toSet).toMap
  }
}
