package prism.egraph

import munit.FunSuite

class EGraphSuite extends FunSuite {

  test("UnionFind maintains disjoint sets with path compression and rank") {
    val uf = new UnionFind()
    val id0 = uf.makeSet()
    val id1 = uf.makeSet()
    val id2 = uf.makeSet()
    val id3 = uf.makeSet()

    assert(!uf.isEquivalent(id0, id1))

    val root01 = uf.union(id0, id1)
    assert(uf.isEquivalent(id0, id1))
    assert(!uf.isEquivalent(id0, id2))

    uf.union(id2, id3)
    uf.union(id1, id3)

    assert(uf.isEquivalent(id0, id2))
    assert(uf.isEquivalent(id1, id3))
  }

  test("EGraph hashconsing deduplicates identical ENodes") {
    val eg = new EGraph()
    val n1 = ENode("x", Nil)
    val n2 = ENode("x", Nil)

    val id1 = eg.add(n1)
    val id2 = eg.add(n2)

    assertEquals(id1, id2)
    assertEquals(eg.numClasses, 1)
  }

  test("EGraph rebuild restores congruence closure invariant") {
    // f(a) and f(b) with a == b => f(a) == f(b)
    val eg = new EGraph()
    val a = eg.add(ENode("a", Nil))
    val b = eg.add(ENode("b", Nil))

    val fa = eg.add(ENode("f", List(a)))
    val fb = eg.add(ENode("f", List(b)))

    assert(!eg.unionFind.isEquivalent(fa, fb))

    // Merge a and b
    eg.merge(a, b)
    // Rebuild congruence closure
    eg.rebuild()

    assert(eg.unionFind.isEquivalent(fa, fb))
  }

  test("EMatcher finds structural matches in EGraph") {
    val eg = new EGraph()
    import Expr._
    // (a + b) * c
    val expr = mul(add(Var("a"), Var("b")), Var("c"))
    val targetClass = eg.addExpr(expr)

    // Pattern: (?x + ?y) * ?z
    val pat = Pattern.op("*", Pattern.op("+", Pattern.v("x"), Pattern.v("y")), Pattern.v("z"))
    val matches = EMatcher.search(eg, pat)

    assertEquals(matches.length, 1)
    val m = matches.head
    assertEquals(m.matchedClass, targetClass)
    assert(m.subst.contains("x"))
    assert(m.subst.contains("y"))
    assert(m.subst.contains("z"))
  }

  test("SaturationEngine simplifies arithmetic identities") {
    import Expr._
    val x = Var("x")
    val zero = Const(0)
    // (x + 0) + 0 ==> x
    val expr = add(add(x, zero), zero)

    val eg = new EGraph()
    val id = eg.addExpr(expr)

    val engine = new SaturationEngine(Rewrite.standardArithmeticRules, maxIterations = 5)
    val report = engine.saturate(eg)

    val extractor = new Extractor(eg)
    val simplified = extractor.extract(id)

    assertEquals(simplified, x)
  }

  test("Proof generator constructs markdown equivalence certificate") {
    import Expr._
    val x = Var("x")
    val zero = Const(0)
    val expr = add(x, zero)

    val eg = new EGraph()
    val id = eg.addExpr(expr)

    val engine = new SaturationEngine(Rewrite.standardArithmeticRules, maxIterations = 5)
    val report = engine.saturate(eg)

    val proof = ProofExplainer.explain(eg, expr, x, report)
    assert(proof.isValid)
    val md = proof.formatMarkdown
    assert(md.contains("Equivalence Proof"))
  }
}
