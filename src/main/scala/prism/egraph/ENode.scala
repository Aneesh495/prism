package prism.egraph

/**
 * High-level expression tree for term rewriting and extraction.
 */
sealed trait Expr extends Serializable {
  def size: Int
  def depth: Int
}

object Expr {
  final case class Var(name: String) extends Expr {
    def size: Int = 1
    def depth: Int = 1
    override def toString: String = name
  }

  final case class Const(value: Long) extends Expr {
    def size: Int = 1
    def depth: Int = 1
    override def toString: String = value.toString
  }

  final case class Op(name: String, args: List[Expr]) extends Expr {
    def size: Int = 1 + args.map(_.size).sum
    def depth: Int = 1 + (if (args.isEmpty) 0 else args.map(_.depth).max)
    override def toString: String = {
      if (args.isEmpty) name
      else if (args.length == 2 && !Character.isLetterOrDigit(name.charAt(0))) {
        s"(${args(0)} $name ${args(1)})"
      } else {
        s"$name(${args.mkString(", ")})"
      }
    }
  }

  def add(a: Expr, b: Expr): Expr = Op("+", List(a, b))
  def sub(a: Expr, b: Expr): Expr = Op("-", List(a, b))
  def mul(a: Expr, b: Expr): Expr = Op("*", List(a, b))
  def div(a: Expr, b: Expr): Expr = Op("/", List(a, b))
}

/**
 * Node in an equivalence graph (ENode).
 * Combines an operator symbol with child equivalence class pointers.
 */
final case class ENode(
  op: String,
  children: List[EClassId]
) extends Ordered[ENode] {

  def arity: Int = children.length

  /**
   * Returns a canonicalized copy of this node where all child class IDs
   * are replaced by their canonical representative in the union-find.
   */
  def canonicalize(uf: UnionFind): ENode = {
    val canonicalChildren = children.map(uf.find)
    if (canonicalChildren == children) this
    else ENode(op, canonicalChildren)
  }

  def compare(that: ENode): Int = {
    val opCmp = this.op.compareTo(that.op)
    if (opCmp != 0) opCmp
    else {
      val lenCmp = this.children.length.compare(that.children.length)
      if (lenCmp != 0) lenCmp
      else {
        var i = 0
        var diff = 0
        while (i < this.children.length && diff == 0) {
          diff = this.children(i).compare(that.children(i))
          i += 1
        }
        diff
      }
    }
  }

  override def toString: String = {
    if (children.isEmpty) op
    else s"$op(${children.mkString(", ")})"
  }
}
