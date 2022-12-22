package prism.sql

import prism.core.data.Datum
import prism.datalog.ast._

/**
 * SQL-to-Datalog transpiler.
 * Lowers relational SQL SELECT queries with joins, filters, and projections
 * into formal Horn clause Datalog rules for differential dataflow evaluation.
 */
object SqlToDatalog {

  type TableSchemas = Map[String, List[String]]

  def translate(
    targetPredicate: String,
    query: SqlQuery,
    schemas: TableSchemas
  ): Rule = {
    val bodyLiterals = collection.mutable.ListBuffer[Literal]()
    val varBindings = collection.mutable.Map[String, String]() // "table.col" or "col" -> Datalog variable name

    // Helper to generate capitalized variable name
    def varFor(tableOpt: Option[String], col: String): String = {
      val key = tableOpt.map(t => s"$t.$col").getOrElse(col)
      varBindings.getOrElseUpdate(key, {
        val cleanCol = col.capitalize
        tableOpt match {
          case Some(t) => s"${t.capitalize}_$cleanCol"
          case None => cleanCol
        }
      })
    }

    // 1. Process table sources
    def processSource(source: TableSource): Unit = source match {
      case TableSource.Table(name, alias) =>
        val effName = alias.getOrElse(name)
        val cols = schemas.getOrElse(name,
          throw new IllegalArgumentException(s"Unknown schema for table '$name'")
        )
        val termVars = cols.map { col =>
          val v = varFor(Some(effName), col)
          // Also alias without table prefix if unambiguous
          if (!varBindings.contains(col)) varBindings(col) = v
          Term.Var(v)
        }
        bodyLiterals += PositiveAtom(name, termVars)

      case TableSource.InnerJoin(left, right, condition) =>
        processSource(left)
        processSource(right)
        processCondition(condition)
    }

    def toCmpOp(op: String): CmpOp = op match {
      case "=" => CmpOp.Eq
      case "<>" | "!=" => CmpOp.Neq
      case "<" => CmpOp.Lt
      case "<=" => CmpOp.Lte
      case ">" => CmpOp.Gt
      case ">=" => CmpOp.Gte
      case _ => throw new IllegalArgumentException(s"Unknown comparison operator: $op")
    }

    def toBinOp(op: String): BinOp = op match {
      case "+" => BinOp.Add
      case "-" => BinOp.Sub
      case "*" => BinOp.Mul
      case "/" => BinOp.Div
      case "%" => BinOp.Mod
      case _ => throw new IllegalArgumentException(s"Unknown arithmetic operator: $op")
    }

    // 2. Process conditions (WHERE and ON)
    def processCondition(cond: SqlExpr): Unit = cond match {
      case SqlExpr.BinaryOp("=", SqlExpr.ColumnRef(c1, t1), SqlExpr.ColumnRef(c2, t2)) =>
        // Equi-join unification: unify variables
        val v1 = varFor(t1, c1)
        val v2 = varFor(t2, c2)
        if (v1 != v2) {
          bodyLiterals += ComparisonLiteral(CmpOp.Eq, Term.Var(v1), Term.Var(v2))
        }

      case SqlExpr.BinaryOp(op, left, right) if Set("=", "<>", "<", "<=", ">", ">=").contains(op) =>
        val leftTerm = exprToTerm(left)
        val rightTerm = exprToTerm(right)
        bodyLiterals += ComparisonLiteral(toCmpOp(op), leftTerm, rightTerm)

      case SqlExpr.BinaryOp("AND", left, right) =>
        processCondition(left)
        processCondition(right)

      case other =>
        // Other boolean filters
        ()
    }

    def exprToTerm(expr: SqlExpr): Term = expr match {
      case SqlExpr.ColumnRef(col, tbl) =>
        Term.Var(varFor(tbl, col))
      case SqlExpr.Literal(d) =>
        Term.Const(d)
      case SqlExpr.BinaryOp(op, l, r) if Set("+", "-", "*", "/").contains(op) =>
        Term.BinaryExpr(toBinOp(op), exprToTerm(l), exprToTerm(r))
      case other =>
        throw new IllegalArgumentException(s"Unsupported SQL expression in condition: $other")
    }

    processSource(query.from)
    query.where.foreach(processCondition)

    // 3. Process head projections
    val headTerms = query.selectItems.flatMap {
      case SelectItem.Star =>
        // Project all bound variables
        varBindings.values.toList.sorted.map(Term.Var.apply)
      case SelectItem.Expression(SqlExpr.ColumnRef(col, tbl), _) =>
        List(Term.Var(varFor(tbl, col)))
      case SelectItem.Expression(SqlExpr.Literal(d), _) =>
        List(Term.Const(d))
      case SelectItem.Expression(bin @ SqlExpr.BinaryOp(op, _, _), _) if Set("+", "-", "*", "/").contains(op) =>
        List(exprToTerm(bin))
      case other =>
        throw new IllegalArgumentException(s"Unsupported projection item: $other")
    }

    Rule(PositiveAtom(targetPredicate, headTerms), bodyLiterals.toList)
  }
}
