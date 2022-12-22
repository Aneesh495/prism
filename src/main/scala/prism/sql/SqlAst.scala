package prism.sql

import prism.core.data.Datum

sealed trait SqlExpr
object SqlExpr {
  final case class ColumnRef(columnName: String, tableName: Option[String] = None) extends SqlExpr
  final case class Literal(datum: Datum) extends SqlExpr
  final case class BinaryOp(op: String, left: SqlExpr, right: SqlExpr) extends SqlExpr
  final case class UnaryOp(op: String, expr: SqlExpr) extends SqlExpr
  final case class FunctionCall(name: String, args: List[SqlExpr]) extends SqlExpr
}

sealed trait SelectItem
object SelectItem {
  case object Star extends SelectItem
  final case class Expression(expr: SqlExpr, alias: Option[String] = None) extends SelectItem
}

sealed trait TableSource
object TableSource {
  final case class Table(name: String, alias: Option[String] = None) extends TableSource
  final case class InnerJoin(left: TableSource, right: TableSource, condition: SqlExpr) extends TableSource
}

final case class SqlQuery(
  selectItems: List[SelectItem],
  from: TableSource,
  where: Option[SqlExpr] = None,
  groupBy: List[SqlExpr] = Nil,
  having: Option[SqlExpr] = None
)
