package prism.sql

import prism.core.data.Datum

/**
 * Recursive-descent SQL parser translating token streams into SqlQuery AST.
 */
final class SqlParser(val tokens: List[SqlToken]) {
  private var pos = 0

  private def peek(): SqlToken = if (pos < tokens.length) tokens(pos) else null
  private def advance(): SqlToken = { val t = peek(); pos += 1; t }
  private def matchToken(expected: SqlToken): Boolean = {
    if (peek() == expected) { advance(); true } else false
  }
  private def expectToken(expected: SqlToken): Unit = {
    val t = advance()
    if (t != expected) {
      throw new IllegalArgumentException(s"Expected token $expected, found $t at position $pos")
    }
  }

  def parseQuery(): SqlQuery = {
    expectToken(SqlToken.Select)
    val selectItems = parseSelectList()
    expectToken(SqlToken.From)
    val tableSource = parseTableSource()

    val whereCond = if (matchToken(SqlToken.Where)) Some(parseExpression()) else None

    val groupByItems = if (matchToken(SqlToken.Group)) {
      expectToken(SqlToken.By)
      parseExpressionList()
    } else Nil

    val havingCond = if (matchToken(SqlToken.Having)) Some(parseExpression()) else None

    SqlQuery(selectItems, tableSource, whereCond, groupByItems, havingCond)
  }

  private def parseSelectList(): List[SelectItem] = {
    val items = collection.mutable.ListBuffer[SelectItem]()
    var continue = true
    while (continue) {
      if (matchToken(SqlToken.Star)) {
        items += SelectItem.Star
      } else {
        val expr = parseExpression()
        val alias = if (matchToken(SqlToken.As)) {
          peek() match {
            case SqlToken.Ident(name) => advance(); Some(name)
            case other => throw new IllegalArgumentException(s"Expected identifier after AS, got $other")
          }
        } else None
        items += SelectItem.Expression(expr, alias)
      }
      if (!matchToken(SqlToken.Comma)) {
        continue = false
      }
    }
    items.toList
  }

  private def parseTableSource(): TableSource = {
    var source: TableSource = peek() match {
      case SqlToken.Ident(tableName) =>
        advance()
        val alias = if (matchToken(SqlToken.As)) {
          peek() match {
            case SqlToken.Ident(a) => advance(); Some(a)
            case _ => None
          }
        } else None
        TableSource.Table(tableName, alias)
      case other =>
        throw new IllegalArgumentException(s"Expected table name in FROM clause, found $other")
    }

    while (peek() == SqlToken.Join || peek() == SqlToken.Inner) {
      if (peek() == SqlToken.Inner) advance()
      expectToken(SqlToken.Join)
      val rightSource = parseTableSource()
      expectToken(SqlToken.On)
      val condition = parseExpression()
      source = TableSource.InnerJoin(source, rightSource, condition)
    }

    source
  }

  private def parseExpressionList(): List[SqlExpr] = {
    val list = collection.mutable.ListBuffer[SqlExpr]()
    var continue = true
    while (continue) {
      list += parseExpression()
      if (!matchToken(SqlToken.Comma)) continue = false
    }
    list.toList
  }

  def parseExpression(): SqlExpr = parseOr()

  private def parseOr(): SqlExpr = {
    var left = parseAnd()
    while (matchToken(SqlToken.Or)) {
      val right = parseAnd()
      left = SqlExpr.BinaryOp("OR", left, right)
    }
    left
  }

  private def parseAnd(): SqlExpr = {
    var left = parseComparison()
    while (matchToken(SqlToken.And)) {
      val right = parseComparison()
      left = SqlExpr.BinaryOp("AND", left, right)
    }
    left
  }

  private def parseComparison(): SqlExpr = {
    var left = parseAddSub()
    val t = peek()
    t match {
      case SqlToken.Eq => advance(); SqlExpr.BinaryOp("=", left, parseAddSub())
      case SqlToken.Neq => advance(); SqlExpr.BinaryOp("<>", left, parseAddSub())
      case SqlToken.Lt => advance(); SqlExpr.BinaryOp("<", left, parseAddSub())
      case SqlToken.Lte => advance(); SqlExpr.BinaryOp("<=", left, parseAddSub())
      case SqlToken.Gt => advance(); SqlExpr.BinaryOp(">", left, parseAddSub())
      case SqlToken.Gte => advance(); SqlExpr.BinaryOp(">=", left, parseAddSub())
      case _ => left
    }
  }

  private def parseAddSub(): SqlExpr = {
    var left = parseMulDiv()
    var continue = true
    while (continue) {
      if (matchToken(SqlToken.Plus)) {
        left = SqlExpr.BinaryOp("+", left, parseMulDiv())
      } else if (matchToken(SqlToken.Minus)) {
        left = SqlExpr.BinaryOp("-", left, parseMulDiv())
      } else {
        continue = false
      }
    }
    left
  }

  private def parseMulDiv(): SqlExpr = {
    var left = parseUnary()
    var continue = true
    while (continue) {
      if (matchToken(SqlToken.Star)) {
        left = SqlExpr.BinaryOp("*", left, parseUnary())
      } else if (matchToken(SqlToken.Slash)) {
        left = SqlExpr.BinaryOp("/", left, parseUnary())
      } else {
        continue = false
      }
    }
    left
  }

  private def parseUnary(): SqlExpr = {
    if (matchToken(SqlToken.Not)) {
      SqlExpr.UnaryOp("NOT", parseUnary())
    } else if (matchToken(SqlToken.Minus)) {
      SqlExpr.UnaryOp("-", parseUnary())
    } else {
      parsePrimary()
    }
  }

  private def parsePrimary(): SqlExpr = {
    advance() match {
      case SqlToken.IntLit(v) => SqlExpr.Literal(Datum.I64(v))
      case SqlToken.FloatLit(v) => SqlExpr.Literal(Datum.F64(v))
      case SqlToken.StringLit(v) => SqlExpr.Literal(Datum.Str(v))
      case SqlToken.Ident(name) =>
        if (matchToken(SqlToken.Dot)) {
          // table.column
          peek() match {
            case SqlToken.Ident(col) =>
              advance()
              SqlExpr.ColumnRef(col, Some(name))
            case other =>
              throw new IllegalArgumentException(s"Expected column name after dot, got $other")
          }
        } else if (matchToken(SqlToken.LParen)) {
          // Function call
          val args = if (peek() == SqlToken.RParen) Nil else parseExpressionList()
          expectToken(SqlToken.RParen)
          SqlExpr.FunctionCall(name, args)
        } else {
          SqlExpr.ColumnRef(name, None)
        }
      case SqlToken.LParen =>
        val e = parseExpression()
        expectToken(SqlToken.RParen)
        e
      case other =>
        throw new IllegalArgumentException(s"Unexpected primary token: $other at position $pos")
    }
  }
}
