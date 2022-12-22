package prism.sql

import munit.FunSuite
import prism.core.data.{Datum, Tuple}
import prism.datalog.ast.{Fact, Program}
import prism.datalog.compiler.Compiler

class SqlSuite extends FunSuite {

  test("SqlLexer tokenizes SQL queries into structured tokens") {
    val query = "SELECT name, age + 1 FROM users WHERE age >= 18"
    val lexer = new SqlLexer(query)
    val tokens = lexer.tokenize()

    assert(tokens.contains(SqlToken.Select))
    assert(tokens.contains(SqlToken.From))
    assert(tokens.contains(SqlToken.Where))
    assert(tokens.contains(SqlToken.Gte))
    assert(tokens.contains(SqlToken.Ident("users")))
  }

  test("SqlParser parses single table queries with arithmetic and filters") {
    val queryStr = "SELECT name, age FROM users WHERE age > 21"
    val lexer = new SqlLexer(queryStr)
    val parser = new SqlParser(lexer.tokenize())
    val ast = parser.parseQuery()

    assertEquals(ast.selectItems.length, 2)
    assert(ast.where.isDefined)
    ast.from match {
      case TableSource.Table(name, _) => assertEquals(name, "users")
      case _ => fail("Expected single table source")
    }
  }

  test("SqlParser parses inner join queries with aliases") {
    val queryStr = "SELECT u.name, o.amount FROM users AS u JOIN orders AS o ON u.id = o.userId WHERE o.amount > 100"
    val lexer = new SqlLexer(queryStr)
    val parser = new SqlParser(lexer.tokenize())
    val ast = parser.parseQuery()

    assertEquals(ast.selectItems.length, 2)
    ast.from match {
      case TableSource.InnerJoin(left, right, cond) =>
        left match {
          case TableSource.Table(n1, a1) =>
            assertEquals(n1, "users")
            assertEquals(a1, Some("u"))
          case _ => fail("Expected table source")
        }
        right match {
          case TableSource.Table(n2, a2) =>
            assertEquals(n2, "orders")
            assertEquals(a2, Some("o"))
          case _ => fail("Expected table source")
        }
      case _ => fail("Expected inner join")
    }
  }

  test("SqlToDatalog transpiles SQL query to Datalog and executes on differential engine") {
    val schemas: Map[String, List[String]] = Map(
      "users" -> List("id", "name"),
      "orders" -> List("orderId", "userId", "amount")
    )

    val queryStr = "SELECT users.name, orders.amount FROM users JOIN orders ON users.id = orders.userId WHERE orders.amount > 50"
    val tokens = new SqlLexer(queryStr).tokenize()
    val sqlQuery = new SqlParser(tokens).parseQuery()

    val datalogRule = SqlToDatalog.translate("highValueOrders", sqlQuery, schemas)
    assertEquals(datalogRule.head.predicate, "highValueOrders")

    // Facts
    val factUsers = List(
      Fact("users", List(Datum.I64(1L), Datum.Str("Alice"))),
      Fact("users", List(Datum.I64(2L), Datum.Str("Bob")))
    )
    val factOrders = List(
      Fact("orders", List(Datum.I64(101L), Datum.I64(1L), Datum.I64(120L))), // Alice: 120 (> 50)
      Fact("orders", List(Datum.I64(102L), Datum.I64(2L), Datum.I64(30L)))   // Bob: 30 (not > 50)
    )

    val program = Program(factUsers ++ factOrders, List(datalogRule))
    val compiled = Compiler.compile(program)
    val results = compiled.query("highValueOrders")

    assertEquals(results.size, 1)
    val expected = Tuple(Datum.Str("Alice"), Datum.I64(120L))
    assert(results.contains(expected))
  }
}
