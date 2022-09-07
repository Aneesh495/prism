package prism.datalog.parser

import prism.core.data.Datum
import prism.datalog.ast._
import scala.collection.mutable.ListBuffer

class ParseException(message: String, line: Int, column: Int)
  extends RuntimeException(s"Parse error at line $line, column $column: $message")

final class Parser(val tokens: List[Token]) {
  private var pos = 0

  private def current: Token = {
    if (pos < tokens.length) tokens(pos)
    else tokens.last
  }

  private def peekType: TokenType = current.tokenType

  private def advance(): Token = {
    val t = current
    if (pos < tokens.length) pos += 1
    t
  }

  private def expect(expected: TokenType): Token = {
    val t = advance()
    if (t.tokenType != expected) {
      throw new ParseException(s"Expected $expected, found ${t.tokenType}", t.line, t.column)
    }
    t
  }

  def parseProgram(): Program = {
    val facts = ListBuffer[Fact]()
    val rules = ListBuffer[Rule]()
    val queries = ListBuffer[String]()

    while (peekType != TokenType.EOF) {
      peekType match {
        case TokenType.Identifier(id) =>
          val clause = parseClause()
          clause match {
            case Left(f) => facts += f
            case Right(r) => rules += r
          }
        case TokenType.Turnstile =>
          // Directive or query: :- query(X).
          advance()
          val qAtom = parsePositiveAtom()
          expect(TokenType.Dot)
          queries += qAtom.predicate
        case other =>
          val t = advance()
          throw new ParseException(s"Unexpected token $other at start of statement", t.line, t.column)
      }
    }

    Program(facts.toList, rules.toList, queries.toList)
  }

  private def parseClause(): Either[Fact, Rule] = {
    val head = parsePositiveAtom()
    peekType match {
      case TokenType.Dot =>
        advance()
        // Ground fact if all terms are constants
        val isGround = head.terms.forall {
          case Term.Const(_) => true
          case _ => false
        }
        if (isGround) {
          val values = head.terms.map {
            case Term.Const(d) => d
            case _ => Datum.NullVal
          }
          Left(Fact(head.predicate, values))
        } else {
          // Rule with empty body
          Right(Rule(head, Nil))
        }

      case TokenType.Turnstile =>
        advance()
        val body = parseBody()
        expect(TokenType.Dot)
        Right(Rule(head, body))

      case other =>
        val t = current
        throw new ParseException(s"Expected '.' or ':-' after head atom, got $other", t.line, t.column)
    }
  }

  private def parseBody(): List[Literal] = {
    val literals = ListBuffer[Literal]()
    literals += parseLiteral()
    while (peekType == TokenType.Comma) {
      advance() // consume comma
      literals += parseLiteral()
    }
    literals.toList
  }

  private def parseLiteral(): Literal = {
    peekType match {
      case TokenType.KwNot =>
        advance()
        val atom = parsePositiveAtom()
        NegatedAtom(atom.predicate, atom.terms)

      case TokenType.Variable(resVar) =>
        // Could be comparison X = Y, X > Y, or aggregation Res = count<Y>(atom)
        val varTerm = Term.Var(resVar)
        advance()
        peekType match {
          case TokenType.Equals =>
            advance()
            peekType match {
              case TokenType.KwCount | TokenType.KwSum | TokenType.KwMin | TokenType.KwMax =>
                parseAggregation(resVar)
              case _ =>
                val right = parseTerm()
                ComparisonLiteral(CmpOp.Eq, varTerm, right)
            }
          case TokenType.NotEquals =>
            advance()
            ComparisonLiteral(CmpOp.Neq, varTerm, parseTerm())
          case TokenType.Less =>
            advance()
            ComparisonLiteral(CmpOp.Lt, varTerm, parseTerm())
          case TokenType.LessEquals =>
            advance()
            ComparisonLiteral(CmpOp.Lte, varTerm, parseTerm())
          case TokenType.Greater =>
            advance()
            ComparisonLiteral(CmpOp.Gt, varTerm, parseTerm())
          case TokenType.GreaterEquals =>
            advance()
            ComparisonLiteral(CmpOp.Gte, varTerm, parseTerm())
          case other =>
            throw new ParseException(s"Unexpected operator after variable $resVar: $other", current.line, current.column)
        }

      case TokenType.Identifier(pred) =>
        parsePositiveAtom()

      case other =>
        val t = current
        throw new ParseException(s"Expected literal, found $other", t.line, t.column)
    }
  }

  private def parseAggregation(resultVar: String): AggLiteral = {
    val fnToken = advance()
    val fnName = fnToken.tokenType match {
      case TokenType.KwCount => "count"
      case TokenType.KwSum => "sum"
      case TokenType.KwMin => "min"
      case TokenType.KwMax => "max"
      case _ => throw new ParseException(s"Expected agg function, got ${fnToken.tokenType}", fnToken.line, fnToken.column)
    }
    expect(TokenType.Less)
    val targetVarToken = advance()
    val targetVar = targetVarToken.tokenType match {
      case TokenType.Variable(name) => name
      case other => throw new ParseException(s"Expected target variable in aggregation, got $other", targetVarToken.line, targetVarToken.column)
    }
    expect(TokenType.Greater)
    expect(TokenType.LParen)
    val innerAtom = parsePositiveAtom()
    expect(TokenType.RParen)
    AggLiteral(resultVar, fnName, targetVar, innerAtom)
  }

  private def parsePositiveAtom(): PositiveAtom = {
    val predToken = advance()
    val predicateName = predToken.tokenType match {
      case TokenType.Identifier(name) => name
      case other => throw new ParseException(s"Expected predicate identifier, got $other", predToken.line, predToken.column)
    }
    expect(TokenType.LParen)
    val terms = ListBuffer[Term]()
    if (peekType != TokenType.RParen) {
      terms += parseTerm()
      while (peekType == TokenType.Comma) {
        advance()
        terms += parseTerm()
      }
    }
    expect(TokenType.RParen)
    PositiveAtom(predicateName, terms.toList)
  }

  private def parseTerm(): Term = {
    var left = parseFactor()
    while (peekType == TokenType.Plus || peekType == TokenType.Minus) {
      val op = peekType match {
        case TokenType.Plus => BinOp.Add
        case TokenType.Minus => BinOp.Sub
        case _ => ???
      }
      advance()
      val right = parseFactor()
      left = Term.BinaryExpr(op, left, right)
    }
    left
  }

  private def parseFactor(): Term = {
    var left = parsePrimary()
    while (peekType == TokenType.Star || peekType == TokenType.Slash || peekType == TokenType.Percent) {
      val op = peekType match {
        case TokenType.Star => BinOp.Mul
        case TokenType.Slash => BinOp.Div
        case TokenType.Percent => BinOp.Mod
        case _ => ???
      }
      advance()
      val right = parsePrimary()
      left = Term.BinaryExpr(op, left, right)
    }
    left
  }

  private def parsePrimary(): Term = {
    val t = advance()
    t.tokenType match {
      case TokenType.Variable(name) => Term.Var(name)
      case TokenType.Number(v) => Term.Const(Datum.I64(v))
      case TokenType.Decimal(v) => Term.Const(Datum.F64(v))
      case TokenType.StrLit(v) => Term.Const(Datum.Str(v))
      case TokenType.SymLit(v) => Term.Const(Datum.Sym(v))
      case TokenType.LParen =>
        val inner = parseTerm()
        expect(TokenType.RParen)
        inner
      case other =>
        throw new ParseException(s"Expected term, found $other", t.line, t.column)
    }
  }
}

object Parser {
  def parse(source: String): Program = {
    val tokens = new Lexer(source).tokenizeAll()
    new Parser(tokens).parseProgram()
  }
}
