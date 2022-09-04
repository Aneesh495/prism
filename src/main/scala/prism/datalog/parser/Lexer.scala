package prism.datalog.parser

import prism.core.data.Datum

/**
 * Token types emitted by the lexical scanner.
 */
enum TokenType {
  case Identifier(name: String)
  case Variable(name: String)
  case Number(value: Long)
  case Decimal(value: Double)
  case StrLit(value: String)
  case SymLit(value: String)
  // Punctuation & Delimiters
  case Dot           // .
  case Comma         // ,
  case LParen        // (
  case RParen        // )
  case Turnstile     // :-
  // Operators
  case Equals        // =
  case NotEquals     // !=
  case Less          // <
  case LessEquals    // <=
  case Greater       // >
  case GreaterEquals // >=
  case Plus          // +
  case Minus         // -
  case Star          // *
  case Slash         // /
  case Percent       // %
  // Keywords
  case KwNot
  case KwCount
  case KwSum
  case KwMin
  case KwMax
  case EOF
}

final case class Token(tokenType: TokenType, line: Int, column: Int)

final class Lexer(val input: String) {
  private val len = input.length
  private var pos = 0
  private var line = 1
  private var col = 1

  private def peek(): Char = if (pos < len) input.charAt(pos) else '\u0000'
  private def peekNext(): Char = if (pos + 1 < len) input.charAt(pos + 1) else '\u0000'

  private def advance(): Char = {
    val ch = peek()
    pos += 1
    if (ch == '\n') {
      line += 1
      col = 1
    } else {
      col += 1
    }
    ch
  }

  def nextToken(): Token = {
    skipWhitespaceAndComments()
    val startLine = line
    val startCol = col

    if (pos >= len) {
      Token(TokenType.EOF, startLine, startCol)
    } else {
      val ch = peek()
      if (ch == ':' && peekNext() == '-') {
        advance()
        advance()
        Token(TokenType.Turnstile, startLine, startCol)
      } else if (ch == ':' && Character.isJavaIdentifierStart(peekNext())) {
        advance()
        val sym = scanIdentifier()
        Token(TokenType.SymLit(sym), startLine, startCol)
      } else if (ch == '!' && peekNext() == '=') {
        advance()
        advance()
        Token(TokenType.NotEquals, startLine, startCol)
      } else if (ch == '<' && peekNext() == '=') {
        advance()
        advance()
        Token(TokenType.LessEquals, startLine, startCol)
      } else if (ch == '>' && peekNext() == '=') {
        advance()
        advance()
        Token(TokenType.GreaterEquals, startLine, startCol)
      } else if (ch == '.') {
        advance()
        Token(TokenType.Dot, startLine, startCol)
      } else if (ch == ',') {
        advance()
        Token(TokenType.Comma, startLine, startCol)
      } else if (ch == '(') {
        advance()
        Token(TokenType.LParen, startLine, startCol)
      } else if (ch == ')') {
        advance()
        Token(TokenType.RParen, startLine, startCol)
      } else if (ch == '=') {
        advance()
        Token(TokenType.Equals, startLine, startCol)
      } else if (ch == '<') {
        advance()
        Token(TokenType.Less, startLine, startCol)
      } else if (ch == '>') {
        advance()
        Token(TokenType.Greater, startLine, startCol)
      } else if (ch == '+') {
        advance()
        Token(TokenType.Plus, startLine, startCol)
      } else if (ch == '-') {
        advance()
        Token(TokenType.Minus, startLine, startCol)
      } else if (ch == '*') {
        advance()
        Token(TokenType.Star, startLine, startCol)
      } else if (ch == '/') {
        advance()
        Token(TokenType.Slash, startLine, startCol)
      } else if (ch == '%') {
        advance()
        Token(TokenType.Percent, startLine, startCol)
      } else if (ch == '"') {
        Token(scanString(), startLine, startCol)
      } else if (Character.isDigit(ch)) {
        Token(scanNumber(), startLine, startCol)
      } else if (Character.isJavaIdentifierStart(ch) || ch == '_') {
        val word = scanIdentifier()
        word match {
          case "not" => Token(TokenType.KwNot, startLine, startCol)
          case "count" => Token(TokenType.KwCount, startLine, startCol)
          case "sum" => Token(TokenType.KwSum, startLine, startCol)
          case "min" => Token(TokenType.KwMin, startLine, startCol)
          case "max" => Token(TokenType.KwMax, startLine, startCol)
          case _ =>
            if (Character.isUpperCase(word.charAt(0)) || word.startsWith("_")) {
              Token(TokenType.Variable(word), startLine, startCol)
            } else {
              Token(TokenType.Identifier(word), startLine, startCol)
            }
        }
      } else {
        advance()
        nextToken()
      }
    }
  }

  private def skipWhitespaceAndComments(): Unit = {
    var inComment = false
    var skipping = true
    while (pos < len && skipping) {
      val c = peek()
      if (inComment) {
        if (c == '\n') inComment = false
        advance()
      } else if (c == '%' || (c == '/' && peekNext() == '/')) {
        inComment = true
        advance()
      } else if (Character.isWhitespace(c)) {
        advance()
      } else {
        skipping = false
      }
    }
  }

  private def scanIdentifier(): String = {
    val sb = new StringBuilder()
    while (pos < len && (Character.isJavaIdentifierPart(peek()) || peek() == '_')) {
      sb.append(advance())
    }
    sb.toString()
  }

  private def scanString(): TokenType = {
    advance() // skip open quote
    val sb = new StringBuilder()
    var closed = false
    while (pos < len && !closed) {
      val c = advance()
      if (c == '"') closed = true
      else if (c == '\\' && pos < len) {
        val escaped = advance()
        escaped match {
          case 'n' => sb.append('\n')
          case 't' => sb.append('\t')
          case 'r' => sb.append('\r')
          case '\"' => sb.append('\"')
          case '\\' => sb.append('\\')
          case other => sb.append(other)
        }
      } else {
        sb.append(c)
      }
    }
    TokenType.StrLit(sb.toString())
  }

  private def scanNumber(): TokenType = {
    val sb = new StringBuilder()
    var isFloat = false
    while (pos < len && (Character.isDigit(peek()) || (peek() == '.' && Character.isDigit(peekNext())))) {
      if (peek() == '.') isFloat = true
      sb.append(advance())
    }
    val str = sb.toString()
    if (isFloat) TokenType.Decimal(str.toDouble)
    else TokenType.Number(str.toLong)
  }

  def tokenizeAll(): List[Token] = {
    val buf = collection.mutable.ListBuffer[Token]()
    var t = nextToken()
    while (t.tokenType != TokenType.EOF) {
      buf += t
      t = nextToken()
    }
    buf += t
    buf.toList
  }
}
