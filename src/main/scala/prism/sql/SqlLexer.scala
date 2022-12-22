package prism.sql

sealed trait SqlToken
object SqlToken {
  // Keywords
  case object Select extends SqlToken
  case object From extends SqlToken
  case object Where extends SqlToken
  case object Join extends SqlToken
  case object Inner extends SqlToken
  case object On extends SqlToken
  case object Group extends SqlToken
  case object By extends SqlToken
  case object Having extends SqlToken
  case object As extends SqlToken
  case object And extends SqlToken
  case object Or extends SqlToken
  case object Not extends SqlToken

  // Literals & Identifiers
  final case class Ident(name: String) extends SqlToken
  final case class IntLit(value: Long) extends SqlToken
  final case class FloatLit(value: Double) extends SqlToken
  final case class StringLit(value: String) extends SqlToken

  // Operators & Delimiters
  case object Star extends SqlToken
  case object Comma extends SqlToken
  case object Dot extends SqlToken
  case object LParen extends SqlToken
  case object RParen extends SqlToken
  case object Eq extends SqlToken
  case object Neq extends SqlToken
  case object Lt extends SqlToken
  case object Lte extends SqlToken
  case object Gt extends SqlToken
  case object Gte extends SqlToken
  case object Plus extends SqlToken
  case object Minus extends SqlToken
  case object Slash extends SqlToken
}

final class SqlLexer(val input: String) {
  private var pos = 0
  private val len = input.length

  private def peek(): Char = if (pos < len) input.charAt(pos) else '\u0000'
  private def advance(): Char = { val c = peek(); pos += 1; c }

  def tokenize(): List[SqlToken] = {
    val tokens = collection.mutable.ListBuffer[SqlToken]()

    while (pos < len) {
      val c = peek()
      if (Character.isWhitespace(c)) {
        advance()
      } else if (c == '-' && pos + 1 < len && input.charAt(pos + 1) == '-') {
        // Line comment
        while (pos < len && peek() != '\n') advance()
      } else if (c == ',') { advance(); tokens += SqlToken.Comma }
      else if (c == '.') { advance(); tokens += SqlToken.Dot }
      else if (c == '(') { advance(); tokens += SqlToken.LParen }
      else if (c == ')') { advance(); tokens += SqlToken.RParen }
      else if (c == '*') { advance(); tokens += SqlToken.Star }
      else if (c == '+') { advance(); tokens += SqlToken.Plus }
      else if (c == '-') { advance(); tokens += SqlToken.Minus }
      else if (c == '/') { advance(); tokens += SqlToken.Slash }
      else if (c == '=') { advance(); tokens += SqlToken.Eq }
      else if (c == '!' && pos + 1 < len && input.charAt(pos + 1) == '=') {
        pos += 2; tokens += SqlToken.Neq
      } else if (c == '<' && pos + 1 < len && input.charAt(pos + 1) == '>') {
        pos += 2; tokens += SqlToken.Neq
      } else if (c == '<' && pos + 1 < len && input.charAt(pos + 1) == '=') {
        pos += 2; tokens += SqlToken.Lte
      } else if (c == '<') { advance(); tokens += SqlToken.Lt }
      else if (c == '>' && pos + 1 < len && input.charAt(pos + 1) == '=') {
        pos += 2; tokens += SqlToken.Gte
      } else if (c == '>') { advance(); tokens += SqlToken.Gt }
      else if (c == '\'' || c == '"') {
        tokens += readString(advance())
      } else if (Character.isDigit(c)) {
        tokens += readNumber()
      } else if (Character.isLetter(c) || c == '_') {
        tokens += readIdentOrKeyword()
      } else {
        throw new IllegalArgumentException(s"Unexpected character in SQL query: '$c' at position $pos")
      }
    }

    tokens.toList
  }

  private def readString(quote: Char): SqlToken = {
    val sb = new StringBuilder()
    var closed = false
    while (pos < len && !closed) {
      val c = advance()
      if (c == quote) closed = true
      else sb.append(c)
    }
    if (!closed) throw new IllegalArgumentException("Unterminated string literal in SQL")
    SqlToken.StringLit(sb.toString())
  }

  private def readNumber(): SqlToken = {
    val start = pos
    var hasDot = false
    while (pos < len && (Character.isDigit(peek()) || (!hasDot && peek() == '.'))) {
      if (peek() == '.') hasDot = true
      advance()
    }
    val raw = input.substring(start, pos)
    if (hasDot) SqlToken.FloatLit(raw.toDouble)
    else SqlToken.IntLit(raw.toLong)
  }

  private def readIdentOrKeyword(): SqlToken = {
    val start = pos
    while (pos < len && (Character.isLetterOrDigit(peek()) || peek() == '_')) {
      advance()
    }
    val text = input.substring(start, pos)
    text.toUpperCase match {
      case "SELECT" => SqlToken.Select
      case "FROM" => SqlToken.From
      case "WHERE" => SqlToken.Where
      case "JOIN" => SqlToken.Join
      case "INNER" => SqlToken.Inner
      case "ON" => SqlToken.On
      case "GROUP" => SqlToken.Group
      case "BY" => SqlToken.By
      case "HAVING" => SqlToken.Having
      case "AS" => SqlToken.As
      case "AND" => SqlToken.And
      case "OR" => SqlToken.Or
      case "NOT" => SqlToken.Not
      case _ => SqlToken.Ident(text)
    }
  }
}
