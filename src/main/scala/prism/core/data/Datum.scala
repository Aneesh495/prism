package prism.core.data

/**
 * Algebraic datum representing strongly typed values in relational Datalog tuples.
 * Designed for fast comparison, deterministic hashing, and high serialization efficiency.
 */
sealed trait Datum extends Ordered[Datum] with Serializable {
  def typeName: String
  def typePrecedence: Int
  def asLong: Long = throw new UnsupportedOperationException(s"Cannot cast $this to Long")
  def asDouble: Double = throw new UnsupportedOperationException(s"Cannot cast $this to Double")
  def asString: String = throw new UnsupportedOperationException(s"Cannot cast $this to String")
  def asBoolean: Boolean = throw new UnsupportedOperationException(s"Cannot cast $this to Boolean")
}

object Datum {
  final case class I64(value: Long) extends Datum {
    def typeName: String = "i64"
    def typePrecedence: Int = 2
    override def asLong: Long = value
    override def asDouble: Double = value.toDouble

    def compare(that: Datum): Int = that match {
      case I64(v) => java.lang.Long.compare(value, v)
      case F64(v) => java.lang.Double.compare(value.toDouble, v)
      case _ => java.lang.Integer.compare(this.typePrecedence, that.typePrecedence)
    }

    override def toString: String = value.toString
  }

  final case class F64(value: Double) extends Datum {
    def typeName: String = "f64"
    def typePrecedence: Int = 3
    override def asDouble: Double = value

    def compare(that: Datum): Int = that match {
      case F64(v) => java.lang.Double.compare(value, v)
      case I64(v) => java.lang.Double.compare(value, v.toDouble)
      case _ => java.lang.Integer.compare(this.typePrecedence, that.typePrecedence)
    }

    override def toString: String = value.toString
  }

  final case class Str(value: String) extends Datum {
    def typeName: String = "str"
    def typePrecedence: Int = 4
    override def asString: String = value

    def compare(that: Datum): Int = that match {
      case Str(v) => value.compareTo(v)
      case _ => java.lang.Integer.compare(this.typePrecedence, that.typePrecedence)
    }

    override def toString: String = s""""$value""""
  }

  final case class Bool(value: Boolean) extends Datum {
    def typeName: String = "bool"
    def typePrecedence: Int = 1
    override def asBoolean: Boolean = value

    def compare(that: Datum): Int = that match {
      case Bool(v) => java.lang.Boolean.compare(value, v)
      case _ => java.lang.Integer.compare(this.typePrecedence, that.typePrecedence)
    }

    override def toString: String = value.toString
  }

  final case class Sym(value: String) extends Datum {
    def typeName: String = "sym"
    def typePrecedence: Int = 5
    override def asString: String = value

    def compare(that: Datum): Int = that match {
      case Sym(v) => value.compareTo(v)
      case _ => java.lang.Integer.compare(this.typePrecedence, that.typePrecedence)
    }

    override def toString: String = s":$value"
  }

  final case class TupleVal(elements: Vector[Datum]) extends Datum {
    def typeName: String = "tuple"
    def typePrecedence: Int = 6

    def compare(that: Datum): Int = that match {
      case TupleVal(other) =>
        val lenCompare = elements.length.compare(other.length)
        if (lenCompare != 0) lenCompare
        else {
          var i = 0
          var diff = 0
          while (i < elements.length && diff == 0) {
            diff = elements(i).compare(other(i))
            i += 1
          }
          diff
        }
      case _ => java.lang.Integer.compare(this.typePrecedence, that.typePrecedence)
    }

    override def toString: String = elements.mkString("(", ", ", ")")
  }

  case object NullVal extends Datum {
    def typeName: String = "null"
    def typePrecedence: Int = 0

    def compare(that: Datum): Int = that match {
      case NullVal => 0
      case _ => -1
    }

    override def toString: String = "null"
  }

  given Ordering[Datum] with {
    def compare(x: Datum, y: Datum): Int = x.compare(y)
  }

  def of(v: Long): Datum = I64(v)
  def of(v: Int): Datum = I64(v.toLong)
  def of(v: Double): Datum = F64(v)
  def of(v: String): Datum = Str(v)
  def of(v: Boolean): Datum = Bool(v)
  def sym(name: String): Datum = Sym(name)
  def tuple(elements: Datum*): Datum = TupleVal(elements.toVector)
}
