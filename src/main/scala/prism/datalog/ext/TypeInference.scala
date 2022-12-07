package prism.datalog.ext

import prism.core.data.Datum
import prism.datalog.ast._
import scala.collection.mutable

sealed trait DType
object DType {
  case object TInt extends DType
  case object TFloat extends DType
  case object TStr extends DType
  case object TBool extends DType
  case object TAny extends DType
  final case class TVar(id: Int) extends DType
}

class TypeError(msg: String) extends RuntimeException(s"Type Inference Error: $msg")

/**
 * Constraint-based type inference engine for Datalog predicates and variables.
 * Enforces strong typing across rules, facts, and arithmetic expressions.
 */
final class TypeInference {
  private var varCounter = 0
  private def freshTypeVar(): DType.TVar = {
    varCounter += 1
    DType.TVar(varCounter)
  }

  private val substitutions = mutable.Map[Int, DType]()

  private def resolve(t: DType): DType = t match {
    case DType.TVar(id) =>
      substitutions.get(id) match {
        case Some(resolved) =>
          val finalRes = resolve(resolved)
          substitutions(id) = finalRes
          finalRes
        case None => t
      }
    case other => other
  }

  private def unify(t1: DType, t2: DType): Unit = {
    val r1 = resolve(t1)
    val r2 = resolve(t2)
    (r1, r2) match {
      case (a, b) if a == b => ()
      case (DType.TAny, _) => ()
      case (_, DType.TAny) => ()
      case (DType.TVar(id), other) =>
        substitutions(id) = other
      case (other, DType.TVar(id)) =>
        substitutions(id) = other
      case (DType.TInt, DType.TFloat) =>
        // Allow promotion of int to float
        ()
      case (a, b) =>
        throw new TypeError(s"Cannot unify type $a with $b")
    }
  }

  def inferProgram(program: Program): Map[String, List[DType]] = {
    val predicateSignatures = mutable.Map[String, List[DType]]()

    // 1. Register signatures from facts
    for (fact <- program.facts) {
      val argTypes = fact.values.map(datumToType)
      predicateSignatures.get(fact.predicate) match {
        case Some(existing) =>
          require(existing.length == argTypes.length, s"Arity mismatch in fact ${fact.predicate}")
          existing.zip(argTypes).foreach((t1, t2) => unify(t1, t2))
        case None =>
          predicateSignatures.put(fact.predicate, argTypes)
      }
    }

    // 2. Infer across rules
    for (rule <- program.rules) {
      val varTypes = mutable.Map[String, DType]()

      // Function to get or assign type to term
      def termType(term: Term): DType = term match {
        case Term.Var(name) =>
          varTypes.getOrElseUpdate(name, freshTypeVar())
        case Term.Const(d) =>
          datumToType(d)
        case Term.BinaryExpr(_, left, right) =>
          val lt = termType(left)
          val rt = termType(right)
          unify(lt, DType.TInt)
          unify(rt, DType.TInt)
          DType.TInt
      }

      // Check body atoms
      for (lit <- rule.body) {
        lit match {
          case PositiveAtom(pred, terms) =>
            val actualTypes = terms.map(termType)
            val sig = predicateSignatures.getOrElseUpdate(pred, actualTypes.map(_ => freshTypeVar()))
            sig.zip(actualTypes).foreach((t1, t2) => unify(t1, t2))

          case NegatedAtom(pred, terms) =>
            val actualTypes = terms.map(termType)
            val sig = predicateSignatures.getOrElseUpdate(pred, actualTypes.map(_ => freshTypeVar()))
            sig.zip(actualTypes).foreach((t1, t2) => unify(t1, t2))

          case ComparisonLiteral(_, left, right) =>
            val lt = termType(left)
            val rt = termType(right)
            unify(lt, rt)

          case _ => ()
        }
      }

      // Unify head
      val headActualTypes = rule.head.terms.map(termType)
      val headSig = predicateSignatures.getOrElseUpdate(rule.head.predicate, headActualTypes.map(_ => freshTypeVar()))
      headSig.zip(headActualTypes).foreach((t1, t2) => unify(t1, t2))
    }

    // Resolve all signatures
    predicateSignatures.map { case (pred, types) =>
      pred -> types.map(resolve)
    }.toMap
  }

  private def datumToType(d: Datum): DType = d match {
    case _: Datum.I64 => DType.TInt
    case _: Datum.F64 => DType.TFloat
    case _: Datum.Str => DType.TStr
    case _: Datum.Bool => DType.TBool
    case _ => DType.TAny
  }
}
