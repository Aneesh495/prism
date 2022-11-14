package prism.temporal

import prism.core.data.Tuple

/**
 * Temporal Logic expression representing temporal properties and sequence patterns.
 */
sealed trait TemporalExpr extends Serializable

object TemporalExpr {
  final case class Predicate(name: String, test: Tuple => Boolean) extends TemporalExpr {
    override def toString: String = name
  }

  final case class And(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  final case class Or(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  final case class Not(inner: TemporalExpr) extends TemporalExpr

  /**
   * Linear Temporal Logic Next operator: property holds at step t + 1.
   */
  final case class Next(inner: TemporalExpr) extends TemporalExpr

  /**
   * Linear Temporal Logic Always (Box) operator: property holds for all steps in window.
   */
  final case class Always(inner: TemporalExpr, windowSpan: Long) extends TemporalExpr

  /**
   * Linear Temporal Logic Eventually (Diamond) operator: property holds for at least one step.
   */
  final case class Eventually(inner: TemporalExpr, windowSpan: Long) extends TemporalExpr

  /**
   * Sequence pattern: event A followed by event B within maxDuration.
   */
  final case class FollowedBy(first: TemporalExpr, second: TemporalExpr, maxDuration: Long) extends TemporalExpr
}
