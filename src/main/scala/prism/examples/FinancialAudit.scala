package prism.examples

import prism.core.semiring.{ProvenancePolynomial, Monomial}

final case class Transaction(
  id: String,
  fromAccount: String,
  toAccount: String,
  amount: Double
)

/**
 * Financial audit and anti-money laundering (AML) taint tracker.
 * Uses Green-Karvounarakis-Tannen provenance polynomials N[X] to trace fund lineage
 * and identify tainted transaction cycles.
 */
object FinancialAudit {

  /**
   * Traces fund lineages from tainted source accounts across multiple transaction hops.
   */
  def traceTaint(
    transactions: List[Transaction],
    taintedSources: Set[String],
    maxHops: Int = 5
  ): Map[String, ProvenancePolynomial] = {
    val accountTaints = collection.mutable.Map[String, ProvenancePolynomial]()

    // Seed tainted sources with symbolic tokens
    for (src <- taintedSources) {
      accountTaints.put(src, ProvenancePolynomial.variable(s"source_$src"))
    }

    var changed = true
    var hop = 0
    while (changed && hop < maxHops) {
      changed = false
      hop += 1

      for (tx <- transactions) {
        accountTaints.get(tx.fromAccount).foreach { incomingTaint =>
          val txToken = ProvenancePolynomial.variable(tx.id)
          // Lineage combines source taint multiplied by transaction token
          val propagatedTaint = incomingTaint * txToken

          val existingTaint = accountTaints.getOrElse(tx.toAccount, ProvenancePolynomial.Zero)
          val combinedTaint = existingTaint + propagatedTaint

          if (combinedTaint != existingTaint) {
            accountTaints.put(tx.toAccount, combinedTaint)
            changed = true
          }
        }
      }
    }

    accountTaints.toMap
  }

  /**
   * Identifies the primary illicit source contributing to an account's balance
   * by calculating partial derivatives of the provenance polynomial.
   */
  def sourceSensitivity(
    accountTaint: ProvenancePolynomial,
    sourceId: String
  ): ProvenancePolynomial = {
    accountTaint.partialDerivative(s"source_$sourceId")
  }
}
