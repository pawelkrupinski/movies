package services.identity.agreement

import scala.collection.concurrent.TrieMap

import models.ListingKey
import services.identity.measures.{Listing, ResolverDecision}

/** The CORRECTION of a film the model took by its own rules ([[AgreementPipeline.decide]]): the take read against the
 *  evidence that can contradict it — withdrawn or switched where that evidence says so, else as the model took it — and
 *  what each correction still waits on. [[AgreementCorrections]] reads it. */
private[agreement] trait ModelTakeCorrection {
  /** Is `decision` a film the model took by its own rules, what a correction reads the evidence against? */
  def correctable(decision: ResolverDecision): Boolean
  /** The model's take `decision`, read under the correction id `key` at the apply's `pass`: withdrawn or switched where
   *  the evidence against it says so, else `decision` itself. */
  def corrected(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], key: String, pass: AgreementPass): ResolverDecision
  /** What each correction still waiting on an answer asks, by its id. */
  def correctionWaits: TrieMap[String, ClusterWait]
}
