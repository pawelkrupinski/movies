package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.{Listing, ResolverDecision}
import services.identity.vocabulary.{Answer, Dependency, Question}

/** The FILL of a cluster's verdict where its families agree on no film ([[ClusterVerdicts]]): the film the first selected
 *  fill rule takes for it, `Unknown` while a TMDB answer the cluster's candidates need is a gap — what the verdict keeps,
 *  every read noted in its `receipt`. [[AgreementTakes]] fills it, beside the takes that read the kept fill. */
private[agreement] trait ClusterFill {
  def filledOf(decision: ResolverDecision, listings: Seq[Listing], verdicts: Seq[FamilyVerdict], stated: Seq[Listing],
               thisYear: Int, asked: mutable.Set[Question], receipt: mutable.Map[Dependency, Long]): Answer[Option[StoredFill]]
}
