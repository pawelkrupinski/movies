package services.identity.agreement

import services.identity.measures.{PosterEvidence, ResolverDecision}
import services.identity.vocabulary.Answer

/** The POSTER-VOTE take of a cluster the families agreed on no film for ([[AgreementTakes.PosterVoteTake]]): the one
 *  candidate its venue posters match within [[PosterEvidence.VoteBits]], none vetoing it — named as `evidence`
 *  ([[AgreementEvidence]]) reads TMDB's record of it. */
private[agreement] final class PosterVoteTake(evidence: AgreementEvidence) {
  /** The decision with the one candidate the venue posters match taken ([[PosterEvidence.vote]]), by their `distances`
   *  to the cluster's candidates. */
  def voted(decision: ResolverDecision, distances: Answer[Seq[Map[Int, Option[Int]]]]): Option[ResolverDecision] =
    distances.answered.flatMap(PosterEvidence.vote).map { case (film, bits) =>
      val named = evidence.looked.flatMap(_.film(film).answered.flatten).fold(s"tmdb $film")(_.titled)
      AgreementTakes.taking(decision)(ResolverDecision.Basis.Poster,
        s"the venue's poster matches $named ($bits bits), no other candidate's within ${PosterEvidence.VoteBits}", film = Some(film))
    }
}
