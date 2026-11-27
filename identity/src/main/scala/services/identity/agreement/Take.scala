package services.identity.agreement

import services.identity.measures.PosterEvidence
import services.identity.vocabulary.Answer

/** What became of the film a cluster's families agree on: taken, pending an answer, vetoed by a venue poster, refused by
 *  the venue's synopsis (and why), or none to take. */
private[agreement] enum Take[+A] {
  case Taken(decision: A)
  case Pending
  case Vetoed
  case Refused(why: String)
  case Untaken
}
private[agreement] object Take {
  /** `take` of `film` as the venues' evidence lets it stand — what an agreed film and a filled one are both screened by:
   *  pending while a venue poster is not hashed (its `distances`), VETOED where one names another candidate
   *  ([[PosterEvidence.veto]]), REFUSED where the venue's synopsis names another's description (`synopsis`,
   *  [[VenueText.synopsisAgainst]]), else taken. */
  def screened[A](film: Option[Int], distances: AgreementTakes.Distances, synopsis: Int => Option[String])(take: => A): Take[A] =
    // a poster given up on (Failed) is no evidence for or against: as no distance found
    distances(film).failedAs(Nil) match {
      case Answer.Known(found) =>
        if (PosterEvidence.veto(film, found).nonEmpty) Take.Vetoed
        else film.flatMap(synopsis).fold[Take[A]](Take.Taken(take))(Take.Refused(_))
      case Answer.Unknown | Answer.Failed => Take.Pending // failedAs leaves no Failed
    }
}
