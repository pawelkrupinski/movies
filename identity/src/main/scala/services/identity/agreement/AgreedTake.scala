package services.identity.agreement

import scala.collection.mutable

import services.identity.BilledPiece
import services.identity.measures.{Listing, ResolverDecision}
import services.identity.vocabulary.{Answer, Question, VoterFamily}

/** The AGREED take of a cluster the model left unmatched ([[AgreementTakes]]): the film its families agree on — unless
 *  its billing names that film as one piece beside another thing ([[pieceOf]]), a venue poster vetoes it, the venue's
 *  synopsis refuses it (`venueText`, [[VenueText.synopsisAgainst]]), or an answer it waits on is not in. Read through
 *  `evidence` ([[AgreementEvidence]]); a film TMDB holds no record of stands on the first of `identities` that names it. */
private[agreement] final class AgreedTake(evidence: AgreementEvidence, venueText: VenueText, identities: Seq[VoterFamily]) {
  import AgreementTakes.taking

  /** What became of the film the families agree on for the cluster `id` (its `listings` digesting to `digest`) on their
   *  `verdict` ([[taken]]): none to take where they agree on none. What it waits on is noted in `finding`. */
  def agreedTake(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: StoredVerdict,
         finding: mutable.Set[Question], distances: AgreementTakes.Distances): Take[ResolverDecision] =
    verdict.agreed.fold[Take[ResolverDecision]](Take.Untaken)(taken(decision, _, finding, distances,
      film => venueText.synopsisAgainst(id, digest, listings, film)))

  /** Does a listing of the cluster bill a stage work ([[services.identity.measures.ListingShape.stagesAWork]])? Then a screen
   *  adaptation agreed for it yields to the relay its screening days name ([[AgreementTakes.BroadcastTake]]). */
  def yieldsToBroadcast(listings: Seq[Listing]): Boolean = listings.exists(services.identity.measures.ListingShape.stagesAWork)

  /** Why the film the families agree on is none of the cluster's: its billing names it as ONE PIECE beside another thing
   *  ([[BilledPiece.ofFamilies]]) — DE "Femme - Frauen heilen die Welt" is a documentary, not the 2023 "Femme". Then the
   *  families' best match is that piece's namesake, and no fill or poster is asked for a film either: what the billing
   *  names is no film the families found. */
  def pieceOf(verdict: StoredVerdict, listings: Seq[Listing]): Option[String] =
    verdict.agreed.flatMap { agreed =>
      val film = agreed.record.film
      BilledPiece.ofFamilies(listings, film, agreed.crossId("tmdb").flatMap(_.toIntOption))
        .map(piece => s"no film: the families agree on '${film.title}', one piece of the billing${if (piece.staging) " beside a staging" else ""} — ${piece.why}")
    }

  /** The decision with the agreed film taken — pending while TMDB was not asked about its IMDb id yet or the venue posters
   *  are not all hashed, and not taken when they VETO it ([[services.identity.measures.PosterEvidence.veto]]): a venue
   *  poster names another candidate; nor when the venue's synopsis names another's description (`synopsis`): REFUSED,
   *  saying why. */
  private def taken(decision: ResolverDecision, agreed: AgreedFilm, finding: mutable.Set[Question],
                    distances: AgreementTakes.Distances, synopsis: Int => Option[String]): Take[ResolverDecision] = {
    val imdb = agreed.crossId("imdb")
    // read only for a film the families' take may stand on: a find only review sites' agreement waits on is none to ask
    lazy val tmdb = evidence.tmdbFilmOf(agreed.crossId("tmdb").flatMap(_.toIntOption), imdb, finding)
    val leaning = Option.when(agreed.leaning.nonEmpty)(s", ${agreed.leaning.toSeq.map(_.label).sorted.mkString(", ")} leaning to it").getOrElse("") +
      Option.when(agreed.corroborated.nonEmpty)(s", corroborated by ${agreed.corroborated.toSeq.sorted.mkString(", ")}").getOrElse("")
    val line = s"${agreed.families.toSeq.map(_.label).sorted.mkString(", ")}$leaning agree on ${agreed.record.film.titled}${imdb.fold("")(id => s" $id")}"
    val ids  = agreed.ids.map { case (family, id) => family.label -> id }
    // a film only review sites take — their pages name no film a card stands on, and match an event's namesake (DE "André
    // Rieus Weihnachtskonzert 2026: Let it Snow": Metacritic's and RT's 2019 "Let It Snow", the model's best candidate)
    if (!agreed.families.exists(_.namesFilms)) Take.Untaken
    else tmdb.failedAs(None) match {
      case Answer.Unknown | Answer.Failed => Take.Pending // failedAs leaves no Failed
      case Answer.Known(Some(film)) => Take.screened(Some(film), distances, synopsis)(taking(decision)(ResolverDecision.Basis.Agreed, line, film = Some(film), agreed = ids))
      case Answer.Known(None) => identityOf(agreed).fold[Take[ResolverDecision]](Take.Untaken) { case (source, id) =>
        Take.screened(None, distances, synopsis)(taking(decision)(ResolverDecision.Basis.Agreed, line,
          fallback = Some(AgreementTakes.fallbackOn(source, id, Some(agreed.record.film.title), agreed.record.film.year)), agreed = ids)) }
    }
  }

  /** The id a film TMDB holds no record of stands on: its IMDb id, else the own id of the first of `identities` that
   *  took it or whose id an agreeing record links — as `(source, id)`, the source a fallback film names. */
  private def identityOf(agreed: AgreedFilm): Option[(String, String)] =
    (VoterFamily.Imdb +: identities).distinct.iterator.flatMap(family =>
      agreed.ids.get(family).orElse(agreed.crossId(family.database)).map(family.database -> _)).nextOption()
}
