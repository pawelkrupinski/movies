package services.identity.agreement

import services.identity.{Answer, IdentityMeasures, Listing}

/**
 * The film a cluster's VENUES list on another family's site (Filmweb's programme of each Polish venue,
 * `/showtimes/cinema/<id>`, else its town's): each listing named by exactly one film of its venue's programme there —
 * a title of the film's record (or its title and original title together, "Lalka (Dolly)") equal to one of the
 * listing's, screening on one of the listing's days — half the cluster's listings at least named by the same one, none
 * by another. A listing its programme names nothing for (a screening past, a venue Filmweb does not list) says nothing:
 * PL Kino Seniora's "Ktoś całkiem obcy" at Luna, Oaza and Sława, the 2024 film, pooled with Kino Kryterium's past one.
 * What a film the model took is read against ([[Correction]]): never a fill — the +1 film it gained an unmatched
 * cluster did not pay for its requests (2026-10-06).
 *
 * Measured 2026-10-05 against prod's PL read model: of 2,850 listings a venue's own Filmweb programme names, 2,846 name
 * the film the model matched and the 4 others were the model's wrong matches (Kino za Rogiem's "Lalka" is Has's 1968
 * film, Kino Seniora's "Ktoś całkiem obcy" the 2024 one); of 338 a town's programme names, 337 agree, none contradict.
 *
 * `Unknown` while a programme or a named film's record is not answered yet — every such question asked, through the
 * answers' reads — and `Known(None)` once two listings name different films: read no further.
 */
object VenueListings {

  def listed(listings: Seq[Listing], answers: FamilyAnswers): Answer[Option[SourceRecord]] = {
    // a listing naming another film than one before it settles it: no film, whatever a gap would answer
    var unknown = false
    var film    = Option.empty[SourceRecord]
    var naming  = 0
    val each    = listings.iterator
    while (each.hasNext) named(each.next(), answers) match {
      case Answer.Unknown                                                    => unknown = true
      case Answer.Known(None)                                                =>
      case Answer.Known(Some(one)) if film.exists(_.crossIds != one.crossIds) => return Answer.Known(None)
      case Answer.Known(one)                                                 => film = one; naming += 1
    }
    // half of them at least: one venue's programme speaks for no cluster of many
    if (unknown) Answer.Unknown else Answer.Known(film.filter(_ => naming * 2 >= listings.size))
  }

  /** The one film the listing's venue programme names on its days, by a title equal to the listing's. */
  private def named(listing: Listing, answers: FamilyAnswers): Answer[Option[SourceRecord]] =
    answers.showing(listing.venue) match {
      case Answer.Unknown => Answer.Unknown
      case Answer.Known(showings) =>
        val onItsDays = showings.filter(showing => showing.days.days.exists(listing.screenings.contains))
        val records   = onItsDays.map(showing => showing.film -> answers.record(showing.film))
        if (records.exists(_._2 == Answer.Unknown)) Answer.Unknown
        else {
          val own = titleKeys(listing)
          records.collect { case (id, Answer.Known(Some(record))) if recordKeys(record.film).exists(own) =>
            record.copy(crossIds = record.crossIds + (answers.family.database -> id)) }.distinctBy(_.crossIds) match {
            case Seq(one) => Answer.Known(Some(one))
            case _        => Answer.Known(None)
          }
        }
    }

  private def titleKeys(listing: Listing): Set[String] =
    (Seq(listing.rawTitle, listing.title, listing.cleanTitle) ++ listing.searchTitle ++ listing.originalTitle)
      .map(IdentityMeasures.key).filter(_.nonEmpty).toSet

  /** A record's titles, and its title and original title together as a venue bills both ("Lalka (Dolly)"). */
  private def recordKeys(film: IdentityMeasures.Film): Seq[String] =
    (film.titles ++ film.originalTitle.filter(_ != film.title).map(original => s"${film.title} $original")).map(IdentityMeasures.key).filter(_.nonEmpty)
}
