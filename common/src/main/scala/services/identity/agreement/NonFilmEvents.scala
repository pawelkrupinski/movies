package services.identity.agreement

import services.cinemas.pl.NonMovieEventClassifier
import services.identity.{Listing, ListingShape}

import java.util.Locale

/**
 * Whether a cluster the model left unmatched is a confident NON-FILM event — a concert, a yoga class, a horror
 * marathon, a secret screening, an esports final, a festival pass — that no film database holds, so the agreement
 * stage asks none of them about it ([[AgreementStage]]): every such cluster cost a dozen family questions (IMDb,
 * Wikidata, Filmweb, RT, Metacritic searches and records) and its posters' hashes, re-asked whenever they went stale,
 * for an answer that is always "nothing". Its card stays as it is: this decides how much the stage SPENDS on a
 * listing, never what is shown — the scrape-time filter ([[NonMovieEventClassifier]], opted into per client) is what
 * drops an event from the listings.
 *
 * High precision before recall, as the scrape filter is: a film skipped here loses the agreement's take for good, a
 * missed event only costs its questions. So a cluster is an event only when EVERY listing of it is, and a listing is
 * not one when it states a film's record (a director and a year), bills a relay (a broadcast, a stage work, a house's
 * season, a concert film: cinema the broadcast take names), or names a film screened with the event ("+ film",
 * "pokaz", "seans", a festival edition's number). Measured over the unmatched clusters' fixture against `labels.tsv`:
 * no listing a label names a right film for is one (`NonFilmEventsFixtureSpec`).
 */
object NonFilmEvents {

  /** What makes a title an event the families hold no record of, beyond the scrape filter's performances
   *  ([[NonMovieEventClassifier.isPerformance]]: its talks and workshops are a film's companion too often) —
   *  language-neutral, as a venue bills its programme in whatever language. Each anchored so a film's own name does not
   *  trip it: a "Maraton" film needs the bare word, so `maraton` counts only as the marathon of a genre or a night
   *  ("Maraton Horrorów", "Halloweenowy Maraton", "Maraton Halloween"). */
  private val Markers: Seq[(String, scala.util.matching.Regex)] = Seq(
    // a screening whose film is withheld: nothing to identify
    "mystery screening" -> """\bsecret\s+(screening|movie|screaming|cinema)|\bmystery\s+movie|\bscream\s+unseen|\bscreen\s+unseen""".r,
    // a night of several films sold as one ticket
    "marathon"          -> """\bmaraton\s+(horror|hallow|grozy|film)|\bhalloween\p{L}*\s+maraton|\bhorror\s+marathon|\bnoc\s+(horror|grozy)|wiecz[oó]r\s+grozy|\bdismember\s+the\s+alamo""".r,
    "esports"           -> """\bleague\s+of\s+legends|\bworlds\s+\d\d\b.*\bfinals""".r,
    // classes and evenings sold through the same ticketing
    "class"             -> """\bmedytacj|\bneurojog|\bmilong|\bslajd""".r,
    // a festival or season pass, never one screening
    "pass"              -> """\bkarnet""".r,
    // a booking of the room, or a day the venue does not screen
    "no screening"      -> """\bscreen\s+hire\b|\bkeine\s+vorstellung|dzi[sś]\s+nie\s+gramy|^sonderveranstaltung$|^sondervorstellung$|^film\s+program$""".r
  )

  /** A film screened with the event — the event its companion, the film still the families' to name — or a festival's
   *  numbered edition billing one of its films ("11. UFF - Teatr weteranów" is a documentary). */
  private val FilmAttached = """\+|\bpokaz\b|\bseans|\bfilm(?!ow)|^\d+\.\s""".r

  /** Why `listings` (a cluster's) are an event no film database holds — the first listing's reason — or `None` unless
   *  every one of them is. */
  def of(listings: Seq[Listing]): Option[String] =
    if (listings.isEmpty) None
    else {
      val reasons = listings.iterator.map(of)
      val first   = reasons.next()
      first.filter(_ => reasons.forall(_.isDefined))
    }

  /** Why `listing` is an event no film database holds, or `None`. */
  def of(listing: Listing): Option[String] =
    if (listing.directors.exists(_.trim.nonEmpty) && listing.year.isDefined) None
    else {
      val raw   = listing.rawTitle.trim
      val title = raw.toLowerCase(Locale.ROOT)
      if (ListingShape.relays(listing, title)) None
      else Markers.collectFirst { case (reason, marker) if marker.findFirstIn(title).isDefined => reason }
        .filter(reason => reason == "no screening" || reason == "pass" || FilmAttached.findFirstIn(title).isEmpty)
        .orElse(Option.when(NonMovieEventClassifier.isPerformance(raw) && FilmAttached.findFirstIn(title).isEmpty)("live event"))
    }
}
