package services.identity.agreement

import services.cinemas.pl.NonMovieEventClassifier
import services.identity.{Listing, ListingShape}

import java.util.Locale

/**
 * Whether a cluster the model left unmatched is a confident NON-FILM event — a concert, a yoga class, a horror
 * marathon, a secret screening, an esports final, a festival pass, a performer's live show, a theatre company's
 * staging — that no film database holds, so the agreement stage asks none of them about it ([[AgreementStage]]): every
 * such cluster cost a dozen family questions (IMDb, Wikidata, Filmweb, RT, Metacritic searches and records) and its
 * posters' hashes, re-asked whenever they went stale, for an answer that is always "nothing" — or worse, a film one
 * fragment of the show's billing names (DE "Gut gegen Nordwind - Thiele-Neumann-Theater": the 2019 film of the play).
 * Its card stays as it is, with no film: the scrape-time filter ([[NonMovieEventClassifier]], opted into per client)
 * is what drops an event from the listings.
 *
 * High precision before recall, as the scrape filter is: a film skipped here loses the agreement's take for good, a
 * missed event only costs its questions. So a cluster is an event only when EVERY listing of it is, and a listing is
 * not one when it states a film's record (a director and a year), bills a relay (a broadcast, a stage work, a house's
 * season, a concert film: cinema the broadcast take names), or names a film screened with the event ("+ film",
 * "pokaz", "seans", a festival edition's number, a performance "joined" to its billing). Measured over the unmatched clusters' fixture against `labels.tsv`:
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

  /** "… joined with …": what follows is the companion of what the title bills before it — a performance named only
   *  there is a screening's ("DYRYGENT - POŁĄCZONY Z KONCERTEM MUZYKI NA ŻYWO", Wajda's film), a performance billed
   *  before it is still one ("Koncert jazzowy połączony z degustacją wina"). */
  private val JoinedWith = """(?iu)\s*\bpołączon\p{L}*\s+ze?\b""".r

  /** A performer billed LIVE: a piece of the title ending in the word ("Jan Logemann LIVE: Herr Logemann hebt ab!",
   *  "Jan Preuß LIVE") — never one opening a title, as a film's own name does ("Live and Let Die"), nor followed by what
   *  a screening or a relay bills ("LIVE VIEWING", "Live Score", "Live In Budapest"); a house's relay ("NT Live: …",
   *  "RBO Live 2026/27: …") is read as one before this ([[ListingShape.relays]]). */
  private val PerformerLive = """\S\s+live\s*(?::|\s[-–—]\s|-\s|$)""".r

  /** A company's staging after the work it stages: a trailing piece that is one name ending in "theater" or "theatre"
   *  ("Gut gegen Nordwind - Thiele-Neumann-Theater"); a cinema's ("Filmtheater", "Lichtspieltheater") is none. */
  private val TheatreCompany = """\s[-–—]\s+[\p{L}.'&-]*theat(?:er|re)\s*$""".r
  private val CinemaTheatre  = """(?:film|kino|licht\p{L}*)-?theat(?:er|re)\s*$""".r

  /** "live im Beluga Kino" at Beluga Kino: the performer is there in person. A relay "live im Kino" names no venue. */
  private def liveAtTheVenue(title: String, venue: String): Boolean = {
    val name = venue.trim.toLowerCase(Locale.ROOT)
    name.nonEmpty && s"""\\blive\\s+\\p{L}{1,4}\\s+(?:\\p{L}{1,4}\\s+)?${java.util.regex.Pattern.quote(name)}""".r.findFirstIn(title).isDefined
  }

  /** A performer billed live ([[PerformerLive]], [[liveAtTheVenue]]), no film named beside the show and no broadcast
   *  named but by its bare "live" ([[ListingShape.relaysNamed]]). `title`: the raw title in lower case. */
  private def liveShow(listing: Listing, title: String): Option[String] =
    Option.when(FilmAttached.findFirstIn(title).isEmpty && (PerformerLive.findFirstIn(title).isDefined || liveAtTheVenue(title, listing.venue)) &&
      !ListingShape.relaysNamed(listing, title))("live show")

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
      if (ListingShape.relays(listing, title)) liveShow(listing, title)
      else Markers.collectFirst { case (reason, marker) if marker.findFirstIn(title).isDefined => reason }
        .filter(reason => reason == "no screening" || reason == "pass" || FilmAttached.findFirstIn(title).isEmpty)
        .orElse(Option.when(NonMovieEventClassifier.isPerformance(JoinedWith.split(raw).head) && FilmAttached.findFirstIn(title).isEmpty)("live event"))
        .orElse(liveShow(listing, title))
        .orElse(Option.when(FilmAttached.findFirstIn(title).isEmpty && TheatreCompany.findFirstIn(title).isDefined &&
          CinemaTheatre.findFirstIn(title).isEmpty)("theatre company"))
        .orElse(ListingShape.programmeSlotOf(raw).map(_ => "programme slot"))
    }
}
