package services.identity.agreement

import services.billing.BillingPieces

import services.cinemas.pl.NonMovieEventClassifier
import services.identity.{Listing, ListingShape}
import services.movies.{TitleContainment, TitleWords}

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
    // a TV series' episodes or season screened ("Heated Rivalry: Season 01 - Epi 05 & 06", "… Season 1 Marathon"): TMDB
    // files a series apart from its films; a house's "Cinema Season 2026-27" names a year, never a season's number
    "tv episodes"       -> """\bseason\s*0?\d{1,2}\s*(?:marathon\b|[-–—:]\s*ep(?:i|isode)?s?\.?\s*\d)""".r,
    // a night of several films sold as one ticket
    "marathon"          -> """\bmaraton\s+(horror|hallow|grozy|film)|\bhalloween\p{L}*\s+maraton|\bhorror\s+marathon|\bnoc\s+(horror|grozy)|wiecz[oó]r\s+grozy""".r,
    // a franchise's films back to back, billed by the night's word before the franchise ("Maraton Diuny", "Maraton Saga
    // Zmierzch") or after it in English ("The Hunger Games Marathon") — never "<film> - maraton", one film's marathon
    // screening ("Inna Mamusia - maraton", judged right)
    "franchise marathon" -> """^maraton\s+\p{L}|\S\s+marathon\s*$""".r,
    "esports"           -> """\bleague\s+of\s+legends|\bworlds\s+\d\d\b.*\bfinals""".r,
    // a console session on the big screen ("Gaming im Kino - 3 Stunden", "Gaming im Lumos")
    "gaming"            -> """\bgaming\s+im\b""".r,
    // a pantomime staged for the cinemas (the BBC's "CBeebies Panto 2026: Treasure Island": the owner, 10-21, an event
    // that stays unmatched, whatever record TMDB files under its name)
    "panto"             -> """\bpanto\b""".r,
    // classes and evenings sold through the same ticketing
    "class"             -> """\bmedytacj|\bneurojog|\bmilong|\bslajd""".r,
    // a festival or season pass, never one screening
    "pass"              -> """\bkarnet""".r,
    // a booking of the room, or a day the venue does not screen
    "no screening"      -> """\bscreen\s+hire\b|\bkeine\s+vorstellung|dzi[sś]\s+nie\s+gramy|^sonderveranstaltung$|^sondervorstellung$|^film\s+program$""".r
  )

  /** A screening whose film is withheld — nothing to identify ("Secret Screening", "Scream Unseen", "Überraschungsfilm"):
   *  the word table's secret screenings ([[TitleWords]]), read on the title's folded words. */
  private val SecretScreening = TitleWords.foldedPattern(TitleWords.Concept.SecretScreening).r

  /** A film screened with the event — the event its companion, the film still the families' to name — or a festival's
   *  numbered edition billing one of its films ("11. UFF - Teatr weteranów" is a documentary). */
  private val FilmAttached = """\+|\bpokaz\b|\bseans|\bfilm(?!ow)|^\d+\.\s""".r

  /** "… joined with …": what follows is the companion of what the title bills before it — a performance named only
   *  there is a screening's ("DYRYGENT - POŁĄCZONY Z KONCERTEM MUZYKI NA ŻYWO", Wajda's film), a performance billed
   *  before it is still one ("Koncert jazzowy połączony z degustacją wina"). */
  private val JoinedWith = """(?iu)\s*\bpołączon\p{L}*\s+ze?\b""".r

  /** What `raw` bills as the event itself — what precedes "joined with" ([[JoinedWith]]), its companion following — or
   *  `None` when the billing attaches a screening to it ([[FilmAttached]]): the film is the evening then, the event its
   *  companion. */
  private[identity] def eventBilled(raw: String): Option[String] =
    Option.when(FilmAttached.findFirstIn(raw.toLowerCase(Locale.ROOT)).isEmpty)(JoinedWith.split(raw).head)

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

  /** What an event's billing states of itself beyond its kind ([[of]]): the edition its title dates (a year, "2026/27"'s
   *  first; an esports final's "Worlds 26"), the series season and episodes it screens ("Season 01 - Epi 05 & 06": `s1e5-6`,
   *  "Season 1 Marathon": `s1`) and the hours a session runs ("Gaming im Kino - 3 Stunden": 180 minutes). Two billings of
   *  one event agree on each they both state; one stating another is another event ([[Facts.apartFrom]]). `name`: the
   *  title's words, an article and the edition however spelt aside — what two venues' spellings of one event share ("The
   *  CBeebies Panto 2026: Treasure Island", "Cbeebies Panto: Treasure Island"). */
  final case class Facts(kind: String, edition: Option[Int], episodes: Option[String], minutes: Option[Int], name: Seq[String] = Nil) {
    /** Whether its kind is one no film record ever is — a marker's ([[Markers]]) or a withheld film's — unlike a live
     *  show, a concert or a festival's slot, whose billing can be a relay or a concert film TMDB holds (UK "Berliner
     *  Philharmoniker LIVE: New Year's Eve Concert 2026" is tmdb 1711927). */
    def filmless: Boolean = FilmlessKinds(kind)
    /** Another edition, other episodes or a session of another length: each a fact both state and disagree on. */
    def apartFrom(other: Facts): Boolean = {
      def differ[A](a: Option[A], b: Option[A]) = a.isDefined && b.isDefined && a != b
      kind != other.kind || differ(edition, other.edition) || differ(episodes, other.episodes) || differ(minutes, other.minutes)
    }
  }

  private val FilmlessKinds: Set[String] = Markers.map(_._1).toSet + "mystery screening"

  private val EditionYear    = """(?<![\p{N}])(20\d\d)(?![\p{N}])""".r
  private val EsportsEdition = """\bworlds\s*(\d\d)\b""".r
  private val SeriesSeason   = """\bseason\s*0*(\d{1,2})\b""".r
  private val Episodes       = """\bep(?:i|isode)?s?\.?\s*0*(\d{1,3})(?:\s*(?:&|and|-|–)\s*0*(\d{1,3}))?""".r
  private val SessionHours   = """\b(\d{1,2})\s*(?:stunden|hours|hrs)\b""".r

  /** `listing`'s event and what its title states of it ([[Facts]]), or `None` when it bills no event ([[of]]). */
  def facts(listing: Listing): Option[Facts] = of(listing).map { kind =>
    val title    = listing.rawTitle.toLowerCase(Locale.ROOT)
    val edition  = EditionYear.findFirstMatchIn(title).map(_.group(1).toInt)
      .orElse(EsportsEdition.findFirstMatchIn(title).map(2000 + _.group(1).toInt))
    val episodes = SeriesSeason.findFirstMatchIn(title).map { season =>
      s"s${season.group(1).toInt}" + Episodes.findFirstMatchIn(title.substring(season.end)).fold("") { m =>
        s"e${m.group(1).toInt}" + Option(m.group(2)).fold("")(last => s"-${last.toInt}")
      }
    }
    // the edition however spelt: "Worlds 2026" and "Worlds 26" are one final's
    val editionWords = edition.toSeq.flatMap(year => Seq(year.toString, f"${year % 100}%02d"))
    val name = TitleContainment.tokens(listing.rawTitle).filterNot(word => word == "the" || EditionYear.matches(word) || editionWords.contains(word))
    Facts(kind, edition, episodes, SessionHours.findFirstMatchIn(title).map(_.group(1).toInt * 60), name)
  }

  /** The event every one of `listings` bills, as the first states it — `None` unless each bills one of one kind. */
  def facts(listings: Seq[Listing]): Option[Facts] = {
    val all = listings.map(facts)
    all.headOption.flatten.filter(first => all.forall(_.exists(_.kind == first.kind)))
  }

  /** Why `listing` is an event no film database holds, or `None`. */
  def of(listing: Listing): Option[String] =
    if (listing.directors.exists(_.trim.nonEmpty) && listing.year.isDefined) None
    else {
      val raw   = listing.rawTitle.trim
      val title = raw.toLowerCase(Locale.ROOT)
      if (ListingShape.relays(listing, title)) liveShow(listing, title)
      else Option.when(SecretScreening.findFirstIn(TitleContainment.tokens(raw).mkString(" ")).isDefined)("mystery screening")
        .orElse(Markers.collectFirst { case (reason, marker) if marker.findFirstIn(title).isDefined => reason })
        .filter(reason => reason == "no screening" || reason == "pass" || FilmAttached.findFirstIn(title).isEmpty)
        .orElse(eventBilled(raw).filter(NonMovieEventClassifier.isPerformance(_)).map(_ => "live event"))
        // what the feeds' own filter reads in any language: a reading, a slam, a cabaret, a stand-up night billed as one
        // (DE "Schöne Aussicht - Die Slam Poetry & Songwriting Show - …": no 2007 "Schöne Aussicht")
        .orElse(Option.when(NonMovieEventClassifier.isNonScreening(raw) && FilmAttached.findFirstIn(title).isEmpty)("non-screening"))
        .orElse(liveShow(listing, title))
        .orElse(Option.when(FilmAttached.findFirstIn(title).isEmpty && TheatreCompany.findFirstIn(title).isDefined &&
          CinemaTheatre.findFirstIn(title).isEmpty)("theatre company"))
        .orElse(BillingPieces.programmeSlot(raw).map(_ => "programme slot"))
        .orElse(Option.when(BillingPieces.billsAFilmFestival(raw))("film festival"))
    }
}
