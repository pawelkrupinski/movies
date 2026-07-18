package services.cinemas.pl
/** Decides whether a scraped listing is a live STAGE/MUSIC event rather than a
 *  film. Small municipal & arthouse venues sell tickets to their own concerts,
 *  stand-up nights, kabaret shows, recitals and theatre plays through the same
 *  ticketing/listing surface their film repertoire comes from, so those rows
 *  leak into the scrape (e.g. "Koncert Joscho Stephan Trio", "Piotr Bałtroczyk
 *  Stand-up", "Edyta Geppert - recital", a tribute gig "…- pamięci Amy
 *  Winehouse", a Silesian "…biesiada z TVS", a gift "Bon podarunkowy", a blind
 *  "Seans w ciemno", or a festival "…– spotkanie z podróżnikiem…").
 *
 *  This is deliberately HIGH-PRECISION, not high-recall: a stray event slipping
 *  through is cosmetic, but dropping a real film is a regression. So two whole
 *  classes of cinema content that merely *contain* event vocabulary are kept:
 *
 *   - "Event cinema" BROADCASTS — opera/ballet/theatre transmissions and
 *     concert films shown on the screen (Royal Ballet & Opera season, National
 *     Theatre Live / "Teatr w Kinie", Met Opera, André Rieu, Pavarotti). These
 *     carry a broadcast marker ([[BroadcastMarkers]]) that vetoes the event
 *     verdict.
 *   - Art DOCUMENTARIES — the "Exhibition on Screen" / "Wielka Sztuka" series
 *     ("…Przełomowa wystawa", "Wielka Sztuka w Kinoteatrze Rialto - Hopper…").
 *     These never trip a marker in the first place: the event vocabulary here is
 *     `wystawa`/art words, which are NOT treated as event markers, and the
 *     venue's "Kino**teatr**ze" survives because [[EventMarkers]] anchors on a
 *     word boundary (`\bteatr`, not a bare substring).
 *
 *  Applied per cinema by [[NonMovieEventFilteringScraper]], which wraps every
 *  client's `fetch()` at the scrape seam (cf. the per-client filters in
 *  `KinoSfinksClient` / `CharlieMonroeClient`, which this complements rather
 *  than replaces — those discriminate on a structured category the venue
 *  exposes; this one works off the title for venues that expose none). */
object NonMovieEventClassifier {

  /** Word-anchored markers of a live stage/music event. Anchored on `\b` so
   *  `\bteatr` matches "Teatr Skene", "Teatralne popołudnie", "teatru capitol"
   *  but NOT "Kinoteatrze Rialto"; `spektakl` (not followed by a Polish letter)
   *  matches "spektakl komediowy", "Spektakl- Pomoc Domowa", "Spektakl_kubuś…"
   *  but NOT "Społeczeństwo spektaklu" (an inflected form — a real film).
   *  `\bgala\b` ignores "Galaxy"/"Galaktyki" (no boundary after "gala").
   *  `koncert`/`gala`/`balet` are Polish-only spellings, so the English
   *  "Royal Ballet" / "…Opera" of the kept broadcasts never match. */
  private val EventMarkers = List(
    """\bstand[\s-]?up\b""".r,
    """\bkabaret""".r,
    """\brecital""".r,
    """\bspektakl(?![a-ząćęłńóśźż])""".r,  // nominative "spektakl", not "spektakl-u/-em"
    """\boperetk""".r,    // operetka / operetki
    """\bkoncert""".r,    // koncert / koncertu
    """\bteatr""".r,      // teatr / teatru / teatrem / teatralne (NOT kinoteatr…)
    """\bbalet""".r,      // baletowa (PL); English "Ballet" stays
    // Music recitals/tributes beyond a bare "koncert". `symfoniczn` matches the
    // adverb of a pops-symphonic gig ("…symfonicznie") but NOT the real silent
    // film "Nosferatu - symfonia grozy" (no "-czn"); `\btribute`/`\bpamięci`
    // ("…- pamięci Amy Winehouse") tag memorial gigs; `\bjazz`/`\borkiestr`/
    // `\bfilharmoni` are stage-music venues/ensembles. `na żywo` = a live music
    // performance (broadcasts say "transmisja/retransmisja na żywo" → vetoed).
    """symfoniczn""".r,
    """\btribute\b""".r,
    """\bpamięci\b""".r,
    """\bjazz\b""".r,
    """\borkiestr""".r,
    """\bfilharmoni""".r,
    """na żywo""".r,
    // Silesian folk/variety shows. `\bszlagier` covers "Szlagierowa uczta…" and
    // "Szlagierowy zawrót głowy" without a bare `uczta`/`zawrót głowy` that would
    // wrongly drop the films "Uczta Babette" / "Zawrót głowy" (PL Vertigo).
    """\bbiesiad""".r,
    """\bszlagier""".r,
    // Gift vouchers sold through the same ticketing surface ("Bon podarunkowy",
    // collapsed "Bonpodarunkowy"). `\s?` absorbs the lost space.
    """bon\s?podarunkow""".r,
    """bon\s?upominkow""".r,
    """\bvoucher""".r,
    // Mystery / blind screenings ("Seans w ciemno", "Seans niespodzianka",
    // collapsed "seanswciemno") — sold as an event, never resolve to a title.
    """seans\s?w\s?ciemno""".r,
    """seans\s?niespodziank""".r
  )

  /** Festival panels / author meetings ("…– spotkanie z podróżnikiem…", "panel
   *  dyskusyjny", "…– debata z udziałem gości…"). Unlike the markers above these
   *  ALSO appear as an addendum to a real film screening — "The Room + spotkanie
   *  z Gregiem Sestero", "Salto - pokaz + prelekcja", "…+ Q&A z…" — so they only
   *  flag an event when NO film-attachment signal ([[FilmAttachmentSignals]]) is
   *  present. A standalone "spotkanie z X" with no film is dropped; a film with a
   *  bolted-on talk is kept. */
  private val DiscussionMarkers = List(
    """spotkanie ze?\b""".r,
    """panel\s?dyskusyjn""".r,
    """\bdebat""".r,
    """\bprelekcj""".r,
    """\bdyskusj""".r,
    """q&a""".r
  )

  /** Substrings that mark a real film the discussion is bolted onto: a `+`/`|`
   *  joining film and extra, or a screening word. Their presence vetoes the
   *  [[DiscussionMarkers]] verdict (but NOT the concert/folk markers above —
   *  a "Koncert + support" is still an event). */
  private val FilmAttachmentSignals = List("+", "|", "pokaz", "seans", "przedpremiera")
  private def hasBoltedOnFilm(t: String): Boolean = FilmAttachmentSignals.exists(t.contains)
  private def isStandaloneDiscussion(t: String): Boolean =
    DiscussionMarkers.exists(_.findFirstIn(t).isDefined) && !hasBoltedOnFilm(t)

  /** "gala" is handled separately from [[EventMarkers]]: it marks a live event
   *  (Gala Baletowa, Światowa gala muzyczna, Gala Rozdania Nagród) EXCEPT a film
   *  screened under a gala banner, which uses a "Banner | Film" title — e.g.
   *  "Gala Wręczenia Nagrody Wolności | Pieśni lasu" (a real documentary). The
   *  pipe distinguishes the two; `\bgala\b` also ignores "Galaxy"/"Galaktyki". */
  private val GalaWord = """\bgala\b""".r
  private def isStandaloneGala(t: String): Boolean =
    GalaWord.findFirstIn(t).isDefined && !t.contains("|")

  /** Markers that this is screened cinema content (a broadcast or a film
   *  screening), which VETO an event verdict even when an event marker is
   *  present — "André Rieu … letni koncert", "…| TEATR W KINIE | National
   *  Theatre Live", "Pavarotti … Koncert gwiazd z Arena di Verona". */
  private val BroadcastMarkers = List(
    "andre rieu", "andré rieu",
    "pavarotti", "arena di verona",
    "na ekranie",                       // "Sztuka na ekranie - Koncert…"
    "retransmisja", "transmisja",
    "national theatre", "teatr w kinie", "nt live", " live",
    "royal opera", "royal ballet", "sezon kinowy",
    "met opera", "the met",
    "w kinie",                          // "André Rieu W KINIE…"
    "pokaz filmu"                       // "Pokaz filmu „…" i koncert
  )

  /** True when `title` carries a marker of screened "event cinema" — an
   *  opera/ballet/theatre transmission or concert film shown ON the screen
   *  (André Rieu, NT Live, Met Opera, a `retransmisja`). This is legitimate
   *  cinema content that must be KEPT even when other signals (an event-y title,
   *  or a venue's `MusicEvent`/`TheaterEvent` structured type) would otherwise
   *  flag it as a live event — see [[BiletynaClient]], which filters on the
   *  schema.org `@type` and reuses this as its veto. */
  def isScreenedBroadcast(title: String): Boolean =
    BroadcastMarkers.exists(title.toLowerCase.contains)

  /** True when `title` names a live stage/music event rather than a film. */
  def isLiveEvent(title: String): Boolean = {
    val t = title.toLowerCase
    if (isScreenedBroadcast(t)) false
    else
      EventMarkers.exists(_.findFirstIn(t).isDefined) ||
      isStandaloneGala(t) ||
      isStandaloneDiscussion(t)
  }
}
