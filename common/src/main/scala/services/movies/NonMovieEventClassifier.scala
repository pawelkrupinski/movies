package services.movies

/** Decides whether a scraped listing is a live STAGE/MUSIC event rather than a
 *  film. Small municipal & arthouse venues sell tickets to their own concerts,
 *  stand-up nights, kabaret shows, recitals and theatre plays through the same
 *  ticketing/listing surface their film repertoire comes from, so those rows
 *  leak into the scrape (e.g. "Koncert Joscho Stephan Trio", "Piotr Bałtroczyk
 *  Stand-up", "Edyta Geppert - recital"). They are not movies and shouldn't
 *  reach the read model.
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
 *  Lives behind a pure boolean so `MovieCache.recordCinemaScrape` — the single
 *  point every cinema client's scrape converges on — can drop events for every
 *  client at once (cf. the per-client filters in `KinoSfinksClient` /
 *  `CharlieMonroeClient`, which this complements rather than replaces). */
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
    """\bgala\b""".r
  )

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

  /** True when `title` names a live stage/music event rather than a film. */
  def isLiveEvent(title: String): Boolean = {
    val t = title.toLowerCase
    EventMarkers.exists(_.findFirstIn(t).isDefined) &&
      !BroadcastMarkers.exists(t.contains)
  }
}
