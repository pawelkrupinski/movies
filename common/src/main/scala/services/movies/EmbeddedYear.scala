package services.movies

/** A release year written into a title as a DELIMITED annotation: parenthesised
 *  or bracketed `(YYYY)` / `[YYYY]` / `{YYYY}` / `<YYYY>` anywhere in the string,
 *  or a trailing separator form `… - YYYY` / `… , YYYY`. A BARE year is never
 *  read — a space-separated or in-word number is title content, not an
 *  annotation ("Blade Runner 2049", "1917", "2001: Odyseja kosmiczna"), and every
 *  bare-year screening in the corpus is a festival/concert edition
 *  ("SUMMER FALL FESTIVAL 2026", "André Rieu … Letni koncert 2026") whose year is
 *  the event, not a film. So only a delimited shape counts.
 *
 *  Used two ways, both PURE functions of the title:
 *   - as a row's LOOKUP year when the scrape carries none, so a yearless
 *     retrospective ("Klasyka w NCKF: Generał (1926) 4K", "Konwicki: Lawa (1989)")
 *     resolves via the year-scoped exact-title path instead of stalling at the
 *     year-less singleton guard (see `MovieService.resolveTmdbId`); and
 *   - PERSISTED at the scrape boundary (`MovieCache.recordCinemaScrape`) as the
 *     row's release year, so a scrape that reported no year still keys, resolves,
 *     and displays as if the cinema had shipped the year — the deterministic
 *     scrape-path re-key `canonicalRank` already reconciles (never the async
 *     resolve path, which would race it).
 *
 *  Abstains (returns None) when there's no delimited year, when it's outside the
 *  plausible film range [1888, maxYear], or when SEVERAL distinct years appear
 *  across the given titles (ambiguous). A wrong/absent year then costs at most a
 *  MISS, never a mis-key or mis-resolve.
 *
 *  Scans SEVERAL spellings, not one: the canonical merge key strips the
 *  annotation (`ExtraTitleRules.xtra-canonical-trailing-paren-year`), so the year
 *  survives only on the raw cinema-slot titles / search candidates. */
object EmbeddedYear {
  // Balanced brackets anywhere (groups 1–4), then a trailing separator form
  // (group 5) that REQUIRES a comma / hyphen / en- or em-dash before the year so a
  // bare trailing number ("… 2049") is left alone. No global `(?i)` — digits are
  // case-insensitive anyway, and the char class stays literal.
  private val Delimited =
    """\((\d{4})\)|\[(\d{4})\]|\{(\d{4})\}|<(\d{4})>|[,–—-]\s*(\d{4})\s*$""".r

  private val FirstFilmYear = 1888

  /** Varargs convenience for the common "scan these few title spellings" call. */
  def of(titles: String*): Option[Int] = ofAll(titles)

  def ofAll(titles: Iterable[String], maxYear: Int = java.time.Year.now().getValue + 1): Option[Int] = {
    val years = titles.iterator
      .flatMap(Delimited.findAllMatchIn)
      .map(m => (1 to m.groupCount).iterator.flatMap(i => Option(m.group(i))).next().toInt)
      .filter(y => y >= FirstFilmYear && y <= maxYear)
      .toSeq.distinct
    years match {
      case Seq(single) => Some(single)
      case _           => None
    }
  }
}
