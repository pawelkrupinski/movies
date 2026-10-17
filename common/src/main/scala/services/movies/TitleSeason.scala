package services.movies

/** A SEASON written into a title — "2026/27", "2026-27", "2026/2027", "2026–2027", and a box office's short "2026/7"
 *  (prod UK "Royal Ballet 2026/7: Romeo & Juliet"): a year and the next, which is how a broadcast or programme series
 *  names its run, never a film's year. Two digits alone are none: an age band ("Kids Club 12/13"), or a banner left from
 *  the season it was first billed in — DE "Berliner Philharmoniker 25/26" screens the 2026 Silvesterkonzert, and read as
 *  2025 held ×73 spellings of it apart; a relay's "RB&O 26/27" takes its season from its screenings. Nor a date a month
 *  runs on from ("2026-7-15"). Read by the identity model (a listing's season, a
 *  record's) and by a venue's slots (two seasons' productions of one work are two films), so both read one season. */
object TitleSeason {
  private val Season = """(?<![\p{N}])((?:18|19|20)\d{2})\s*[/–—-]\s*(\d{4}|\d{1,2})(?![\p{N}]|[-/.]\d)""".r

  /** The season's first year, when the match is one: a year and the very next. */
  private def start(m: scala.util.matching.Regex.Match): Option[Int] = {
    val first = m.group(1).toInt
    val end   = m.group(2)
    Option.when(end.toInt == (first + 1) % math.pow(10, end.length).toInt)(first)
  }

  /** A season — or any year read off a title — needs an ASCII digit (`\d` is ASCII-only in Java): a title without one
   *  skips the regex. */
  private[services] def hasAsciiDigit(t: String): Boolean = t.exists(c => c >= '0' && c <= '9')

  /** The one season `titles` name, by its first year; none when they name none, or two. A record's titles: any season
   *  it was billed in. */
  def of(titles: Seq[String]): Option[Int] = of(titles, None)

  /** The one season a LISTING's `titles` name, screening in the season `screenedIn` (the identity model's
   *  `ScreeningDays.season`): only one a cinema could relay then is a season — from [[RelayEraSeasons]] before its
   *  screenings (an encore of a past season's capture: DE "Royal Opera House 2017/18: Alice im Wunderland" screening in
   *  2026) to the next. A span of years further off is the title's own text: DE "Warschau 1944/45" bills no 1944 season,
   *  and read as one denied every film not dated 1944–45 and read the listing as a relay. */
  def of(titles: Seq[String], screenedIn: Option[Int]): Option[Int] =
    titles.iterator.filter(hasAsciiDigit).flatMap(Season.findAllMatchIn).flatMap(start)
      .filter(season => screenedIn.forall(screened => near(season, screened))).toSeq.distinct match {
      case Seq(one) => Some(one)
      case _        => None
    }

  /** How many seasons back a listing's season may be: cinema relays began with the Met's 2006/07 Live in HD, twenty
   *  seasons before 2026's, and venues still screen captures from their first seasons. */
  val RelayEraSeasons = 20

  /** Can a listing screening in season `screened` bill season `season`: one of the relay era's ([[RelayEraSeasons]]
   *  back), its own or the next? */
  def near(season: Int, screened: Int): Boolean = season >= screened - RelayEraSeasons && season <= screened + 1

  /** `t` with every season removed, so a season's end year is never read as a bracketed year. */
  def without(t: String): String =
    if (!hasAsciiDigit(t)) t else Season.replaceAllIn(t, m => if (start(m).isDefined) " " else scala.util.matching.Regex.quoteReplacement(m.matched))
}
