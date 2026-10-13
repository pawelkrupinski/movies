package services.movies

/** A SEASON written into a title — "2026/27", "2026-27", "2026/2027", "2026–2027", and a box office's short "2026/7"
 *  (prod UK "Royal Ballet 2026/7: Romeo & Juliet"): a year and the next, which is how a broadcast or programme series
 *  names its run, never a film's year. Two digits alone are none: an age band ("Kids Club 12/13"), or a banner left from
 *  the season it was first billed in — DE "Berliner Philharmoniker 25/26" screens the 2026 Silvesterkonzert, and read as
 *  2025 held ×73 spellings of it apart; a relay's "RB&O 26/27" takes its season from its screenings. Read by the identity model (a listing's season, a
 *  record's) and by a venue's slots (two seasons' productions of one work are two films), so both read one season. */
object TitleSeason {
  private val Season = """(?<![\p{N}])((?:18|19|20)\d{2})\s*[/–—-]\s*(\d{4}|\d{1,2})(?![\p{N}])""".r

  /** The season's first year, when the match is one: a year and the very next. */
  private def start(m: scala.util.matching.Regex.Match): Option[Int] = {
    val first = m.group(1).toInt
    val end   = m.group(2)
    Option.when(end.toInt == (first + 1) % math.pow(10, end.length).toInt)(first)
  }

  /** A season needs an ASCII digit (`\d` is ASCII-only in Java): a title without one skips the regex. */
  private def hasAsciiDigit(t: String): Boolean = t.exists(c => c >= '0' && c <= '9')

  /** The one season `titles` name, by its first year; none when they name none, or two. */
  def of(titles: Seq[String]): Option[Int] =
    titles.iterator.filter(hasAsciiDigit).flatMap(Season.findAllMatchIn).flatMap(start).toSeq.distinct match {
      case Seq(one) => Some(one)
      case _        => None
    }

  /** `t` with every season removed, so a season's end year is never read as a bracketed year. */
  def without(t: String): String =
    if (!hasAsciiDigit(t)) t else Season.replaceAllIn(t, m => if (start(m).isDefined) " " else scala.util.matching.Regex.quoteReplacement(m.matched))
}
