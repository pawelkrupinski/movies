package services.cinemas

import org.jsoup.nodes.Document

import java.time.LocalTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Parsing snippets shared by the hand-rolled cinema scrapers. Each of these
  * shapes was copy-pasted across several `*Client`s before — keeping one copy
  * here means a fix (or a new quoting/format quirk) lands in every scraper at
  * once. */
private[cinemas] object ScraperParse {
  private val HourMinute = """(\d{1,2}):(\d{2})""".r
  private val CssUrl     = """url\((?:'|"|&quot;)?(.+?)(?:'|"|&quot;)?\)""".r

  /** Polish genitive month names as the cinema pages spell dates ("5 maja",
    * "12 grudnia"). Shared so the hand-rolled scrapers don't each carry their
    * own copy of the same 12-entry map. */
  val PolishMonths: Map[String, Int] = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4, "maja" -> 5, "czerwca" -> 6,
    "lipca" -> 7, "sierpnia" -> 8, "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

  /** Polish three-letter month abbreviations as several cinema pages spell them
    * ("10 Cze 2026", "5 paź"). Keyed lower-case; [[polishMonthAbbrev]] folds case
    * so a page can capitalise them ("Cze") or not ("cze"). Shared so the
    * hand-rolled scrapers (Kino Żak, the MSI portals, Kijów, Kinomuzeum, Praha)
    * don't each carry their own copy of the same 12-entry map. */
  private val PolishMonthAbbrevs: Map[String, Int] = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )

  /** The month number for a Polish three-letter abbreviation, case-insensitively
    * ("Cze"/"cze" → 6); `None` for anything not a known abbreviation. */
  def polishMonthAbbrev(token: String): Option[Int] =
    PolishMonthAbbrevs.get(token.trim.toLowerCase)

  /** The first `HH:mm` in `s` as a `LocalTime`, or `None` when there's no
    * match or the captured hour/minute is out of range. */
  def parseHHmm(s: String): Option[LocalTime] =
    HourMinute.findFirstMatchIn(s)
      .flatMap(m => Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)

  /** The URL inside a CSS `url(...)` value, unwrapping `'`, `"` or `&quot;`
    * quoting. `None` when `s` holds no `url(...)`. */
  def cssUrl(s: String): Option[String] =
    CssUrl.findFirstMatchIn(s).map(_.group(1))

  /** Text of the `<dd>` immediately after the `<dt>` whose text contains
    * `label` (case-insensitive), searched within `dtSelector`. Trimmed; empty
    * → `None`. The Drupal-style cinema sites render film metadata as such
    * definition lists. */
  def ddField(doc: Document, label: String, dtSelector: String = "dt"): Option[String] =
    doc.select(dtSelector).asScala
      .find(_.text.toLowerCase.contains(label))
      .flatMap(dt => Option(dt.nextElementSibling))
      .filter(_.tagName == "dd")
      .map(_.text.trim)
      .filter(_.nonEmpty)

  /**
   * Convert an ALL-CAPS title to sentence case. Lowercases everything, then
   * uppercases the first character and any letter after `. ` where the
   * preceding token is a sentence-ending token (digit, or a 4+-letter word).
   * Keeps "Mavka. Prawdziwy mit" and "90. Urodziny pavarottiego" correct while
   * leaving Polish abbreviations like "ang." / "reż." untouched. Used by the MSI
   * scrapers (Cinema1, Kino Zamek, Kino Kijów) whose portals serve ALL-CAPS titles.
   */
  def sentenceCase(title: String): String = {
    if (title.isEmpty) return title
    val chars = title.toLowerCase.toCharArray
    chars(0) = chars(0).toUpper
    var i = 0
    while (i + 2 < chars.length) {
      if (chars(i) == '.' && chars(i + 1) == ' ' && precedingTokenEndsSentence(chars, i))
        chars(i + 2) = chars(i + 2).toUpper
      i += 1
    }
    new String(chars)
  }

  private def precedingTokenEndsSentence(chars: Array[Char], dotIdx: Int): Boolean = {
    if (dotIdx == 0) return false
    val prev = chars(dotIdx - 1)
    if (prev.isDigit) return true
    if (!prev.isLetter) return false
    var len = 0
    var j = dotIdx - 1
    while (j >= 0 && chars(j).isLetter) { len += 1; j -= 1 }
    len >= 4
  }

  /** Canonical `https://www.youtube.com/watch?v=<id>` form for a YouTube
    * embed / watch / `youtu.be` URL; Vimeo URLs pass through unchanged for the
    * view layer's `TrailerEmbed` to reshape, and anything else is dropped. Each
    * scraper grabs a trailer URL off its own page shape, then funnels it
    * through this single canonicaliser. */
  def canonicalTrailer(url: String): Option[String] =
    services.movies.TrailerEmbed.youTubeId(url).map(id => s"https://www.youtube.com/watch?v=$id")
      .orElse(services.movies.TrailerEmbed.vimeoId(url).map(_ => url))
}
