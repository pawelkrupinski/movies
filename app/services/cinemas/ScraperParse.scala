package services.cinemas

import org.jsoup.nodes.Document

import java.time.{LocalDate, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Parsing snippets shared by the hand-rolled cinema scrapers. Each of these
  * shapes was copy-pasted across several `*Client`s before — keeping one copy
  * here means a fix (or a new quoting/format quirk) lands in every scraper at
  * once. */
private[cinemas] object ScraperParse {
  private val HourMinute = """(\d{1,2}):(\d{2})""".r
  private val CssUrl     = """url\((?:'|"|&quot;)?(.+?)(?:'|"|&quot;)?\)""".r
  private val DayMonthYear = """(\d{1,2})\s+([a-ząćęłńóśźż]+)\s+((?:19|20)\d{2})""".r

  /** Polish genitive month names as the cinema pages spell dates ("5 maja
    * 2023", "12 grudnia"). Shared so the hand-rolled scrapers don't each carry
    * their own copy of the same 12-entry map. */
  val PolishMonths: Map[String, Int] = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4, "maja" -> 5, "czerwca" -> 6,
    "lipca" -> 7, "sierpnia" -> 8, "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

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

  /** Canonical `https://www.youtube.com/watch?v=<id>` form for a YouTube
    * embed / watch / `youtu.be` URL; Vimeo URLs pass through unchanged for the
    * view layer's `TrailerEmbed` to reshape, and anything else is dropped. Each
    * scraper grabs a trailer URL off its own page shape, then funnels it
    * through this single canonicaliser. */
  def canonicalTrailer(url: String): Option[String] =
    services.movies.TrailerEmbed.youTubeId(url).map(id => s"https://www.youtube.com/watch?v=$id")
      .orElse(services.movies.TrailerEmbed.vimeoId(url).map(_ => url))

  /** The first "DD <month> YYYY" Polish date in `text` (e.g. a "Premiera
    * kinowa 5 maja 2023 r." line) as a `LocalDate`. For pages that print the
    * full date with an explicit year; the year-less listing variants keep their
    * own roll-forward logic. */
  def parsePolishDate(text: String): Option[LocalDate] =
    DayMonthYear.findFirstMatchIn(text.toLowerCase).flatMap { m =>
      PolishMonths.get(m.group(2)).flatMap(mon =>
        Try(LocalDate.of(m.group(3).toInt, mon, m.group(1).toInt)).toOption)
    }
}
