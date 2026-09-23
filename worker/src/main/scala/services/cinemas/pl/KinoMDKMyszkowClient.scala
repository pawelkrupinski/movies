package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The cinema run by MDK (Miejski Dom Kultury), Myszków. A WordPress site on
 * the Events Manager plugin, whose `kino` category page
 * (`/wydarzenia/kategorie/kino/`) lists upcoming event links but no
 * structured showtime data of its own (Events Manager's structured "Data"
 * field only ever gives the run's date RANGE) — the real per-day showtime is
 * hand-typed prose further down each event's own detail page, e.g.
 * `<h1><strong>25, 26, 27.09.2026</strong></h1>` + `<h1><strong>godz.
 * 18:00</strong></h1>`, so this is a two-step scrape: list the category page
 * for event URLs, then read each one's free text.
 *
 * Per detail page (`.em-event-single`):
 *   - `h1.entry-title`                → title
 *   - the comma day-list + `.MM.YYYY` line → the screening dates (one time of
 *     day shared by every date named)
 *   - `godz. HH:MM`                   → the time
 *   - `czas trwania: NN min.`         → runtime
 *   - the longest non-boilerplate `<p>` → synopsis (a "źródło: filmweb.pl"
 *     credit confirms this venue sources its blurb from Filmweb)
 *   - the first `img[src]`            → poster
 *
 * A tiny venue (currently a single event on the category page, confirmed the
 * only one through the end of October) — verified as a genuine film
 * screening (not a charity/festival event, despite the title) by its
 * explicit runtime, Filmweb source credit and priced tickets. No online
 * ticketing found anywhere on either page — door sales only, so no booking
 * URL is set.
 */
class KinoMDKMyszkowClient(http: HttpFetch, override val cinema: Cinema = KinoMDKMyszkow) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoMDKMyszkowClient.CategoryUrl)
  override def sourceUrl: Option[String] = Some(KinoMDKMyszkowClient.CategoryUrl)

  def fetch(): Seq[CinemaMovie] = {
    val urls = KinoMDKMyszkowClient.eventUrls(http.get(KinoMDKMyszkowClient.CategoryUrl))
    urls.flatMap(url => KinoMDKMyszkowClient.parseEvent(http.get(url), url, cinema))
  }
}

object KinoMDKMyszkowClient {

  val BaseUrl     = "https://www.mdk-myszkow.pl"
  val CategoryUrl = s"$BaseUrl/wydarzenia/kategorie/kino/"

  // "25, 26, 27.09.2026" — a comma day-list, then the shared month.year.
  private val DateListPat = """([\d,\s]+)\.(\d{1,2})\.(\d{4})""".r
  private val RuntimePat  = """czas trwania:\s*(\d+)\s*min""".r
  private val TimePat     = """godz\.?\s*(\d{1,2}:\d{2})""".r

  private val BoilerplateStarts = Seq("czas trwania", "źródło", "bilety", "data")

  def eventUrls(html: String): Seq[String] =
    Jsoup.parse(html, BaseUrl).select(".em-category-single a[href*=/wydarzenia/]").asScala.toSeq
      .map(_.attr("abs:href")).filter(_.nonEmpty).distinct

  def parseEvent(html: String, url: String, cinema: Cinema): Option[CinemaMovie] = {
    val doc   = Jsoup.parse(html, url)
    val block = Option(doc.selectFirst(".em-event-single"))
    val text  = block.map(_.text).getOrElse("")

    for {
      title  <- Option(doc.selectFirst("h1.entry-title")).map(_.text.trim).filter(_.nonEmpty)
      dateM  <- DateListPat.findFirstMatchIn(text)
      month  = dateM.group(2).toInt
      year   = dateM.group(3).toInt
      days   = dateM.group(1).split(",").toSeq.flatMap(_.trim.toIntOption)
      dates  = days.flatMap(d => Try(LocalDate.of(year, month, d)).toOption)
      if dates.nonEmpty
      time   <- TimePat.findFirstMatchIn(text).flatMap(m => ScraperParse.parseHHmm(m.group(1)))
    } yield CinemaMovie(
      movie     = Movie(title, runtimeMinutes = RuntimePat.findFirstMatchIn(text).map(_.group(1).toInt)),
      cinema    = cinema,
      posterUrl = block.flatMap(b => Option(b.selectFirst("img[src]"))).map(_.attr("abs:src")).filter(_.nonEmpty),
      filmUrl   = Some(url),
      synopsis  = block.flatMap(synopsisOf),
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = dates.map(d => Showtime(LocalDateTime.of(d, time), None)).sortBy(_.dateTime)
    )
  }

  /** The longest `<p>` in the event block that isn't one of its boilerplate
   *  fields (runtime, source credit, price, the structured date range). */
  private def synopsisOf(block: Element): Option[String] =
    block.select("p").asScala.toSeq
      .map(_.text.trim)
      .filter(t => t.length > 60 && !BoilerplateStarts.exists(t.toLowerCase.startsWith))
      .sortBy(-_.length).headOption
}
