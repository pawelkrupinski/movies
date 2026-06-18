package services.cinemas

import scala.math.Ordering.Implicits.infixOrderingOps

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Tatry — the independent repertory cinema at ul. Sienkiewicza 40 in Łódź.
 * Filmweb stopped carrying its (sparse) programme, so it's scraped straight from
 * the venue's own WordPress site (`kinotatrylodz.pl`, the `ergotree` theme).
 *
 * The repertoire archive at `/repertuar/` lists film posts newest-first, each
 * linking to a `/repertuar/<slug>/` detail page. Showtimes live ONLY on the
 * detail page, in a `ul.harmonogram-list` whose `<li>` items read
 * `<strong>DD/MM/YYYY:</strong> HH:MM` (a `li` may carry several comma-separated
 * times). The list page carries titles but no dates, so each film's schedule is
 * read from its detail page (fetched in parallel, tolerantly).
 *
 * Scope: the FIRST archive page only. This is a low-frequency club cinema — a
 * new screening is published as a fresh post, so the currently-scheduled films
 * are always the newest entries; older pages are past films with no upcoming
 * dates. Films whose detail page has no future-dated `harmonogram-list` entry
 * (the bulk of the archive) yield no showtimes and are dropped.
 */
class KinoTatryClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoTatryClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    val detailUrls = filmUrls(Try(http.get(RepertoireUrl)).getOrElse(""))
    if (detailUrls.isEmpty) return Seq.empty

    val byUrl = ParallelDetailFetch.keyed("kino-tatry-detail", detailUrls, 1.minute)(identity) { url =>
      Try(http.get(url)).toOption.flatMap(html => parseDetail(html, url, today, cinema))
    }
    detailUrls.flatMap(byUrl.getOrElse(_, None)).sortBy(_.movie.title)
  }
}

object KinoTatryClient {

  val BaseUrl       = "https://kinotatrylodz.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val DetailPath = """^https://kinotatrylodz\.pl/repertuar/[a-z0-9-]+/$""".r
  private val DateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private val DatePat    = """(\d{2}/\d{2}/\d{4})""".r
  private val TimePat    = """(\d{1,2}:\d{2})""".r
  private val YearPat    = """Premiera\D{0,12}(\d{4})""".r

  /** Distinct `/repertuar/<slug>/` detail-page URLs linked from the archive
   *  listing, in document order (the listing's own pagination/`/repertuar/`
   *  links are filtered out by the strict slug pattern). */
  private[cinemas] def filmUrls(listingHtml: String): Seq[String] =
    Jsoup.parse(listingHtml, BaseUrl).select("a[href]").asScala.toSeq
      .map(_.absUrl("href"))
      .collect { case u @ DetailPath() => u }
      .distinct

  /** Parse one `/repertuar/<slug>/` detail page into a `CinemaMovie`, or `None`
   *  when it has no future-dated screening (an archive film). */
  private[cinemas] def parseDetail(html: String, url: String, today: LocalDate, cinema: Cinema): Option[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val title = Option(document.selectFirst("div.content h2")).orElse(Option(document.selectFirst("h2")))
      .map(_.text.trim).filter(_.nonEmpty)

    title.flatMap { t =>
      val showtimes = document.select("ul.harmonogram-list li").asScala.toSeq
        .flatMap(slotTimes(_, today))
        .distinct
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie       = Movie(t, releaseYear = YearPat.findFirstMatchIn(html).map(_.group(1).toInt)),
        cinema      = cinema,
        posterUrl   = Option(document.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty),
        filmUrl     = Some(url),
        synopsis    = None,
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = showtimes
      ))
    }
  }

  /** One `harmonogram-list` `<li>` → its future-dated `Showtime`s. The `<strong>`
   *  holds `DD/MM/YYYY:`; the remaining text holds one or more `HH:MM` times. */
  private def slotTimes(li: Element, today: LocalDate): Seq[Showtime] = {
    val date = Option(li.selectFirst("strong")).map(_.text)
      .flatMap(DatePat.findFirstMatchIn).map(_.group(1))
      .flatMap(d => Try(LocalDate.parse(d, DateFormat)).toOption)
      .filter(_ >= today)
    date.toSeq.flatMap { d =>
      TimePat.findAllMatchIn(li.ownText).flatMap(m => ScraperParse.parseHHmm(m.group(1)))
        .map(t => Showtime(LocalDateTime.of(d, t), bookingUrl = None)).toSeq
    }
  }
}
