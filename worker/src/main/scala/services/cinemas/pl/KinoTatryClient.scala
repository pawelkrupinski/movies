package services.cinemas.pl

import scala.math.Ordering.Implicits.infixOrderingOps
import services.cinemas.common.CinemaScraper

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
 * The whole currently-scheduled slate lives inline on the HOMEPAGE ("START |
 * REPERTUAR KINA"): a carousel of cards, each an `h3.title-article` title, a
 * `ul.harmonogram-list` of `<li><strong>DD/MM/YYYY:</strong> HH:MM</li>` slots
 * (a `li` may carry several comma-separated times), an `a.btn-theme` link to the
 * film's `/repertuar/<slug>/` detail page, and an `img.wp-post-image` poster.
 *
 * The homepage is curated by the cinema to its current programme and drops a
 * card once its screenings pass, so reading it directly is both complete and
 * up-to-date. (The earlier design walked the `/repertuar/` archive newest-first,
 * assuming the newest posts were the scheduled ones — but the venue re-publishes
 * old film posts when it re-screens them, so the archive's newest entries are
 * NOT the current slate; every listed film aged out and the scrape went empty.)
 * Per-card showtimes are still filtered to `>= today`, so a straggler whose only
 * date is in the past yields no showtimes and is dropped.
 *
 * The homepage card has no release year, but this is a repertory cinema showing
 * classics (Pianista 2002, Possession 1981, Basic Instinct 1992) where the year
 * is the disambiguator that stops TMDB matching a same-titled remake — so each
 * listed film's `/repertuar/<slug>/` detail page (a handful, fetched in
 * parallel) is read for its `Premiera</strong>YYYY` line.
 */
class KinoTatryClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoTatryClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(HomepageUrl)

  def fetch(): Seq[CinemaMovie] = {
    val cards = parseHomepage(Try(http.get(HomepageUrl)).getOrElse(""), today, cinema)
    if (cards.isEmpty) return Seq.empty

    val detailUrls = cards.flatMap(_.filmUrl).distinct
    val yearByUrl = ParallelDetailFetch.keyed("kino-tatry-detail", detailUrls, 1.minute)(identity) { url =>
      Try(http.get(url)).toOption.flatMap(yearOf)
    }
    cards.map { card =>
      val year = card.filmUrl.flatMap(yearByUrl.getOrElse(_, None))
      card.copy(movie = card.movie.copy(releaseYear = year))
    }
  }
}

object KinoTatryClient {

  val BaseUrl     = "https://kinotatrylodz.pl"
  val HomepageUrl = s"$BaseUrl/"

  private val DateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private val DatePat    = """(\d{2}/\d{2}/\d{4})""".r
  private val TimePat    = """(\d{1,2}:\d{2})""".r
  private val YearPat    = """Premiera\D{0,12}(\d{4})""".r

  /** The release year mined from a `/repertuar/<slug>/` detail page's
   *  `Premiera</strong>YYYY` line, if present. */
  private[cinemas] def yearOf(html: String): Option[Int] =
    YearPat.findFirstMatchIn(html).map(_.group(1).toInt)

  /** Parse the homepage repertoire carousel into `CinemaMovie`s. Each
   *  `h3.title-article` heading anchors one film card; its showtimes, detail
   *  link and poster are read from that card. Films with no future-dated
   *  `harmonogram-list` entry yield no showtimes and are dropped. */
  private[cinemas] def parseHomepage(html: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    document.select("h3.title-article").asScala.toSeq.flatMap { heading =>
      val title = heading.text.trim
      Option(heading.parent()).filter(_ => title.nonEmpty).flatMap { card =>
        val showtimes = card.select("ul.harmonogram-list li").asScala.toSeq
          .flatMap(slotTimes(_, today))
          .distinct
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None
        else Some(CinemaMovie(
          movie       = Movie(title),
          cinema      = cinema,
          posterUrl   = Option(card.selectFirst("img.wp-post-image")).map(_.absUrl("src")).filter(_.nonEmpty),
          filmUrl     = Option(card.selectFirst("a.btn-theme[href]")).map(_.absUrl("href")).filter(_.nonEmpty),
          synopsis    = None,
          cast        = Seq.empty,
          director    = Seq.empty,
          showtimes   = showtimes
        ))
      }
    }.sortBy(_.movie.title)
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
