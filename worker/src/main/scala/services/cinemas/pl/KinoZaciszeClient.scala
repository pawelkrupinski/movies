package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Zacisze (Piekary Śląskie). Its repertoire lives on the venue's own
 * WordPress ("Specto" theme) site at `kinozacisze.pl/repertuar/` — plain
 * server-rendered HTML, no JSON-LD and no JSON API, so this is the most fragile
 * of the own-site scrapers. Previously scraped via biletyna.pl, which had
 * stopped carrying the film programme.
 *
 * The page groups screenings by day: one `<div id="YYYY/MM/DD">` per date (the
 * id carries the full date — no year inference), each wrapping one
 * `div.row.movie-tabs` per film showing that day:
 *   - `h3.no-underline`            → title, with a trailing `(YYYY)` release
 *                                    year we split out into `releaseYear`
 *   - `img[alt]` / the poster link → poster URL
 *   - the title link `a[href]`     → the venue's own `/movies/<slug>/` page
 *   - `ul.show-times span.time`    → each `HH:MM`, with the booking link on the
 *                                    span's `onclick` (`window.location.href=…`)
 *
 * The venue also sells tickets to "event cinema" retransmissions (e.g. an
 * André Rieu Maastricht concert) and retrospectives alongside ordinary films,
 * so [[OnlyMovieEventsFilter]] is mixed in: it drops genuine non-film events
 * (concerts, kabaret, stand-up) while keeping broadcasts and art docs.
 */
class KinoZaciszeClient(http: HttpFetch, override val cinema: Cinema = KinoZacisze)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoZaciszeClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoZaciszeClient.RepertoireUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    KinoZaciszeClient.parse(http.get(KinoZaciszeClient.RepertoireUrl), cinema)
}

object KinoZaciszeClient {

  val BaseUrl       = "https://www.kinozacisze.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  // The day-container id, "2026/06/25" → year, month, day.
  private val DatePat = """^(\d{4})/(\d{2})/(\d{2})$""".r
  // A trailing "(YYYY)" release-year tag on the shouted title.
  private val TitleYear = """^(.*?)\s*\((\d{4})\)\s*$""".r
  // The booking URL inside the time span's `onclick="window.location.href='…'"`.
  private val OnClickUrl = """window\.location\.href\s*=\s*'([^']+)'""".r

  private case class RawSlot(
    title:     String,
    year:      Option[Int],
    dateTime:  LocalDateTime,
    booking:   Option[String],
    filmUrl:   Option[String],
    poster:    Option[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div[id~=^\\d{4}/\\d{2}/\\d{2}$]").asScala.toSeq
      .flatMap(parseDay)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (_, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title = head.title, releaseYear = head.year),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  /** Every screening slot under one `<div id="YYYY/MM/DD">` day container —
   *  the id supplies the date, each `div.row.movie-tabs` a film and its times. */
  private def parseDay(day: Element): Seq[RawSlot] =
    day.id() match {
      case DatePat(year, month, dom) =>
        day.select("div.row.movie-tabs").asScala.toSeq
          .flatMap(parseFilm(_, year.toInt, month.toInt, dom.toInt))
      case _ => Seq.empty
    }

  /** One film's slots for a given day: its title/poster/link plus one
   *  [[RawSlot]] per `span.time`. */
  private def parseFilm(block: Element, year: Int, month: Int, dom: Int): Seq[RawSlot] = {
    val rawTitle = Option(block.selectFirst("h3.no-underline")).map(_.text.trim).filter(_.nonEmpty)
    rawTitle.toSeq.flatMap { raw =>
      val (title, parsedYear) = raw match {
        case TitleYear(t, y) => (t.trim, y.toIntOption)
        case _               => (raw, None)
      }
      val filmUrl = Option(block.selectFirst(".col-md-2 a[href], a.arrow-button"))
        .map(_.attr("abs:href")).filter(_.nonEmpty)
      val poster = Option(block.selectFirst("img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)

      block.select("ul.show-times span.time").asScala.toSeq.flatMap { span =>
        for {
          time <- ScraperParse.parseHHmm(span.text)
          dt   <- Try(LocalDateTime.of(year, month, dom, time.getHour, time.getMinute)).toOption
        } yield RawSlot(
          title    = title,
          year     = parsedYear,
          dateTime = dt,
          booking  = OnClickUrl.findFirstMatchIn(span.attr("onclick")).map(_.group(1)),
          filmUrl  = filmUrl,
          poster   = poster
        )
      }
    }
  }
}
