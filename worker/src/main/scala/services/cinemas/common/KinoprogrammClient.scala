package services.cinemas.common

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * One German venue's programme from www.kinoprogramm.com — the FALLBACK behind
 * Filmstarts ([[WebediaShowtimesClient]]), served by [[SourceFallbackScraper]]
 * only once a venue's own Filmstarts scrape has failed several separate runs.
 * Sampled against Filmstarts on 20 venues (2026-09-26): 96% of showtimes matched
 * on date, time and title, and 99% agreed on the language version.
 *
 * A venue page (`/kino/<town>/<venue>-<id>`) is a 7-day grid, server-rendered:
 * one `article[data-kino-week-film]` per film, one `section[data-kino-week-day]`
 * per day, and inside a day one `[data-kino-week-version]` block per language
 * version holding `span.kino-week-time` times. `?datum=YYYY-MM-DD` serves the 7
 * days from that date, so the whole horizon is walked a week at a time until
 * [[ScrapeHorizon.MaxEmptyWeeks]] blank weeks in a row — never a fixed window.
 *
 * The page carries no booking link, auditorium or original title; the fallback
 * serves what the page has, and TMDB enriches the rest downstream.
 */
class KinoprogrammClient(
  http: HttpFetch,
  path: String,                 // the venue page's path, e.g. "/kino/hannover/kino-am-raschplatz-60676"
  override val cinema: Cinema,
  /** The day the horizon is measured from; `None` means today in Germany. */
  today: Option[LocalDate] = None
) extends CinemaScraper {
  import KinoprogrammClient._

  private val referenceDay: LocalDate = today.getOrElse(LocalDate.now(Zone))

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  override def sourceUrl: Option[String] = Some(BaseUrl + path)

  def fetch(): Seq[CinemaMovie] = {
    val films = Seq.newBuilder[Film]
    // A week that fails to fetch or parse counts as blank; only a walk on which
    // EVERY week failed fails the scrape (`ScrapeHorizon` rethrows), so one bad
    // week still lets the rest of the programme through.
    ScrapeHorizon.liveWeeks(referenceDay) { weekStart =>
      val week = parseWeek(http.get(weekUrl(weekStart)))
        .map(film => film.copy(showtimes = film.showtimes.filterNot(_.dateTime.toLocalDate.isBefore(referenceDay))))
        .filter(_.showtimes.nonEmpty)
      films ++= week
      week.nonEmpty
    }
    toMovies(films.result(), cinema, sourceUrl)
  }

  private def weekUrl(weekStart: LocalDate): String = s"$BaseUrl$path?datum=$weekStart"
}

object KinoprogrammClient {
  val BaseUrl: String = "https://www.kinoprogramm.com"
  val Zone: ZoneId    = ZoneId.of("Europe/Berlin")

  /** One film's screenings on one week page, before films are merged across weeks. */
  private[common] final case class Film(
    title:          String,
    filmPath:       Option[String],
    runtimeMinutes: Option[Int],
    genres:         Seq[String],
    ageRating:      Option[String],
    showtimes:      Seq[Showtime]
  )

  /** The language-version keys the page uses, as the tokens Filmstarts emits for
   *  the same versions ([[WebediaMarket.Germany]]). "deutsch" is either a German
   *  film or a German dub, which the page does not tell apart, so it gets NO
   *  token — the unmarked default, as a German film has on Filmstarts. An
   *  unknown key also gets none rather than failing the week. */
  private val VersionTokens: Map[String, String] = Map(
    "original-mit-untertiteln"          -> "OmU",
    "original-mit-englischen-untertiteln" -> "OmeU",
    "original"                          -> "OV",
    "originalfassung"                   -> "OV"
  )

  private val Runtime = """(?iu)Laufzeit:\s*(\d+)\s*Min""".r
  private val Fsk     = """(?iu)\bFSK\s*(\d+)\b""".r
  private val Time    = """(\d{1,2}):(\d{2})""".r

  /** Every film on one week page. A page without the programme list is not an
   *  empty week but a changed layout or a block page, so it throws; a list with
   *  no films is a genuinely empty week. */
  private[common] def parseWeek(html: String): Seq[Film] = {
    val doc: Document = Jsoup.parse(html, BaseUrl)
    if (doc.selectFirst("[data-kino-week-list]") == null)
      throw new IllegalStateException("kinoprogramm.com page has no [data-kino-week-list] — layout changed or blocked?")
    doc.select("article[data-kino-week-film]").asScala.toSeq.flatMap(parseFilm)
  }

  private def parseFilm(article: Element): Option[Film] = {
    // The poster link wraps an image and has no text; the title link is the one with text.
    val titleLink = article.select("a[href]").asScala.find(_.text.trim.nonEmpty)
    titleLink.map { link =>
      val meta  = Option(article.selectFirst("p")).map(_.text).getOrElse("")
      val parts = meta.split('·').map(_.trim).filter(_.nonEmpty).toSeq
      Film(
        title          = link.text.trim,
        filmPath       = Option(link.attr("href")).filter(_.startsWith("/")),
        runtimeMinutes = Runtime.findFirstMatchIn(meta).map(_.group(1).toInt),
        genres         = parts.headOption.filterNot(p => Fsk.findFirstIn(p).isDefined || Runtime.findFirstIn(p).isDefined)
                           .toSeq.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty)),
        ageRating      = Fsk.findFirstMatchIn(meta).map(m => s"FSK ${m.group(1)}").flatMap(AgeRating.normalize(_)),
        showtimes      = article.select("section[data-kino-week-day]").asScala.toSeq.flatMap(parseDay)
      )
    }
  }

  private def parseDay(day: Element): Seq[Showtime] =
    Try(LocalDate.parse(day.attr("data-kino-week-day"))).toOption.toSeq.flatMap { date =>
      day.select("[data-kino-week-version]").asScala.toSeq.flatMap { version =>
        val format = VersionTokens.get(version.attr("data-kino-week-version")).toList
        version.select("span.kino-week-time").asScala.toSeq.flatMap(time => parseTime(time.text)).map(t =>
          Showtime(LocalDateTime.of(date, t), bookingUrl = None, format = format))
      }
    }

  private def parseTime(text: String): Option[LocalTime] =
    Time.findFirstMatchIn(text).flatMap(m => Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)

  /** Merge a venue's films across the weeks walked: one `CinemaMovie` per film —
   *  keyed by the film's own page, since two films can share a title — showtimes
   *  de-duplicated and in time order. */
  private[common] def toMovies(films: Seq[Film], cinema: Cinema, venueUrl: Option[String]): Seq[CinemaMovie] =
    films.groupBy(film => film.filmPath.getOrElse(film.title)).toSeq.map { case (_, same) =>
      val first = same.head
      CinemaMovie(
        movie       = Movie(first.title, runtimeMinutes = same.flatMap(_.runtimeMinutes).headOption,
                            genres = same.flatMap(_.genres).distinct),
        cinema      = cinema,
        posterUrl   = None,
        filmUrl     = first.filmPath.map(BaseUrl + _).orElse(venueUrl),
        synopsis    = None,
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = same.flatMap(_.showtimes).distinctBy(s => (s.dateTime, s.format)).sortBy(_.dateTime),
        ageRating   = same.flatMap(_.ageRating).headOption
      )
    }.sortBy(_.movie.title)
}
