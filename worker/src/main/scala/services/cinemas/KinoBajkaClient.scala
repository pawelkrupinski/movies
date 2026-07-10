package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json.{JsObject, Json}
import services.movies.TitleNormalizer
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Kino Bajka (Lublin) — the art-house cinema run by Centrum Kultury, at
 * ul. Radziszewskiego 8. Its WordPress repertoire page at `/repertuar/` no
 * longer server-renders the schedule as HTML; the whole advance window now
 * ships as an HTML-entity-encoded JSON blob in the `data-dane` attribute of the
 * page's `<div id="rep2">`, which the site's `rep2` widget `JSON.parse`s
 * client-side to build the day blocks. No AJAX round-trip — a single page fetch
 * still carries every screening, so the scraper just reads the attribute
 * (jsoup entity-decodes it) and parses the JSON.
 *
 * The blob is `{ "buy": <booking-base-url>, "dni": { "YYYY-MM-DD": [film, …] } }`
 * where each film carries:
 *   - `t` → title (cleaned via the `kino-bajka` title rules).
 *   - `u` → the film's detail-page URL (`filmUrl`).
 *   - `p` → poster URL.
 *   - `m` → a "genres · format · NNN min" caption; the trailing `NNN min` is the
 *     runtime (country isn't published here — TMDB enriches it downstream).
 *   - `s` → one entry per showtime: `g` (`HH:MM`, paired with the day's date),
 *     `h` (hall code) and `x` (1 for a past screening). Past screenings are kept
 *     — the day carries the year so they resolve to the right `LocalDateTime`.
 * The top-level `buy` is the ticketing host used as every showtime's booking URL.
 *
 * The listing has everything we display; there's no per-film detail page to
 * fetch (TMDB enriches the rest downstream). One `CinemaMovie` per title, with
 * the screenings merged and sorted.
 */
class KinoBajkaClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoBajkaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  def parseHtml(html: String): Seq[CinemaMovie] = {
    // jsoup decodes the `data-dane` HTML entities, handing back the raw JSON.
    val dane = Option(Jsoup.parse(html).selectFirst("#rep2"))
      .map(_.attr("data-dane")).filter(_.nonEmpty)
      .flatMap(s => Try(Json.parse(s)).toOption)

    val slots = dane.toSeq.flatMap { root =>
      val booking = (root \ "buy").asOpt[String].filter(_.nonEmpty)
      (root \ "dni").asOpt[JsObject].toSeq.flatMap { dni =>
        dni.fields.flatMap { case (dateStr, films) =>
          dayDate(dateStr).toSeq.flatMap { date =>
            films.asOpt[Seq[JsObject]].getOrElse(Seq.empty).flatMap(parseFilm(_, date, booking))
          }
        }
      }
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = head.runtime),
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
}

object KinoBajkaClient {

  val BaseUrl = "https://kinobajka.pl"
  val PageUrl = s"$BaseUrl/repertuar/"

  // The `m` caption folds "genres · format · NNN min"; the trailing run is runtime.
  private val DurationPat = """(\d+)\s*min""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    poster:   Option[String],
    filmUrl:  Option[String],
    runtime:  Option[Int]
  )

  private def dayDate(id: String): Option[LocalDate] =
    Try(LocalDate.parse(id)).toOption  // the `dni` keys are ISO `YYYY-MM-DD`

  /** Parse one film object under a day into its screenings. */
  private def parseFilm(film: JsObject, date: LocalDate, booking: Option[String]): Seq[RawSlot] = {
    val title   = (film \ "t").asOpt[String]
                    .map(t => TitleNormalizer.cinemaClean("kino-bajka", t.trim)).filter(_.nonEmpty)
    val filmUrl = (film \ "u").asOpt[String].filter(_.nonEmpty)
    val poster  = (film \ "p").asOpt[String].filter(_.nonEmpty)
    val runtime = (film \ "m").asOpt[String]
                    .flatMap(DurationPat.findFirstMatchIn).map(_.group(1).toInt)

    title.toSeq.flatMap { t =>
      (film \ "s").asOpt[Seq[JsObject]].getOrElse(Seq.empty).flatMap { slot =>
        (slot \ "g").asOpt[String].flatMap(ScraperParse.parseHHmm).map { lt =>
          RawSlot(t, LocalDateTime.of(date, lt), booking, poster, filmUrl, runtime)
        }
      }
    }
  }
}
