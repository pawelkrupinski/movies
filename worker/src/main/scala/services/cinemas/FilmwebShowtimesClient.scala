package services.cinemas

import models._
import play.api.libs.json._
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, ZoneId}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Universal Filmweb showtimes scraper, driven by Filmweb's JSON API rather
 * than HTML scraping. Revives the approach of an old `FilmwebClient.fetch()`
 * (long since removed): Filmweb still serves a whole cinema's programme as
 * structured JSON, and the payload is now richer — every screening carries a
 * per-hour `orderLinks` map (multiplex booking deep-link, the
 * `?SC_CMP=FILMWEB` referral) plus a language-version flag
 * (`dubbing`/`subtitles`/`lektor`) we surface as format tokens.
 *
 * Two endpoints, same `api/v1` family the ratings [[services.enrichment.FilmwebClient]]
 * uses:
 *
 *   1. GET /api/v1/cinema/{cinemaId}/seances?date=YYYY-MM-DD
 *      → JSON array, one element per (film, hours) screening at that
 *        cinema/date. `film` is Filmweb's internal film id; `hours` is a
 *        space-separated list of `HH.MM`; `orderLinks` maps each `HH.MM` to a
 *        booking URL; optional `dubbing`/`subtitles`/`lektor` give the
 *        language version.
 *   2. GET /api/v1/title/{filmId}/info
 *      → { title, posterPath, year, ... }. Poster URL is
 *        `https://fwcdn.pl/ppo` + `posterPath.replace("$","2")`.
 *
 * `cinemaId` is Filmweb's INTERNAL cinema id (Poznań: Multikino=633, Kino
 * Muza=75, Rialto=78), distinct from the `/showtimes/<City>/<Name>-<id>` URL
 * id. One client instance serves one venue — adding a Filmweb-backed cinema is
 * a new instance with its (cinemaId, cinema) pair, no code change here (OCP).
 *
 * Fetches are tolerant: a day whose seances request fails or returns empty is
 * dropped, not fatal — Filmweb routinely 404s/empties future dates a venue
 * hasn't published yet. TMDB enriches synopsis/cast/director/runtime/genres
 * downstream, so this client leaves those empty and only carries the title,
 * poster, booking links and format tokens Filmweb actually provides.
 */
class FilmwebShowtimesClient(
  http:     HttpFetch,
  cinemaId: Int,
  override val cinema: Cinema,
  daysAhead: Int       = 6,
  today:     LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import FilmwebShowtimesClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiBase)

  def fetch(): Seq[CinemaMovie] = {
    val dates = (0 to daysAhead).map(today.plusDays(_))

    // One seances page per date, fetched in parallel and tolerantly: a failed
    // or unparseable day yields no seances rather than killing the batch.
    val seancesByDate = ParallelDetailFetch.keyed(
      "filmweb-seances", dates, 1.minute
    )(d => seancesUrl(cinemaId, d)) { url =>
      Try(http.get(url)).toOption.toSeq.flatMap(body => parseSeancesForUrl(body, url))
    }
    val seances: Seq[RawSeance] = dates.flatMap(d => seancesByDate.getOrElse(d, Seq.empty))

    if (seances.isEmpty) return Seq.empty

    // One /title/{id}/info per unique film, in parallel.
    val filmIds = seances.map(_.filmId).distinct
    val infos = ParallelDetailFetch.keyed(
      "filmweb-title-info", filmIds, 1.minute
    )(id => titleInfoUrl(id)) { url =>
      Try(http.get(url)).toOption.flatMap(parseFilmInfo)
    }

    seances.groupBy(_.filmId).toSeq.flatMap { case (filmId, group) =>
      val info = infos.get(filmId).flatten
      val title = info.flatMap(_.title).map(_.trim).filter(_.nonEmpty)
        .orElse(group.flatMap(_.fallbackTitle).headOption)
      title match {
        case None => None
        case Some(t) =>
          val showtimes = group.flatMap(_.showtimes)
            .distinctBy(s => (s.dateTime, s.bookingUrl))
            .sortBy(_.dateTime)
          if (showtimes.isEmpty) None
          else Some(CinemaMovie(
            movie       = Movie(title = t, releaseYear = info.flatMap(_.year)),
            cinema      = cinema,
            posterUrl   = info.flatMap(_.posterUrl),
            filmUrl     = Some(filmPageUrl(filmId)),
            synopsis    = None,
            cast        = Seq.empty,
            director    = Seq.empty,
            showtimes   = showtimes,
            externalIds = Map("filmweb" -> filmId.toString)
          ))
      }
    }
  }

  /** Parse one seances response. `date` is the calendar day this page was
   *  requested for; each `hours` token combines with it to form the screening
   *  `LocalDateTime`. Pure + public so the spec can feed fixture bytes. */
  def parseSeances(json: String, date: LocalDate): Seq[RawSeance] =
    Json.parse(json).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty).flatMap { js =>
      (js \ "film").asOpt[Long].map { filmId =>
        val format        = formatTokens(js)
        val orderLinks    = (js \ "orderLinks").asOpt[Map[String, String]].getOrElse(Map.empty)
        val fallbackTitle = (js \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        val showtimes = (js \ "hours").asOpt[String].getOrElse("")
          .split("\\s+").iterator.map(_.trim).filter(_.nonEmpty).flatMap { tok =>
            hourToTime(tok).map { case (hh, mm) =>
              Showtime(date.atTime(hh, mm), orderLinks.get(tok), None, format)
            }
          }.toSeq
        RawSeance(filmId, showtimes, fallbackTitle)
      }
    }

  private def parseSeancesForUrl(json: String, url: String): Seq[RawSeance] =
    dateOf(url).map(d => Try(parseSeances(json, d)).getOrElse(Seq.empty)).getOrElse(Seq.empty)

  /** Parse one /title/{id}/info response into title + year + poster URL.
   *  Pure + public so the spec can feed fixture bytes. */
  def parseFilmInfo(json: String): Option[FilmInfo] =
    Try(Json.parse(json)).toOption.map { j =>
      FilmInfo(
        title     = (j \ "title").asOpt[String],
        year      = (j \ "year").asOpt[Int],
        posterUrl = (j \ "posterPath").asOpt[String].filter(_.nonEmpty).map(posterUrlFor)
      )
    }
}

object FilmwebShowtimesClient {
  private val ApiBase     = "https://www.filmweb.pl/api/v1"
  private val PosterBase  = "https://fwcdn.pl/ppo"
  private val HourPat     = """^(\d{1,2})\.(\d{2})$""".r
  private val DateParam   = """[?&]date=(\d{4}-\d{2}-\d{2})""".r

  /** Language-version flags Filmweb sets on a seance, mapped to the project's
   *  format-token vocabulary (mirrors `NoveKinoClient.FormatWord`). Only the
   *  flag(s) actually present on a given screening are emitted. */
  private val VersionTokens: Seq[(String, String)] = Seq(
    "subtitles" -> "NAP",
    "dubbing"   -> "DUB",
    "lektor"    -> "LEK"
  )

  def seancesUrl(cinemaId: Int, date: LocalDate): String =
    s"$ApiBase/cinema/$cinemaId/seances?date=$date"

  def titleInfoUrl(filmId: Long): String = s"$ApiBase/title/$filmId/info"

  def filmPageUrl(filmId: Long): String = s"https://www.filmweb.pl/film/$filmId"

  /** Poster URL the way the old client built it: prepend the CDN base and
   *  swap the `$` size-placeholder for `2` (medium poster). */
  def posterUrlFor(posterPath: String): String =
    PosterBase + posterPath.replace("$", "2")

  private def formatTokens(js: JsValue): List[String] =
    VersionTokens.collect { case (field, token) if (js \ field).asOpt[String].exists(_.nonEmpty) => token }.toList

  private def hourToTime(token: String): Option[(Int, Int)] = token match {
    case HourPat(h, m) =>
      val hh = h.toInt; val mm = m.toInt
      if (hh < 24 && mm < 60) Some(hh -> mm) else None
    case _ => None
  }

  private def dateOf(url: String): Option[LocalDate] =
    DateParam.findFirstMatchIn(url).flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)

  /** One screening row from the seances endpoint: the film id, the parsed
   *  showtimes (date + hour + booking link + format), and Filmweb's optional
   *  inline `title` (a fallback when /title/info doesn't resolve a title). */
  case class RawSeance(filmId: Long, showtimes: Seq[Showtime], fallbackTitle: Option[String])

  /** Distilled /title/{id}/info — the fields this client needs. */
  case class FilmInfo(title: Option[String], year: Option[Int], posterUrl: Option[String])
}
