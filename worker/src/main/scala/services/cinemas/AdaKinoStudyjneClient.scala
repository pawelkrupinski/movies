package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * ADA Kino Studyjne (Warszawa, ul. Chrościckiego 14) — operated by the
 * Ursynów cultural centre, ticketed through biletyna.pl.
 *
 * The place page at `https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne`
 * embeds a single `<script type="application/ld+json">` block whose top-level
 * `@type` is `Place` and whose `events` array contains one `ScreeningEvent`
 * per individual screening (not per film). Each entry carries:
 *   - `name`      — film title
 *   - `image`     — poster URL
 *   - `url`       — biletyna booking page (`/film/<Slug>?eid=N#opis`)
 *   - `startDate` — ISO-8601 with UTC offset (`2026-06-07T15:00:00+02:00`)
 *
 * All events on a single page — no pagination, no per-film detail fetch.
 * Screenings for the same film are grouped so the result is one `CinemaMovie`
 * per title.
 */
class AdaKinoStudyjneClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import AdaKinoStudyjneClient._

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(PageUrl)
    parseHtml(html, cinema)
  }
}

object AdaKinoStudyjneClient {

  val BaseUrl = "https://www.biletyna.pl"
  val PageUrl = s"$BaseUrl/Warszawa/ADA-Kino-Studyjne"

  // ISO-8601 offset datetime — e.g. "2026-06-07T15:00:00+02:00"
  private val IsoDtFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXX")

  private[cinemas] def parseHtml(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc   = Jsoup.parse(html)
    val block = doc.select("script[type=application/ld+json]").asScala
      .map(_.data())
      .find(d => d.contains("\"@type\":\"Place\"") || d.contains("ScreeningEvent"))
      .getOrElse(return Seq.empty)

    parseJson(block, cinema)
  }

  private[cinemas] def parseJson(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    val root  = Try(Json.parse(json)).getOrElse(return Seq.empty)
    val events = (root \ "events").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)

    // One entry per individual screening — group by title.
    val rawSlots: Seq[(String, Option[String], Option[String], LocalDateTime)] =
      events.flatMap { ev =>
        val title   = (ev \ "name").asOpt[String].filter(_.nonEmpty)
        val image   = (ev \ "image").asOpt[String].filter(_.nonEmpty)
        val url     = (ev \ "url").asOpt[String].filter(_.nonEmpty)
        val dtStr   = (ev \ "startDate").asOpt[String].filter(_.nonEmpty)
        for {
          t  <- title
          ds <- dtStr
          dt <- Try(LocalDateTime.from(IsoDtFmt.parse(ds))).toOption
        } yield (t, image, url, dt)
      }

    rawSlots
      .groupBy(_._1)
      .toSeq
      .flatMap { case (title, group) =>
        val poster    = group.flatMap(_._2).headOption
        val showtimes = group
          .map { case (_, _, url, dt) => Showtime(dt, url) }
          .distinctBy(_.dateTime)
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(title),
          cinema    = cinema,
          posterUrl = poster,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
      .sortBy(_.movie.title)
  }
}
