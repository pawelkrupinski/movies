package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

/**
 * Kino Mikro (kinomikro.pl) and its sister screen Mikro Bronowice share one
 * Joomla/Omomo backend that exposes the whole programme as JSON at
 * `api.php/v1/repertoires` — no HTML scraping, no detail-page fetch. Both
 * venues come back in the same flat array; `location_institution_name`
 * (`"Kino Mikro"` vs `"Mikro Bronowice"`) is the discriminant, so one client
 * parameterised by the venue name serves either screen. The JSON carries no
 * runtime / genres — TMDB supplies those downstream — but the
 * `event_description` HTML blob does name the director, which we extract.
 */
class KinoMikroClient(http: HttpFetch, venueName: String, override val cinema: Cinema) extends CinemaScraper {
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoMikroClient.ApiUrl, "https://bilety.kinomikro.pl")

  def fetch(): Seq[CinemaMovie] = KinoMikroParser.parse(http.get(KinoMikroClient.ApiUrl), venueName, cinema)
}

object KinoMikroClient {
  // The feed paginates: with no `limit` it caps the response at ~20 records and
  // silently drops the back half of the multi-week programme — one screening per
  // film survives, the advance-date repeats are truncated. `limit` is honoured
  // server-side, so ask for far more than the cinema could ever schedule to pull
  // the whole window (06.06–30.06 at recording time) in one shot.
  val ApiUrl = "https://kinomikro.pl/api.php/v1/repertoires?limit=1000"
}

object KinoMikroParser {
  // `event_date` is rendered as `DD.MM.YYYY HH:mm` (e.g. "06.06.2026 18:00").
  private val DateFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")

  // `event_description` is an HTML blob whose director line reads either
  // `<div>Reżyseria George Sluizer</div>` (no colon) or `<br>Reżyseria:
  // Federico Fellini <br>` (colon). Once Jsoup flattens the markup to text the
  // `<div>`/`<br>` boundaries collapse to spaces, so the value is bounded by
  // the next field label rather than by markup. Capture everything after the
  // `Reżyseria` marker up to the next known label (or end of text).
  private val FieldLabels = "Obsada|Scenariusz|Gatunek|Produkcja|Czas|Dystrybutor"
  private val DirectorPat =
    ("""(?i)Reżyseria\s*:?\s*(.+?)\s*(?=(?:""" + FieldLabels + """)\b|$)""").r

  /** Pull the director name(s) out of a row's `event_description` HTML. Splits
   *  the captured value on `,`/`;`, trims, and drops empties. Returns an empty
   *  Seq when the blob carries no `Reżyseria` marker. */
  private[cinemas] def parseDirector(eventDescriptionHtml: String): Seq[String] = {
    val text = Jsoup.parse(eventDescriptionHtml).text()
    DirectorPat.findFirstMatchIn(text).map(_.group(1)).toSeq.flatMap { captured =>
      captured.split("[,;]").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    }
  }

  def parse(json: String, venueName: String, cinema: Cinema): Seq[CinemaMovie] = {
    val records = (Json.parse(json) \ "data").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)

    val rows = records.flatMap { r =>
      for {
        inst    <- (r \ "location_institution_name").asOpt[String] if inst == venueName
        title   <- (r \ "event_title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        dateStr <- (r \ "event_date").asOpt[String]
        dt      <- Try(LocalDateTime.parse(dateStr, DateFmt)).toOption
      } yield {
        val booking = for {
          slug <- (r \ "slug").asOpt[String].filter(_.nonEmpty)
          id   <- (r \ "event_id").asOpt[String].filter(_.nonEmpty)
        } yield s"https://kinomikro.pl/repertoire/$slug/$id.html?view=event"
        val poster = (r \ "system_image_url").asOpt[String].filter(_.nonEmpty)
          .map(u => if (u.startsWith("http")) u else s"https://bilety.kinomikro.pl$u")
        val director = (r \ "event_description").asOpt[String]
          .map(parseDirector).getOrElse(Seq.empty)
        RawSlot(title, dt, booking, poster, director)
      }
    }

    rows.groupBy(_.title).toSeq.map { case (title, group) =>
      val sorted = group.sortBy(_.dateTime)
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = sorted.flatMap(_.poster).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = sorted.map(_.director).find(_.nonEmpty).getOrElse(Seq.empty),
        showtimes = sorted.map(s => Showtime(s.dateTime, s.booking)).distinctBy(s => (s.dateTime, s.bookingUrl))
      )
    }.sortBy(_.movie.title)
  }

  private case class RawSlot(
    title: String,
    dateTime: LocalDateTime,
    booking: Option[String],
    poster: Option[String],
    director: Seq[String]
  )
}
