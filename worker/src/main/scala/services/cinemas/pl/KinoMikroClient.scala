package services.cinemas.pl

import services.cinemas.common.ScraperParse
import tools.HttpFetch
import models._
import play.api.libs.json._
import org.jsoup.Jsoup
import services.cinemas.common.CinemaScraper

import java.time.{LocalDateTime, OffsetDateTime}
import scala.util.Try

/**
 * Kino Mikro (kinomikro.pl) and its sister screen Mikro Bronowice sell tickets
 * through one VisualSoft ticketing instance (`bilety.kinomikro.pl`), whose
 * `service.php/repertoire/list.json` feed is the whole upcoming programme of
 * both screens as JSON — no HTML scraping, no detail-page fetch.
 * `location.institution_name` (`"Kino Mikro"` vs `"Mikro Bronowice"`) is the
 * discriminant, so one client parameterised by the venue name serves either
 * screen. The JSON carries no runtime / genres — TMDB supplies those
 * downstream — but the `event.description` HTML blob sometimes names the
 * director, which we extract.
 *
 * One request covers the whole programme: the feed honours `limit` and reports
 * the total in `meta.nbResults` (95 screenings across both screens, five weeks
 * out, on 2026-09-26), so there is no horizon to walk. Until 2026-09-23 the
 * venue's own Joomla site re-served this same programme at
 * `kinomikro.pl/api.php/v1/repertoires`; the WordPress rebuild that replaced it
 * 404s that path, and the venue sat on the Filmweb fallback until this client
 * moved to the ticketing feed directly.
 */
class KinoMikroClient(
  http:                HttpFetch,
  venueName:           String,
  override val cinema: Cinema
) extends CinemaScraper {
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoMikroClient.BaseUrl)
  // One feed serves both Mikro screens; the venue name picks this one's rows.
  override def sourceKey: Option[String] = Some(s"${CinemaScraper.urlKey(KinoMikroClient.FeedUrl)}#$venueName")

  def fetch(): Seq[CinemaMovie] = KinoMikroParser.parse(http.get(KinoMikroClient.FeedUrl), venueName, cinema)
}

object KinoMikroClient {
  val BaseUrl = "https://bilety.kinomikro.pl"
  // `limit` is honoured (the default page is small); 1000 is ~10× a month of
  // both screens.
  val FeedUrl = s"$BaseUrl/service.php/repertoire/list.json?limit=1000&advanced=1"
}

object KinoMikroParser {
  // `event.description` is an HTML blob whose director line reads either
  // `<div>Reżyseria George Sluizer</div>` (no colon) or `<br>Reżyseria:
  // Federico Fellini <br>` (colon). Once Jsoup flattens the markup to text the
  // `<div>`/`<br>` boundaries collapse to spaces, so the value is bounded by
  // the next field label rather than by markup. Capture everything after the
  // `Reżyseria` marker up to the next known label, a `|` separator
  // (`Reżyseria: Sam Raimi | Produkcja: USA, 1987`), or the end of text. The
  // label's end is `(?!\p{L})`, not `\b`: Java's `\b` is ASCII-only, so it sees
  // no boundary after the `ą` of "Występują".
  private val FieldLabels = "Obsada|Występują|Scenariusz|Muzyka|Zdjęcia|Gatunek|Produkcja|Czas|Dystrybutor"
  private val DirectorPat =
    ("""(?i)Reżyseria\s*:?\s*(.+?)\s*(?=(?:""" + FieldLabels + """)(?!\p{L})|\||$)""").r

  /** Pull the director name(s) out of a row's `event.description` HTML. Splits
   *  the captured value on `,`/`;`, trims, and drops empties. Returns an empty
   *  Seq when the blob carries no `Reżyseria` marker. */
  private[cinemas] def parseDirector(eventDescriptionHtml: String): Seq[String] = {
    val text = Jsoup.parse(eventDescriptionHtml).text()
    DirectorPat.findFirstMatchIn(text).map(_.group(1)).toSeq.flatMap { captured =>
      captured.split("[,;]").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    }
  }

  /** The feed's `repertoires` object (screening id → screening) → this venue's
   *  films. A film's dubbed and subtitled screenings arrive as separate titles
   *  ("Marsupilami- dubbing"); the version tag is peeled into the showtime's
   *  format so they fold onto one row. */
  def parse(json: String, venueName: String, cinema: Cinema): Seq[CinemaMovie] = {
    val records = (Json.parse(json) \ "repertoires").asOpt[JsObject].map(_.values.toSeq).getOrElse(Seq.empty)

    val rows = records.flatMap { r =>
      for {
        inst     <- (r \ "location" \ "institution_name").asOpt[String] if inst == venueName
        rawTitle <- (r \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        (title, format) = ScraperParse.extractFormatTags(rawTitle)
        if title.nonEmpty
        dt       <- (r \ "date").asOpt[String].flatMap(d => Try(OffsetDateTime.parse(d).toLocalDateTime).toOption)
      } yield RawSlot(
        title    = title,
        dateTime = dt,
        booking  = (r \ "url").asOpt[String].filter(_.nonEmpty).map(KinoMikroClient.BaseUrl + _),
        poster   = (r \ "image").asOpt[String].filter(_.nonEmpty).map(KinoMikroClient.BaseUrl + _),
        director = (r \ "event" \ "description").asOpt[String].map(parseDirector).getOrElse(Seq.empty),
        format   = format
      )
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
        showtimes = sorted.map(s => Showtime(s.dateTime, s.booking, None, s.format)).distinctBy(s => (s.dateTime, s.bookingUrl))
      )
    }.sortBy(_.movie.title)
  }

  private case class RawSlot(
    title: String,
    dateTime: LocalDateTime,
    booking: Option[String],
    poster: Option[String],
    director: Seq[String],
    format: List[String]
  )
}
