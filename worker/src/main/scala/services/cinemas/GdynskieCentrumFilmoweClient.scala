package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Gdyńskie Centrum Filmowe (Gdynia) — the city's art-house cinema, run by
 * the cultural foundation of the same name. Its repertoire is published on the
 * venue's own WordPress site and ticketed through an in-house MSI portal at
 * `bilet.gcf.org.pl`.
 *
 * The repertoire page at `https://gcf.org.pl/kino-studyjne/repertuar/` renders
 * all upcoming films in a server-side WordPress template. Each film occupies a
 * `div.film-width > div.box-item-content` wrapper containing:
 *   - `a[title]` — the film title (attribute, e.g. "Diabeł ubiera się u Prady 2")
 *   - `img.image-film[src]` — poster
 *   - `a.film-hours[data-date][data-hour][href]` — one booking link per
 *     screening, with `data-date="YYYY-MM-DD"` and `data-hour="HH:MM"`.
 *
 * Screenings for the same film (same title) can appear in multiple
 * `div.projection` blocks (one per hall); they are merged into a single
 * `CinemaMovie` row. The hall name is in an immediately preceding
 * `div.projection-location`.
 *
 * Year inference: `data-date` already carries the four-digit year, so no
 * guessing is needed.
 */
class GdynskieCentrumFilmoweClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import GdynskieCentrumFilmoweClient._

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    parseHtml(html, cinema)
  }
}

object GdynskieCentrumFilmoweClient {

  val BaseUrl       = "https://gcf.org.pl"
  val RepertoireUrl = s"$BaseUrl/kino-studyjne/repertuar/"

  private val DateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val TimePat = """(\d{1,2}):(\d{2})""".r

  private[cinemas] case class RawSlot(
    title:   String,
    poster:  Option[String],
    dateTime: LocalDateTime,
    booking: Option[String],
    room:    Option[String]
  )

  private[cinemas] def parseHtml(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc   = Jsoup.parse(html)
    val slots = doc.select("div.film-width").asScala.toSeq.flatMap(parseFilmBlock)

    slots
      .groupBy(_.title)
      .toSeq
      .flatMap { case (title, group) =>
        val poster    = group.flatMap(_.poster).headOption
        val showtimes = group
          .map(s => Showtime(s.dateTime, s.booking, s.room))
          .distinctBy(s => (s.dateTime, s.room))
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

  /** Parse all screenings out of one `div.film-width` block. */
  private def parseFilmBlock(filmDiv: Element): Seq[RawSlot] = {
    // Title is on the anchor wrapping the image, via its `title` attribute.
    val titleOpt = Option(filmDiv.selectFirst("div.box-item-content > a[title]"))
      .map(_.attr("title").trim)
      .filter(_.nonEmpty)
    val poster   = Option(filmDiv.selectFirst("img.image-film[src]"))
      .map(_.attr("src").trim)
      .filter(_.nonEmpty)

    titleOpt match {
      case None    => Seq.empty
      case Some(t) =>
        // Each `div.projection` block within the film entry groups one hall's
        // screenings. The hall name is in a sibling `div.projection-location`.
        filmDiv.select("div.projection").asScala.toSeq.flatMap { projDiv =>
          val hall = Option(projDiv.selectFirst("div.projection-location"))
            .map(_.text.trim).filter(_.nonEmpty)

          projDiv.select("a.film-hours[data-date][data-hour]").asScala.toSeq.flatMap { a =>
            val dateStr = a.attr("data-date").trim
            val hourStr = a.attr("data-hour").trim
            val booking = Option(a.attr("href")).filter(u => u.nonEmpty && u != "#")
            parseDateTime(dateStr, hourStr).map { dt =>
              RawSlot(t, poster, dt, booking, hall)
            }
          }
        }
    }
  }

  private def parseDateTime(dateStr: String, hourStr: String): Option[LocalDateTime] =
    for {
      date <- Try(LocalDate.parse(dateStr, DateFmt)).toOption
      m    <- TimePat.findFirstMatchIn(hourStr)
      time <- Try(java.time.LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption
    } yield LocalDateTime.of(date, time)
}
