package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
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

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  // gcf.org.pl's WordPress origin flaps: ~10–25% of requests come back HTTP 500,
  // independent of our request shape (headers, UA, IP — confirmed by an A/B probe
  // that saw the same rate from an unrelated address). The 500s fast-fail in
  // ~0.6s (vs ~4.5s for a real render) and cluster into short bad windows. The
  // default 3 attempts (sleeping 1s+2s) sometimes all land inside one such window
  // and the cinema drops out of the cache for that tick (a red uptime bar). More
  // attempts are nearly free here — each failure is a sub-second fast-fail — and
  // the longer 1s/2s/4s/8s backoff span crosses the bad windows, so the scrape
  // recovers within the tick instead of dropping. A recovered tick records a clean
  // green bar (UptimeRecordingScraper only flags a scrape whose retries are all
  // exhausted, since RetryingCinemaScraper swallows the recovered ones), so these
  // sub-second blips no longer surface as uptime noise.
  override val maxFetchAttempts: Int = 5

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
    val document   = Jsoup.parse(html)
    val slots = document.select("div.film-width").asScala.toSeq.flatMap(parseFilmBlock)

    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking, s.room),
      distinctBy = s => (s.dateTime, s.room)
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
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
