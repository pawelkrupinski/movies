package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * OKF Iluzja (Kino Studyjne Iluzja), Częstochowa — the art-house screen of
 * the Ośrodek Kultury Filmowej at Al. NMP 64.  The cinema publishes a
 * server-rendered WordPress schedule at `/repertuar/repertuar-tygodniowy/`,
 * listing the full current week on a single page (no per-day pagination, no
 * JavaScript).
 *
 * Page structure:
 *   - Multiple `div.repertuar-list` blocks, one per day.
 *   - Each block opens with `h2.date-f` carrying the date in Polish long form
 *     ("7 czerwca 2026").  No year is needed from the header because the
 *     site includes the year explicitly.
 *   - Within the block, each `div.box` is one screening:
 *       * `span.time-f` — start time ("HH:MM").
 *       * `div.right h3` — film title, sometimes with an inner `<span>` tag
 *         carrying a programme label ("POWRÓT NA EKRAN") that is stripped by
 *         taking only the h3's own text nodes.
 *       * first `a[href^="https://okf.czest.pl/film/"]` — the cinema-relative
 *         film-detail page URL; carried as `filmUrl`.
 *
 * Booking is via the cinema's generic MSI portal
 * (https://bilety.okf.czest.pl/MSI/mvc/pl) — there are no per-screening
 * booking URLs on the repertoire page, so `bookingUrl` is always `None`.
 *
 * `today` is accepted so the spec can pin the date inference to a fixed
 * capture day; production passes the real `LocalDate.now`.  The date is
 * taken directly from the h2 header (year included), not inferred from
 * `today` — `today` is currently unused but kept for structural consistency
 * with other single-page scrapers that do need it.
 */
class OkfIluzjaClient(
  http:              HttpFetch,
  override val cinema: Cinema = OkfIluzja,
  today:             LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import OkfIluzjaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(WeeklyUrl)
    val document  = Jsoup.parse(html)
    val slots = parseDocument(document)

    // Group by filmUrl (the per-film page slug) so the same title on multiple
    // days collapses into one CinemaMovie with all its showtimes.
    val byFilmUrl = slots.groupBy(_.filmUrl)
    byFilmUrl.toSeq.flatMap { case (_, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, bookingUrl = None))
        .distinctBy(_.dateTime)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie     = Movie(head.title),
          cinema    = cinema,
          posterUrl = Some(head.posterUrl).filter(_.nonEmpty),
          filmUrl   = Some(head.filmUrl),
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }
}

object OkfIluzjaClient {

  val BaseUrl    = "https://okf.czest.pl"
  val WeeklyUrl  = s"$BaseUrl/repertuar/repertuar-tygodniowy/"

  /** Polish genitive month names — reused from the shared map in [[ScraperParse]].
   *  The h2 date header uses the same genitive form ("7 czerwca 2026"). */
  private val PolishMonths: Map[String, Int] = ScraperParse.PolishMonths

  // "7 czerwca 2026" — day, genitive month, year (all with optional surrounding whitespace)
  private val DatePat = """(\d{1,2})\s+(\w+)\s+(\d{4})""".r

  private[cinemas] case class RawSlot(
    title:    String,
    filmUrl:  String,
    posterUrl: String,
    dateTime: LocalDateTime
  )

  private[cinemas] def parseDocument(document: org.jsoup.nodes.Document): Seq[RawSlot] =
    document.select("div.repertuar-list").asScala.toSeq.flatMap { block =>
      val dateOpt = parseBlockDate(block)
      dateOpt match {
        case None       => Seq.empty
        case Some(date) => parseBoxes(block, date)
      }
    }

  /** Extract a `LocalDate` from the `h2.date-f` header of a day block. */
  private def parseBlockDate(block: org.jsoup.nodes.Element): Option[LocalDate] =
    Option(block.selectFirst("h2.date-f")).flatMap { h2 =>
      DatePat.findFirstMatchIn(h2.text.trim).flatMap { m =>
        val day   = m.group(1).toInt
        val month = PolishMonths.get(m.group(2).toLowerCase)
        val year  = Try(m.group(3).toInt).toOption
        for (mo <- month; yr <- year; date <- Try(LocalDate.of(yr, mo, day)).toOption)
          yield date
      }
    }

  /** Parse all `div.box` film cards inside one day block. */
  private def parseBoxes(block: org.jsoup.nodes.Element, date: LocalDate): Seq[RawSlot] =
    block.select("div.box").asScala.toSeq.flatMap { box =>
      val timeOpt  = Option(box.selectFirst("span.time-f"))
        .flatMap(element => ScraperParse.parseHHmm(element.text.trim))
      // Title: use the h3's own text only (ownText strips inner <span> labels)
      val titleOpt = Option(box.selectFirst("div.right h3"))
        .map(_.ownText().trim)
        .filter(_.nonEmpty)
      // Film page URL: first link to an okf.czest.pl/film/ path in the box.
      // We match by checking if the href contains "/film/" rather than using a
      // CSS ^= prefix selector with a full https:// URL (Jsoup can struggle with
      // colons in unquoted CSS attribute selectors).
      val filmUrlOpt = box.select("a[href]").asScala
        .map(_.attr("href").trim)
        .find(h => h.contains(s"$BaseUrl/film/") || h.startsWith("/film/"))
        .map(h => if (h.startsWith("/")) BaseUrl + h else h)
        .filter(_.nonEmpty)
      // Poster: img inside the box, from the film-page link
      val posterUrl = Option(box.selectFirst("div.right img[src]"))
        .map(_.attr("src").trim)
        .getOrElse("")

      (titleOpt, filmUrlOpt, timeOpt) match {
        case (Some(title), Some(filmUrl), Some(time)) =>
          Seq(RawSlot(title, filmUrl, posterUrl, LocalDateTime.of(date, time)))
        case _ => Seq.empty
      }
    }
}
