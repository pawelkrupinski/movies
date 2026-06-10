package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Praha (Mazowiecki Teatr Muzyczny im. Jana Kiepury, Warszawa). Its own
 * listing at `mteatr.pl/pl/repertuar-kino-praha` is a bespoke server-rendered
 * page — one `div.post` per screening, grouped under `div.month-heading` day
 * headers. Each post carries:
 *   - `a[href^=/pl/]`            → the film detail link (also the post wrapper)
 *   - `a > div.label`           → the screening stamp "10 Cze 2026 / 15:30",
 *                                 with an absolute 4-digit year, so the date is
 *                                 read off it directly — no year inference from
 *                                 a yearless day header (the badge label inside
 *                                 `div.image`, e.g. "przedpremiera", carries the
 *                                 extra `.special-1` class and is excluded).
 *   - `div.box_tytul h2`        → the clean film title
 *
 * The page's own poster is a low-res thumbnail behind a 2x3 placeholder, so —
 * like the other own-site scrapers — we leave `posterUrl` empty and let TMDB
 * enrichment supply the canonical art rather than seeding the film-wide poster
 * fallback list with a worse image.
 *
 * Kino Praha was closed 18 May – 9 Jun 2026 and only fed Filmweb thinly; like
 * the other small venues we moved off Filmweb (Spójnia, Ślęża, the MSI portals)
 * its own site is the canonical, single-venue source, so we scrape that.
 */
class PrahaClient(http: HttpFetch,
                  override val cinema: Cinema = KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PrahaClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    PrahaClient.parse(http.get(PrahaClient.RepertoireUrl), cinema)
}

object PrahaClient {

  val BaseUrl       = "https://www.mteatr.pl"
  val RepertoireUrl = s"$BaseUrl/pl/repertuar-kino-praha"

  // "10 Cze 2026 / 15:30" — day, Polish month abbreviation, year, then time.
  private val StampPat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})\s*/\s*(\d{1,2}):(\d{2})""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    filmUrl:  Option[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, BaseUrl)

    val slots = doc.select("div.post:has(div.box_tytul):has(div.label)").asScala.toSeq.flatMap { post =>
      for {
        titleEl <- Option(post.selectFirst("div.box_tytul h2"))
        title    = titleEl.text.trim if title.nonEmpty
        stamp   <- Option(post.selectFirst("div.label:not(.special-1)")).map(_.text.trim)
        dateTime <- parseStamp(stamp)
      } yield RawSlot(
        title    = title,
        dateTime = dateTime,
        filmUrl  = Option(post.selectFirst("a[href]"))
                     .map(_.attr("abs:href")).filter(_.nonEmpty)
      )
    }

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, bookingUrl = None))
        .distinctBy(_.dateTime)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  /** "10 Cze 2026 / 15:30" → `LocalDateTime`; `None` when the month
   *  abbreviation is unknown or the day/time is out of range. */
  private def parseStamp(stamp: String): Option[LocalDateTime] =
    StampPat.findFirstMatchIn(stamp).flatMap { m =>
      ScraperParse.polishMonthAbbrev(m.group(2)).flatMap { month =>
        Try(LocalDateTime.of(
          m.group(3).toInt, month, m.group(1).toInt, m.group(4).toInt, m.group(5).toInt)).toOption
      }
    }
}
