package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, MonthDay, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Praha (Mazowiecki Teatr Muzyczny im. Jana Kiepury, Warszawa). Its own
 * listing at `mteatr.pl/pl/repertuar-kino-praha` is a bespoke server-rendered
 * page — one `div.post` per screening, grouped under `div.month-heading` day
 * headers. Each post carries:
 *   - `a[href^=/pl/]`            → the film detail link (also the post wrapper)
 *   - `a > div.label`           → the screening stamp, whose exact shape has
 *                                 drifted twice since June 2026: "10 Cze 2026 /
 *                                 15:30" (year present), then "09 Wrz 2026 (Śr)
 *                                 / 16:00" (a weekday abbreviation inserted),
 *                                 then "12 Wrz (Sb) / 16:10" (the year dropped
 *                                 entirely). `StampPat` tolerates all three; a
 *                                 missing year is inferred from `today` (the
 *                                 badge label inside `div.image`, e.g.
 *                                 "przedpremiera", carries the extra
 *                                 `.special-1` class and is excluded).
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
                  override val cinema: Cinema = KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha,
                  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")))
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PrahaClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(PrahaClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    PrahaClient.parse(http.get(PrahaClient.RepertoireUrl), cinema, today)
}

object PrahaClient {

  val BaseUrl       = "https://www.mteatr.pl"
  val RepertoireUrl = s"$BaseUrl/pl/repertuar-kino-praha"

  // "10 Cze 2026 / 15:30" — day, Polish month abbreviation, an optional year,
  // then time. The site first started inserting a parenthesised weekday
  // abbreviation between the year and the slash ("09 Wrz 2026 (Śr) / 16:00"),
  // then four days later dropped the year entirely ("12 Wrz (Sb) / 16:10") —
  // both tolerated without capturing the weekday (redundant with the date
  // itself) and without requiring the year (see `parseStamp`, which infers it
  // from `today` via `ScraperParse.upcomingDate` when absent).
  private val StampPat = """(\d{1,2})\s+(\p{L}+)(?:\s+(\d{4}))?(?:\s*\([^)]*\))?\s*/\s*(\d{1,2}):(\d{2})""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    filmUrl:  Option[String]
  )

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div.post:has(div.box_tytul):has(div.label)").asScala.toSeq.flatMap { post =>
      for {
        titleElement <- Option(post.selectFirst("div.box_tytul h2"))
        title    = titleElement.text.trim if title.nonEmpty
        stamp   <- Option(post.selectFirst("div.label:not(.special-1)")).map(_.text.trim)
        dateTime <- parseStamp(stamp, today)
      } yield RawSlot(
        title    = title,
        dateTime = dateTime,
        filmUrl  = Option(post.selectFirst("a[href]"))
                     .map(_.attr("abs:href")).filter(_.nonEmpty)
      )
    }

    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, bookingUrl = None),
      distinctBy = _.dateTime
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  /** "10 Cze 2026 / 15:30" or the yearless "12 Wrz / 16:10" → `LocalDateTime`;
   *  `None` when the month abbreviation is unknown or the day/time is out of
   *  range. A stamp without a year has its year inferred from `today` via
   *  [[ScraperParse.upcomingDate]] — the page only ever lists near-future
   *  screenings, so the default 60-day grace is generous. */
  private def parseStamp(stamp: String, today: LocalDate): Option[LocalDateTime] =
    StampPat.findFirstMatchIn(stamp).flatMap { m =>
      ScraperParse.polishMonthAbbrev(m.group(2)).flatMap { month =>
        val day = m.group(1).toInt
        val date = Option(m.group(3)) match {
          case Some(year) => Try(LocalDate.of(year.toInt, month, day)).toOption
          case None       => Try(MonthDay.of(month, day)).toOption.flatMap(ScraperParse.upcomingDate(_, today))
        }
        date.flatMap(d => Try(d.atTime(m.group(4).toInt, m.group(5).toInt)).toOption)
      }
    }
}
