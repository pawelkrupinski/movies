package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Patria (Ruda Śląska) — a single-screen independent cinema hosted on a
 * bespoke WordPress theme (kinopatria.com). The repertoire page renders two
 * layout blocks in one HTML response:
 *
 *   1. `div.amy-movie-showtimews-daily-1` — the "currently showing" section.
 *      A row of date tabs each carries `data-date="DD-MM-YYYY"` and
 *      `data-movie="id1,id2"` listing which movie IDs play that date.
 *      Each movie item has an `h3.amy-movie-field-title` and a
 *      `div.amy-movie-item-showtimes.amy-item-{id}` with the screening times
 *      in `span` elements of `div.amy-movie-intro-times`. Because the same
 *      set of times applies to every date in the header, each (date, movie)
 *      pair is paired with all those times.
 *
 *   2. `div.amy-movie-showtimews-1` — the "coming soon / weekly" section.
 *      Each movie item has a `div.showtimes-cinema-group` → `div.amy-movie-times`
 *      → one `div.amy-cell` per day, each carrying the date as `DD.MM` in the
 *      `div.amy-head` and times as plain text in `div.amy-intro-times > div`.
 *      Only day+month are present — the year is inferred from `today` (rolled
 *      to next year when the month has already passed).
 *
 * Tickets are cash/phone only — no external booking URL is available.
 */
class KinoPatriaClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoPatria,
  today: LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoPatriaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    // Fetch OUTSIDE the Try so a 5xx/timeout propagates and surfaces as a red
    // uptime error, rather than being swallowed into an empty list that reads as
    // a successful "0 showtimes" scrape (white — indistinguishable from a
    // genuinely film-dormant venue). Only a PARSE failure is swallowed. Mirrors
    // the guard in KinoZamekClient.
    val html = http.get(RepertoireUrl)
    Try(parseRepertoire(html, cinema, today)).getOrElse(Seq.empty)
  }
}

object KinoPatriaClient {

  val RepertoireUrl = "https://kinopatria.com/repertuar/"

  // "19.15", " 15.00" — dot-separated HH.MM (not HH:MM) with optional leading space
  private val TimePat = """^\s*(\d{1,2})\.(\d{2})\s*$""".r

  // "DD-MM-YYYY" as emitted by data-date attributes in the daily tabs
  private val DailyDatePat = """^(\d{2})-(\d{2})-(\d{4})$""".r

  // "DD.MM" as emitted by the amy-head cells in the weekly grid
  private val WeeklyDatePat = """^(\d{1,2})\.(\d{2})$""".r

  /** Parse a time token of the form "HH.MM" (dot-separated, as used on this
   *  site rather than the conventional colon). */
  private[cinemas] def parseTime(s: String): Option[LocalTime] =
    TimePat.findFirstMatchIn(s.trim).flatMap { m =>
      Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption
    }

  /** Infer the full year for a (day, month) pair relative to `today`. If
   *  the month is strictly before today's month, assume next year (the
   *  schedule wraps the calendar year boundary). */
  private[cinemas] def inferYear(day: Int, month: Int, today: LocalDate): Option[LocalDate] =
    Try {
      val year = if (month < today.getMonthValue) today.getYear + 1 else today.getYear
      LocalDate.of(year, month, day)
    }.toOption

  private[cinemas] def parseRepertoire(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, RepertoireUrl)

    val dailyMovies  = parseDailySection(doc, cinema)
    val weeklyMovies = parseWeeklySection(doc, cinema, today)

    // Merge: a title present in both sections gets its showtimes combined;
    // the daily section carries absolute dates so is preferred for overlap
    // resolution — we use distinctBy(dateTime) after merging.
    val byTitle = (dailyMovies ++ weeklyMovies)
      .groupBy(_.movie.title)
      .values
      .toSeq
      .map { group =>
        val head = group.head
        val merged = group.flatMap(_.showtimes)
          .distinctBy(s => s.dateTime)
          .sortBy(_.dateTime)
        head.copy(showtimes = merged)
      }
      .filter(_.showtimes.nonEmpty)
      .sortBy(_.movie.title)

    byTitle
  }

  // ── Daily section ─────────────────────────────────────────────────────────

  /** Parse the `amy-movie-showtimews-daily-1` block.
   *
   *  Step 1: build a map from movie id → set of dates it plays (from the
   *  header tabs' `data-date` + `data-movie` attributes).
   *  Step 2: for each movie item, read the title and the times from
   *  `div.amy-movie-item-showtimes.amy-item-{id} span`, then cross-product
   *  with the dates to produce showtimes. */
  private def parseDailySection(doc: org.jsoup.nodes.Document, cinema: Cinema): Seq[CinemaMovie] =
    Option(doc.selectFirst("div.amy-movie-showtimews-daily-1")).toSeq.flatMap { dailyBlock =>
      // Build map: movieId → list of LocalDate
      val idToDates = scala.collection.mutable.Map.empty[String, List[LocalDate]]
      dailyBlock.select("a[data-date][data-movie]").asScala.foreach { a =>
        val dateStr  = a.attr("data-date")
        val movieIds = a.attr("data-movie").split(",").map(_.trim).filter(_.nonEmpty)
        val date = DailyDatePat.findFirstMatchIn(dateStr).flatMap { m =>
          Try(LocalDate.of(m.group(3).toInt, m.group(2).toInt, m.group(1).toInt)).toOption
        }
        date.foreach { d =>
          movieIds.foreach { id =>
            idToDates(id) = d :: idToDates.getOrElse(id, Nil)
          }
        }
      }

      // For each movie item, extract title + times from the amy-item-{id} div
      dailyBlock.select("div.amy-movie-item").asScala.toSeq.flatMap { item =>
        val title = Option(item.selectFirst("h3.amy-movie-field-title a"))
          .map(a => ScraperParse.stripFormatTags(a.text.trim))
          .filter(_.nonEmpty)
        val posterUrl = Option(item.selectFirst("div.amy-movie-item-poster img[src]"))
          .map(_.attr("abs:src")).filter(_.nonEmpty)
        val filmUrl = Option(item.selectFirst("h3.amy-movie-field-title a[href]"))
          .map(_.attr("abs:href")).filter(_.nonEmpty)

        // Find the amy-item-{id} div that belongs to this movie item.
        val showtimeDiv = item.select("div[class~=amy-item-]").asScala.headOption
        val movieId = showtimeDiv.flatMap { div =>
          div.classNames().asScala.find(_.startsWith("amy-item-")).map(_.stripPrefix("amy-item-"))
        }

        val times = showtimeDiv.toSeq.flatMap { div =>
          div.select("div.amy-movie-intro-times span").asScala
            .flatMap(s => parseTime(s.text))
        }

        val dates = movieId.map(id => idToDates.getOrElse(id, Nil)).getOrElse(Nil)

        val showtimes = for {
          d <- dates
          t <- times
        } yield Showtime(LocalDateTime.of(d, t), bookingUrl = None)

        title.filter(_ => showtimes.nonEmpty).map { t =>
          CinemaMovie(
            movie     = Movie(t),
            cinema    = cinema,
            posterUrl = posterUrl,
            filmUrl   = filmUrl,
            synopsis  = None,
            cast      = Seq.empty,
            director  = Seq.empty,
            showtimes = showtimes.sortBy(_.dateTime)
          )
        }
      }
    }

  // ── Weekly section ────────────────────────────────────────────────────────

  /** Parse the `amy-movie-showtimews-1` block (upcoming/weekly grid).
   *
   *  Each movie item contains `div.amy-cell` elements, each with a
   *  `div.amy-head` (day name + `DD.MM`) and `div.amy-intro-times > div`
   *  for times. Year is inferred from `today`. */
  private def parseWeeklySection(doc: org.jsoup.nodes.Document, cinema: Cinema,
                                  today: LocalDate): Seq[CinemaMovie] =
    Option(doc.selectFirst("div.amy-movie-showtimews-1")).toSeq.flatMap { weeklyBlock =>
      weeklyBlock.select("div.amy-movie-item").asScala.toSeq.flatMap { item =>
        val title = Option(item.selectFirst("h3.amy-movie-field-title a"))
          .map(a => ScraperParse.stripFormatTags(a.text.trim))
          .filter(_.nonEmpty)
        val posterUrl = Option(item.selectFirst("div.amy-movie-item-front img[src]"))
          .map(_.attr("abs:src")).filter(_.nonEmpty)
        val filmUrl = Option(item.selectFirst("h3.amy-movie-field-title a[href]"))
          .map(_.attr("abs:href")).filter(_.nonEmpty)

        val showtimes = item.select("div.amy-cell").asScala.toSeq.flatMap { cell =>
          // The amy-head contains two child divs: day-of-week name and "DD.MM"
          val dateText = cell.select("div.amy-head div").asScala
            .map(_.text.trim)
            .find(WeeklyDatePat.matches(_))
          val date = dateText.flatMap { s =>
            WeeklyDatePat.findFirstMatchIn(s).flatMap { m =>
              inferYear(m.group(1).toInt, m.group(2).toInt, today)
            }
          }
          val times = cell.select("div.amy-intro-times div").asScala
            .flatMap(d => parseTime(d.text))

          for {
            d <- date.toSeq
            t <- times
          } yield Showtime(LocalDateTime.of(d, t), bookingUrl = None)
        }

        title.filter(_ => showtimes.nonEmpty).map { t =>
          CinemaMovie(
            movie     = Movie(t),
            cinema    = cinema,
            posterUrl = posterUrl,
            filmUrl   = filmUrl,
            synopsis  = None,
            cast      = Seq.empty,
            director  = Seq.empty,
            showtimes = showtimes.sortBy(_.dateTime)
          )
        }
      }
    }
}
