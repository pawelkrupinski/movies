package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.movies.TitleNormalizer
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Bajka (Lublin) — the art-house cinema run by Centrum Kultury, at
 * ul. Radziszewskiego 8. Its WordPress (Astra) repertoire page at `/repertuar/`
 * server-renders the WHOLE advance window inline: one
 * `div.screening-day[id=screening-YYYYMMDD]` per date, each holding a
 * `div.screening-item` per film. The date picker above only toggles which
 * day-block is visible client-side, so a single page fetch carries every
 * screening — no per-date AJAX round-trip needed.
 *
 * Each `div.screening-item` carries:
 *   - `h4 a` → film title and the film's detail-page URL (`filmUrl`).
 *   - `img.screening-poster[src]` → poster.
 *   - `span.duration` (`"110 min"`) → runtime; `span.country` → production
 *     country, a slug like `wielka_brytania` de-slugged to spaces.
 *   - one `div.screening-link` per showtime: `span.time` (`HH:MM`) read under
 *     the enclosing day's date, and `data-url` → the ticketing host
 *     (`bajka-lublin.biletpro24.pl`), kept as the booking URL when present.
 *     Past screenings carry `class="screening-link is-past"` with an empty
 *     `data-url`; they're still real screenings of the day, and the day id
 *     carries the year, so they resolve to the right `LocalDateTime`.
 *
 * The listing has everything we display; there's no per-film detail page to
 * fetch (TMDB enriches the rest downstream). One `CinemaMovie` per title, with
 * the screenings merged and sorted.
 */
class KinoBajkaClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoBajkaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  def parseHtml(html: String): Seq[CinemaMovie] = {
    val slots = Jsoup.parse(html).select("div.screening-day[id]").asScala.toSeq.flatMap(parseDay)

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie     = Movie(title, runtimeMinutes = head.runtime, countries = head.countries),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = group.flatMap(_.filmUrl).headOption,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }
}

object KinoBajkaClient {

  val BaseUrl = "https://kinobajka.pl"
  val PageUrl = s"$BaseUrl/repertuar/"

  // `id="screening-YYYYMMDD"` on each day block.
  private val DayIdPat = """screening-(\d{8})""".r
  private val DayIdFmt = DateTimeFormatter.ofPattern("yyyyMMdd")

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    poster:    Option[String],
    filmUrl:   Option[String],
    runtime:   Option[Int],
    countries: Seq[String]
  )

  /** Parse one `div.screening-day` — its id gives the date that every
    * `span.time` inside it pairs with. */
  private def parseDay(day: Element): Seq[RawSlot] =
    dayDate(day.id).toSeq.flatMap { date =>
      day.select("div.screening-item").asScala.toSeq.flatMap(item => parseItem(item, date))
    }

  private def dayDate(id: String): Option[LocalDate] =
    DayIdPat.findFirstMatchIn(id).flatMap(m => Try(LocalDate.parse(m.group(1), DayIdFmt)).toOption)

  private def parseItem(item: Element, date: LocalDate): Seq[RawSlot] = {
    val link    = Option(item.selectFirst("div.title-age-group h4 a[href]"))
                    .orElse(Option(item.selectFirst("h4 a[href]")))
    val title   = link.map(l => TitleNormalizer.cinemaClean("kino-bajka", l.text.trim)).filter(_.nonEmpty)
    val filmUrl = link.map(_.attr("href")).filter(_.nonEmpty)
    val poster  = Option(item.selectFirst("img.screening-poster[src]"))
                    .map(_.attr("src")).filter(_.nonEmpty)
    val runtime = Option(item.selectFirst("span.duration")).map(_.text)
                    .flatMap(DurationPat.findFirstMatchIn).map(_.group(1).toInt)
    val countries = Option(item.selectFirst("span.country")).map(_.text.trim)
                      .filter(_.nonEmpty).map(parseCountries).getOrElse(Seq.empty)

    title match {
      case None => Seq.empty
      case Some(t) =>
        item.select("div.screening-link").asScala.toSeq.flatMap { slot =>
          val time = Option(slot.selectFirst("span.time")).map(_.text)
                       .flatMap(ScraperParse.parseHHmm)
          time.map { lt =>
            val booking = Option(slot.attr("data-url")).map(_.trim).filter(_.nonEmpty)
            RawSlot(t, LocalDateTime.of(date, lt), booking, poster, filmUrl, runtime, countries)
          }
        }
    }
  }

  private val DurationPat = """(\d+)\s*min""".r

  /** The `span.country` text is a comma-separated list of country slugs
    * (`wielka_brytania`, `francja, niemcy`). De-slug underscores to spaces and
    * keep the names verbatim — cross-cinema spelling is unified by the
    * `MovieRecord` merge, not here. */
  private def parseCountries(s: String): Seq[String] =
    s.split(",").map(_.trim.replace('_', ' ')).filter(_.nonEmpty).toSeq
}
