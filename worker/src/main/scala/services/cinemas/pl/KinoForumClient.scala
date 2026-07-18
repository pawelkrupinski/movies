package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.CinemaScraper

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Forum — the film screen of the Białostocki Ośrodek Kultury (BOK). The
 * venue publishes a single server-rendered repertoire page at `/repertuar/`
 * carrying its whole schedule (and a multi-year archive of past screenings) as
 * a flat list of `<div class="repertoire-row">` blocks, each tagged with:
 *   - `data-date="YYYY-MM-DD"` and `data-hour="HH:MM"` — the screening slot.
 *   - `.repertoire-row__title h3 a` — the film title (entity-decoded by jsoup;
 *     programme prefixes like "17. PRZEGLĄD NOWEGO KINA FRANCUSKIEGO:" are kept
 *     verbatim — `TitleNormalizer.ProgrammePrefix` handles them downstream).
 *   - `.repertoire-row__time` — the runtime as `NN'` (e.g. `128'`). Pure
 *     non-film cultural events on the same list (exhibitions, meetings) carry
 *     no runtime, so its presence is the film discriminator.
 *   - `.repertoire-row__production` — comma-separated production countries.
 *   - `a.more` — the booking link on the ticketing host
 *     (`bilety.bok.bialystok.pl`), absent for a handful of free screenings.
 *
 * The archive stretches back years, so `today` (injected for test-pinning, as
 * with Helios) gates out everything before the current day. The listing has
 * everything we surface; TMDB enriches the rest downstream, so there's no
 * per-film detail fetch.
 */
class KinoForumClient(
  http:  tools.HttpFetch,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoForumClient._

  override val cinema: Cinema = KinoForum

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = {
    val document   = Jsoup.parse(http.get(PageUrl))
    val slots = document.select("div.repertoire-row[data-date][data-hour]").asScala.toSeq
      .flatMap(parseRow)
      .filter(!_.dateTime.toLocalDate.isBefore(today))

    slots.groupBy(_.title).toSeq.map { case (title, group) =>
      val sorted = group.sortBy(_.dateTime)
      val first  = sorted.head
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = first.runtimeMinutes, countries = first.countries),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = sorted.map(s => Showtime(s.dateTime, s.booking)).distinctBy(s => (s.dateTime, s.bookingUrl))
      )
    }.sortBy(_.movie.title)
  }
}

object KinoForumClient {

  val BaseUrl = "https://bok.bialystok.pl"
  val PageUrl = s"$BaseUrl/repertuar/"

  private val DateFmt   = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val RuntimeRe = """(\d+)""".r

  private case class RawSlot(
    title:          String,
    dateTime:       LocalDateTime,
    booking:        Option[String],
    runtimeMinutes: Option[Int],
    countries:      Seq[String]
  )

  /** A repertoire row → its film screening, or `None` when the row is a
    * non-film event (no runtime) or lacks a parseable date/time/title. */
  private def parseRow(row: Element): Option[RawSlot] =
    for {
      date  <- Try(LocalDate.parse(row.attr("data-date"), DateFmt)).toOption
      time  <- ScraperParse.parseHHmm(row.attr("data-hour"))
      title <- Option(row.selectFirst(".repertoire-row__title h3 a")).map(_.text.trim).filter(_.nonEmpty)
      // Runtime presence is the film filter — exhibitions/meetings on the same
      // list have an empty `__time` cell.
      runtime <- Option(row.selectFirst(".repertoire-row__time")).map(_.text.trim)
                   .flatMap(t => RuntimeRe.findFirstIn(t)).flatMap(s => Try(s.toInt).toOption)
    } yield {
      val booking = Option(row.selectFirst("a.more")).map(_.attr("href")).filter(_.nonEmpty)
      val countries = Option(row.selectFirst(".repertoire-row__production")).map(_.text.trim).filter(_.nonEmpty)
        .map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq).getOrElse(Seq.empty)
      RawSlot(title, LocalDateTime.of(date, time), booking, Some(runtime), countries)
    }
}
