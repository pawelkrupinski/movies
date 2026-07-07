package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Awangarda 2 (Olsztyn). A bespoke Joomla site whose weekly repertoire is a
 * single article rendered as a flat run of sibling `<p>` blocks — no per-screening
 * container, no table. The blocks come in two shapes, in document order:
 *
 *   - A DAY HEADER: `"{Weekday} {DD.MM.}"` (e.g. `Piątek 19.06.`). Weekday is
 *     decorative; the `DD.MM` is the only date token. There is NO year anywhere on
 *     the page, so the year is derived from the injected `today` with the standard
 *     forward-roll (a `DD.MM` that already sits >6 months in the past belongs to
 *     next year — the December→January boundary).
 *   - A SCREENING: a `<p>` carrying an `<a>` (the film link) AND a `HH.MM` time,
 *     e.g. `<a>"Drugie życie"</a> - prod. Francja, Belgia : godz. 17.45 sala arte`.
 *     A screening belongs to the most recent day header seen. Navigation/info
 *     links ("RODO", "Ceny biletów", festival section headers) are `<a>`s with NO
 *     time, so requiring a time on the line excludes them.
 *
 * Title quirks the parser handles:
 *   - The link text is usually the film title wrapped in quotes (`"Drugie życie"`).
 *   - Retrospective/series screenings prefix the series name and quote the actual
 *     film (`Federico Fellini : Ciao a tutti "Wałkonie"`). Taking the LAST quoted
 *     substring recovers the film (`Wałkonie`); a link with no quotes falls back to
 *     the whole text.
 *
 * Tickets are phone-reservation only, so no booking URL is surfaced. The country
 * list ("- prod. Francja, Belgia : …") is captured when present; some lines omit it.
 */
class KinoAwangarda2Client(
  http:  HttpFetch,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")),
  override val cinema: Cinema = KinoAwangarda2
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoAwangarda2Client.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoAwangarda2Client.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    // Fetch OUTSIDE the Try so a 5xx/timeout from the venue's (flaky, shared-
    // hosting) server propagates and surfaces as a red uptime error — not a
    // silently swallowed empty, which `RetryingCinemaScraper` records as a
    // successful "0 showtimes" scrape (white on the uptime bar, indistinguishable
    // from a genuinely film-dormant venue). Only a PARSE failure on fetched HTML
    // is swallowed to an empty list. Mirrors the same guard in KinoZamekClient.
    val html = http.get(KinoAwangarda2Client.RepertoireUrl)
    Try(KinoAwangarda2Client.parse(html, today, cinema)).getOrElse(Seq.empty)
  }
}

object KinoAwangarda2Client {

  val BaseUrl       = "https://www.awangarda.olsztyn.pl"
  val RepertoireUrl = s"$BaseUrl/index.php?option=com_content&view=article&id=77&Itemid=104"

  /** A day header: a block whose text STARTS with `"{Weekday} DD.MM"` (trailing
   *  dot optional). Anchoring at the start is what separates a header block from
   *  a screening line, which never starts with a weekday word. */
  private val DayHeader =
    """^(?:Poniedziałek|Wtorek|Środa|Czwartek|Piątek|Sobota|Niedziela)\s+(\d{1,2})\.(\d{1,2})""".r

  /** The screening time on a film line: `HH.MM` (or `HH:MM`). The only `DD.MM`-
   *  shaped token on a screening line is the time (day headers are separate
   *  blocks), so the first match is the showtime. */
  private val ScreeningTime = """\b(\d{1,2})[.:](\d{2})\b""".r

  /** A quoted substring inside the link text. */
  private val Quoted = """"([^"]+)"""".r

  /** The production-country list: between `prod.` and the closing `:` that
   *  precedes the time. Absent on some lines (`- prod. 19.15`). */
  private val Countries = """(?i)prod\.\s*([^:]*?)\s*:""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, countries: Seq[String])

  def parse(html: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    // Walk the article's `<p>` blocks in document order, carrying the current
    // day forward across screenings until the next day header resets it.
    var currentDate: Option[LocalDate] = None
    val slots = document.select("p").asScala.toSeq.flatMap { p =>
      val text = p.text.trim
      DayHeader.findFirstMatchIn(text) match {
        case Some(m) =>
          currentDate = dateOf(m.group(1).toInt, m.group(2).toInt, today)
          Seq.empty
        case None =>
          currentDate.toSeq.flatMap(date => screeningSlot(p, text, date))
      }
    }

    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, None),
      distinctBy = _.dateTime
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title, countries = group.flatMap(_.countries).distinct),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  /** A screening slot from a `<p>`, or `None` when the block isn't a screening
   *  (no film link, or no time — a nav/info link or a festival section header). */
  private def screeningSlot(p: Element, text: String, date: LocalDate): Option[RawSlot] =
    for {
      link  <- Option(p.selectFirst("a"))
      title  = titleOf(link.text) if title.nonEmpty
      tm    <- ScreeningTime.findFirstMatchIn(text).flatMap(timeOf)
    } yield RawSlot(title, LocalDateTime.of(date, tm), countriesOf(text))

  /** The film title: the LAST quoted substring (recovers the film from a
   *  "Series : prefix \"Film\"" line), or the whole link text when unquoted.
   *  Format tags (2D/DUB/NAP) stripped. */
  private def titleOf(linkText: String): String = {
    val raw   = linkText.trim
    val title = Quoted.findAllMatchIn(raw).map(_.group(1)).toSeq.lastOption.getOrElse(raw)
    ScraperParse.stripFormatTags(title).trim
  }

  /** Production countries from `- prod. <list> : …`; empty when absent or when
   *  the captured chunk is just a time/number (the `- prod. 19.15` shape). */
  private def countriesOf(text: String): Seq[String] =
    Countries.findFirstMatchIn(text).map(_.group(1)).toSeq
      .flatMap(_.split(","))
      .map(_.trim)
      .filter(_.nonEmpty)
      .filterNot(_.matches(""".*\d.*"""))

  private def timeOf(m: scala.util.matching.Regex.Match): Option[LocalTime] =
    Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption

  /** The `DD.MM` mapped to a `LocalDate`, rolling forward into next year only
   *  when it sits >6 months before `today` (the Dec→Jan boundary) — mirrors
   *  `KinoMuzaClient`/`KinoBulgarskaClient`. */
  private def dateOf(day: Int, month: Int, today: LocalDate): Option[LocalDate] =
    Try {
      val candidate = LocalDate.of(today.getYear, month, day)
      if (candidate.isBefore(today.minusMonths(6))) candidate.plusYears(1) else candidate
    }.toOption
}
