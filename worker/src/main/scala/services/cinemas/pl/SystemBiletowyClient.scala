package services.cinemas.pl

import services.cinemas.common.ScraperParse
import services.movies.TitleNormalizer
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for cinemas ticketed through the VisualSoft ticketing platform
 * — branded `systembiletowy.pl` when a venue takes the vendor's subdomain (e.g.
 * `shd.systembiletowy.pl` for the Suchedniów cultural centre's Kino Kuźnica),
 * but the SAME software is also white-labelled onto venues' own domains
 * (`bilety.kino.bochnia.pl`, `kgl.systembiletowy.pl`, …). The instance homepage
 * (`<base>/index.php`) is server-rendered in one of four skins:
 *
 *   1. `table.tbl_repertoire` rows — `td.title a` / `td.date span.day|hour` /
 *      `td.link a` (`repertoire.html?id=N` booking link).
 *   2. Bootstrap `div.event-item` rows — `div.title a` / `div.date`
 *      ("… 10 czerwca 2026 … godz. 13:30") with a `repertoire.html` link.
 *   3. The `/css/visual9` skin — `div.event-item[data-date][data-time]`
 *      carrying the ISO date + time as attributes, an `h2`/`h3.event-title`, and a
 *      `/index.php/kup-bilet/…` booking link. A venue on this skin can ALSO
 *      carry a `data-group` attribute naming which category the event belongs
 *      to (empty for every venue seen until BCKino, Bytom, which sells
 *      theatre/workshops/concerts through the same listing and tags each
 *      event's category — "BCKino" for films, "Warsztaty"/"Koncerty"/
 *      "Spotkanie autorskie"/"BECEK CZYTA" for everything else). `filmGroups`
 *      names the category value(s) that ARE films for such a venue; when
 *      non-empty, only matching events are kept, and a "<group> – "/"<group> -
 *      " title prefix the venue's own listing glues on ("BCKino – Kandydaci
 *      śmierci") is stripped. Left empty (the default) for every other venue,
 *      which changes nothing.
 *   4. The "repertoire-once" skin (Kino Orzeł, Ustrzyki Dolne — and Kino CKiB
 *      Nowa Sarzyna, whose booking link reads `kup-bilet/…` instead of
 *      `repertoire.html?id=N`, otherwise the identical markup) —
 *      `div.repertoire-once.row.<yyyy-mm-dd>` per screening (a same-classed
 *      `div.repertoire-once.date-separator` header groups them visually but
 *      carries no data of its own), `div.title a` / `div.link a` around the
 *      booking link, and the day-of-week-prefixed date + "godz. HH:MM" time
 *      mashed into one `div.date`. Titles here carry a "-Film"/"- Film"
 *      boilerplate word ahead of the format tag ("…-Film 2D", "… - Film 2D
 *      dubbing") that isn't a real format/version word, so it's peeled
 *      before the shared format-tag stripping runs.
 *
 * One instance per venue, captured by its `baseUrl` + `cinema` (+ `filmGroups`
 * for a venue on skin 3 that mixes categories), so adding a VisualSoft-hosted
 * cinema is a catalog line, not a new client (OCP).
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class SystemBiletowyClient(http: HttpFetch, baseUrl: String, override val cinema: Cinema,
                           titles: TitleNormalizer, filmGroups: Set[String] = Set.empty)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    SystemBiletowyClient.parse(http.get(s"$baseUrl/index.php"), cinema, baseUrl, titles, filmGroups)
}

object SystemBiletowyClient {

  // "12 czerwca 2026" — day, Polish genitive month, year (all present).
  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], format: List[String])

  // The "repertoire-once" skin's boilerplate "-Film"/"- Film" word ahead of the
  // format tag ("…-Film 2D", "… - Film 2D dubbing") — not a real format/version
  // word, so it's peeled before FormatTags sees the title. Restricted (via the
  // lookahead) to where it's immediately followed by a real format/version word,
  // so a title that legitimately contains "Film" is never touched.
  private val FilmBoilerplate =
    """(?i)[-–—]\s*Film\b(?=\s+(?:2D|3D|IMAX|4DX|dolby|atmos|dubbing|dubb|dub|napisy|nap|lektor|lek)\b)""".r

  def parse(html: String, cinema: Cinema, baseUrl: String, titles: TitleNormalizer,
            filmGroups: Set[String] = Set.empty): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, baseUrl)
    // Per-cinema title cleanup (PerCinema rules) on top of the shared cleanTitle,
    // plus the format/language tokens peeled off the title so the dub/subtitle
    // screenings merge onto one row AND each keeps its language badge.
    def clean(raw: String): (String, List[String]) =
      (titles.cinemaClean(cinema.slug, cleanTitle(raw)), ScraperParse.extractFormatTags(raw)._2)

    val tblSlots = document.select("table.tbl_repertoire tr").asScala.toSeq.flatMap { tr =>
      // Only rows that are a real screening carry a repertoire/booking link.
      if (tr.selectFirst("a[href*=repertoire.html]") == null) None
      else for {
        titleElement <- Option(tr.selectFirst("td.title a"))
        titled   = clean(titleElement.text) if titled._1.nonEmpty
        dayText <- Option(tr.selectFirst("td.date span.day")).map(_.text)
        date    <- ScraperParse.parseDayMonthYear(dayText)
        time    <- Option(tr.selectFirst("td.date span.hour")).flatMap(h => ScraperParse.parseHHmm(h.text))
      } yield RawSlot(
        title    = titled._1,
        dateTime = date.atTime(time),
        booking  = Option(tr.selectFirst("td.link a[href]")).map(_.attr("abs:href"))
                     .filter(_.nonEmpty).orElse(Option(titleElement.attr("abs:href")).filter(_.nonEmpty)),
        format   = titled._2
      )
    }

    // Alternate Bootstrap-grid skin (Pszczyna, Żory, Oświęcim): one
    // `div.event-item` per screening, with the Polish full date + time mashed
    // into `div.date` ("… 10 czerwca 2026 … godz. 13:30") and the title/booking
    // in `div.title a`.
    val altSlots = document.select("div.event-item:has(a[href*=repertoire.html])").asScala.toSeq.flatMap { item =>
      for {
        titleElement <- Option(item.selectFirst("div.title a"))
        titled   = clean(titleElement.text) if titled._1.nonEmpty
        dateText <- Option(item.selectFirst("div.date")).map(_.text)
        date    <- ScraperParse.parseDayMonthYear(dateText)
        time    <- ScraperParse.parseHHmm(dateText)
      } yield RawSlot(
        title    = titled._1,
        dateTime = date.atTime(time),
        booking  = Option(titleElement.attr("abs:href")).filter(_.nonEmpty),
        format   = titled._2
      )
    }

    // Current `/css/visual9` skin (kgl/kck.systembiletowy.pl, bilety.kino.bochnia.pl,
    // bck.systembiletowy.pl): one `div.event-item` per screening with the ISO date
    // + time as data attributes, the title in `.event-title` (an h3 on most
    // instances, an h2 on Bochnia's since ~2026-09-21 — so the heading level is
    // never part of the selector), and a `kup-bilet`
    // booking link. The booking-link slug embeds the FIRST screening's date, not
    // this row's, so the showtime is read from the attributes — never parsed out
    // of the href. When `filmGroups` is non-empty (a venue mixing categories,
    // e.g. BCKino), only events whose `data-group` is one of them are kept, and
    // that group's "<group> – "/"<group> - " title prefix is peeled.
    val attrSlots = document.select("div.event-item[data-date]").asScala.toSeq
      .filter(item => filmGroups.isEmpty || filmGroups.contains(item.attr("data-group")))
      .flatMap { item =>
        for {
          titleElement <- Option(item.selectFirst(".event-title"))
          rawTitle = stripGroupPrefix(titleElement.text, item.attr("data-group"))
          titled   = clean(rawTitle) if titled._1.nonEmpty
          day     <- Try(LocalDate.parse(item.attr("data-date"))).toOption
          time    <- ScraperParse.parseHHmm(item.attr("data-time"))
        } yield RawSlot(
          title    = titled._1,
          dateTime = day.atTime(time),
          booking  = Option(item.selectFirst("a[href*=kup-bilet]")).map(_.attr("abs:href")).filter(_.nonEmpty),
          format   = titled._2
        )
      }

    // "repertoire-once" skin (Ustrzyki Dolne; Nowa Sarzyna on the `kup-bilet`
    // link variant): one div.repertoire-once.row per screening — guarded by
    // `:has(a[href*=repertoire.html], a[href*=kup-bilet])` so the same-classed
    // `div.repertoire-once.row.no-repertoire` "Brak wydarzeń…" placeholder (which
    // carries no such link) is never mistaken for a real screening.
    val repertoireOnceSlots = document.select(
      "div.repertoire-once.row:has(a[href*=repertoire.html], a[href*=kup-bilet])"
    ).asScala.toSeq
      .flatMap { item =>
        for {
          titleElement <- Option(item.selectFirst("div.title a"))
          titled   = clean(FilmBoilerplate.replaceFirstIn(titleElement.text, "")) if titled._1.nonEmpty
          dateText <- Option(item.selectFirst("div.date")).map(_.text)
          date    <- ScraperParse.parseDayMonthYear(dateText)
          time    <- ScraperParse.parseHHmm(dateText)
        } yield RawSlot(
          title    = titled._1,
          dateTime = date.atTime(time),
          booking  = Option(item.selectFirst("div.link a[href*=repertoire.html], div.link a[href*=kup-bilet]")).map(_.attr("abs:href"))
                       .filter(_.nonEmpty).orElse(Option(titleElement.attr("abs:href")).filter(_.nonEmpty)),
          format   = titled._2
        )
      }

    val slots = (tblSlots ++ altSlots ++ attrSlots ++ repertoireOnceSlots).distinctBy(s => (s.title, s.dateTime, s.booking))
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
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

  /** Drop the trailing `dubbing`/`napisy`/… version tag (so the same film's
   *  dubbed and subtitled screenings merge into one row) and sentence-case the
   *  result. Tag stripping is shared with the other portal clients. */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(ScraperParse.stripFormatTags(raw))

  /** Peel a "<group> – "/"<group> - " prefix a mixed-category venue's own
   *  listing glues onto its title ("BCKino – Kandydaci śmierci" → "Kandydaci
   *  śmierci"), case-insensitively. A no-op when `group` is empty (every venue
   *  before BCKino, whose `data-group` is unset). */
  private def stripGroupPrefix(title: String, group: String): String =
    if (group.isEmpty) title
    else title.replaceFirst("(?i)^" + java.util.regex.Pattern.quote(group) + """\s*[-–—]\s*""", "")
}
