package services.cinemas.pl

import tools.HttpFetch

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

/**
 * Kino Zamek — the cinema screen of the Zamek Książąt Pomorskich (Castle of the
 * Pomeranian Dukes) in Szczecin, read from the castle's OWN site.
 *
 * === Why not the MSI ticketing portal ===
 * This client used to take its showtimes from `bilety.zamek.szczecin.pl`, using
 * the castle's kino listing only as a film allow-list. That portal stopped
 * accepting TCP connections altogether between 2026-08-04 and 2026-08-08 — DNS
 * still resolves, but :443 and :80 both time out — so every scrape burned the
 * host's whole time budget and the venue went red.
 *
 * Losing it costs nothing, and the venue is better off: while the portal was
 * still UP on 2026-08-04 it advertised **zero** films, at a time when the
 * castle's own site was carrying a nine-film open-air season. The portal also
 * mixed concerts, workshops and the terrace season in with the cinema, which is
 * why the old client needed an allow-list, a slug approximation and a
 * venue-specific "lato na tarasach" marker to fish the films back out. Reading
 * the castle's own `/wydarzenia/kino/` category instead means the source is
 * films by construction, so all three of those heuristics are gone.
 *
 * === Shape ===
 * `/wydarzenia/kino/` lists the category's events, each linking to
 * `/wydarzenie/kino/<slug>/`. Every event page carries:
 *
 *   - `<p class="event-details">` — the event's dates as `dd-MM-yyyy`, comma
 *     separated. This is the only place a YEAR appears, so it is what resolves
 *     the year-less prose dates below.
 *   - `<h1>` — the event's name.
 *   - a flat `<p>` stream inside `div.content`, in which a screening is a
 *     paragraph like `4 sierpnia (wtorek), godzina 21:30` (the season pages
 *     write `godz. 19:00`, the summer cycle `godzina 21:30` — both accepted),
 *     usually followed by `dramat, USA/Francja 1984, 160’` and
 *     `Reżyseria: Milos Forman`.
 *
 * === One page is not one film ===
 * Two page shapes exist and the difference is load-bearing:
 *
 *   - A SINGLE-FILM page (the in-season norm — `casablanca`, `orly-republiki`)
 *     names the film in its `<h1>` and lists one or more dates for it. Its only
 *     bold heading is the programme strand ("SZCZECIŃSKIE ŚWIĘTO KLASYKI
 *     FILMOWEJ W KINIE ZAMEK"), which is emphatically NOT the film's title.
 *   - A CYCLE page (`zamkowe-noce-filmowe-2026`) holds a whole season — ten
 *     different films, each introduced by its own bold heading and followed by
 *     its date. Here the `<h1>` is the cycle's name, not a film.
 *
 * So the title cannot come from the heading always, nor from the `<h1>` always.
 * [[parseEvent]] pairs each screening with the nearest bold heading above it and
 * treats the page as a cycle only when those headings actually DIFFER between
 * screenings; otherwise the page is about one film and the `<h1>` names it. On
 * a single-film page every screening resolves to the same strand heading (or to
 * none at all), which is exactly the signal that the heading is not a title.
 *
 * === Dates ===
 * The prose carries no year, so each `(day, month)` is resolved against the
 * `event-details` list, which does. Only when that list has no matching day does
 * it fall back to the nearest occurrence to `today` — and screenings already in
 * the past are dropped rather than rolled forward a year, because "30 czerwca"
 * read in August is a screening that has happened, not one eleven months out.
 *
 * @parameter http   HTTP client (swap for `FakeHttpFetch` in tests).
 * @parameter cinema The [[Cinema]] source tag attached to every [[CinemaMovie]].
 * @parameter today  Calendar anchor; screenings before it are dropped. Defaults
 *               to the current Warsaw clock date.
 */
class KinoZamekClient(
  http:                HttpFetch,
  override val cinema: Cinema,
  today:               LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with OnlyMovieEventsFilter {

  import KinoZamekClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  // The castle's own kino category — films by construction, unlike the retired
  // MSI portal that mixed in concerts and workshops.
  override def sourceUrl: Option[String] = Some(ListingUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val urls = eventUrls(http.get(ListingUrl))

    // Per-event fetches are best-effort: one flaky page must not throw away the
    // rest of the programme. But a source that is wholly down must never read as
    // a quiet venue — if EVERY event page failed, propagate the first failure so
    // `RetryingCinemaScraper` retries and the bar goes red, rather than recording
    // a silent "0 showtimes" success that scrape-prune would read as the films
    // having stopped. (Same total-outage guard as `MsiClient`'s month walk.)
    val fetched = urls.map(url => url -> Try(http.get(url)))
    if (fetched.nonEmpty && fetched.forall(_._2.isFailure))
      fetched.head._2.failed.foreach(throw _)

    val films = fetched.collect { case (url, Success(html)) => parseEvent(html, url, today, cinema) }.flatten

    // One film routinely appears on TWO event pages — its own, and the festival
    // umbrella page that lists the whole month — so merge by title rather than
    // emitting near-duplicate cards. Only the film's own page carries a
    // `Reżyseria:` line, so the merge has to UNION the fields and anchor on the
    // page that has them; taking the first entry seen would drop every director
    // the moment the umbrella page happened to be scraped first.
    films.groupBy(_.movie.title).toSeq.sortBy(_._1).flatMap { case (_, group) =>
      val showtimes = group.flatMap(_.showtimes).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      val richest   = group.maxBy(_.director.size)
      if (showtimes.isEmpty) None
      else Some(richest.copy(showtimes = showtimes, director = group.flatMap(_.director).distinct))
    }
  }
}

object KinoZamekClient {

  val BaseUrl    = "https://zamek.szczecin.pl"
  val ListingUrl = s"$BaseUrl/wydarzenia/kino/"

  /** A screening line: `4 sierpnia (wtorek), godzina 21:30` or the season pages'
    * `21 czerwca (niedziela), godz. 19:00`. The weekday parenthetical is optional
    * because not every entry carries one. */
  private val ScreeningLine =
    """(?i)(\d{1,2})\s+(\p{L}+)\b[^,]*,\s*godz(?:ina)?\.?\s*(\d{1,2})[:.](\d{2})""".r

  /** `12-06-2026` inside the comma-separated `p.event-details` list. */
  private val EventDetailsDate = """(\d{2})-(\d{2})-(\d{4})""".r

  private val DirectorLine = """(?i)^Re[żz]yseria\s*:\s*(.+)$""".r

  /** The event pages linked from the kino category listing.
    *
    * Throws when the listing renders without its `#events-list` container: a
    * page we cannot read must not be reported as a venue with no films. That
    * distinction is the whole point — a silent zero is indistinguishable from a
    * dormant venue on the uptime bar, which is how a CMS migration can hide for
    * weeks. An `#events-list` that is present but holds no kino links IS a real
    * empty category, and returns empty. (Same guard as `KinoSfinksClient` /
    * `KinoStudioClient` / `MsiClient`.) */
  private[cinemas] def eventUrls(html: String): Seq[String] = {
    val document = Jsoup.parse(html, BaseUrl)
    if (document.selectFirst("#events-list") == null)
      throw new IllegalStateException(
        s"Kino Zamek listing at $ListingUrl rendered no #events-list container — the page shape drifted")
    document.select("a[href*='/wydarzenie/kino/']").asScala.toSeq
      .map(_.attr("abs:href").takeWhile(_ != '?'))
      .filter(_.nonEmpty)
      .distinct
  }

  /** One event page → the films it advertises, with their future screenings. */
  private[cinemas] def parseEvent(html: String, url: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val document  = Jsoup.parse(html, BaseUrl)
    val dated     = eventDates(document)
    val pageTitle = Option(document.selectFirst("h1")).map(_.text.trim).filter(_.nonEmpty)

    // Scope to the article body: the site's nav renders inside <p> too, and its
    // menu text would otherwise be picked up as a film heading.
    val paragraphs = Option(document.selectFirst("div.content")).getOrElse(document)
      .select("p").asScala.toSeq
      .map(element => Paragraph(collapse(element.text), isHeading(element)))
      .filter(_.text.nonEmpty)

    // Pair each screening line with the nearest bold heading above it.
    var heading = Option.empty[String]
    val screenings = paragraphs.zipWithIndex.flatMap { case (paragraph, index) =>
      ScreeningLine.findFirstMatchIn(paragraph.text) match {
        case Some(matched) =>
          for {
            month <- ScraperParse.PolishMonthsAnyCase.get(matched.group(2).toLowerCase)
            time  <- Try(LocalTime.of(matched.group(3).toInt, matched.group(4).toInt)).toOption
            date  <- resolveDate(matched.group(1).toInt, month, dated, today)
            if !date.isBefore(today)
          } yield Screening(date, time, heading, directorAfter(paragraphs, index))
        case None =>
          if (paragraph.isHeading) heading = Some(paragraph.text)
          None
      }
    }

    // A cycle page introduces each of its films with its OWN heading; a
    // single-film page has at most one heading and it is the programme strand,
    // not the film. So headings only name films when they actually vary.
    val isCycle = screenings.flatMap(_.heading).distinct.sizeIs > 1

    screenings
      .groupBy(screening => if (isCycle) screening.heading.orElse(pageTitle) else pageTitle)
      .toSeq
      .collect { case (Some(rawTitle), group) if rawTitle.nonEmpty =>
        CinemaMovie(
          movie     = Movie(ScraperParse.sentenceCase(stripStrand(rawTitle))),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = Some(url),
          synopsis  = None,
          cast      = Seq.empty,
          director  = group.flatMap(_.director).distinct,
          // The castle sells at the box office and through the portal that went
          // dark, so there is no per-screening booking link to carry.
          showtimes = group.map(s => Showtime(LocalDateTime.of(s.date, s.time), bookingUrl = None))
            .distinctBy(_.dateTime).sortBy(_.dateTime)
        )
      }
      .sortBy(_.movie.title)
  }

  private case class Paragraph(text: String, isHeading: Boolean)
  private case class Screening(date: LocalDate, time: LocalTime, heading: Option[String], director: Seq[String])

  /** The site bolds the line that introduces a screening. Bold ALONE is the
    * marker — deliberately not the 14pt span the cycle pages also carry, because
    * on the festival's umbrella page 14pt marks the WEEK ("II TYDZIEŃ POKAZÓW –
    * GODARD, WAJDA…") while each film's own title is bold-but-not-14pt. Keying
    * on 14pt there produced three "films" named after weeks of the programme. */
  private def isHeading(element: Element): Boolean =
    element.select("strong").asScala.nonEmpty

  /** The `Reżyseria:` line belonging to the screening at `index` — the first one
    * before the NEXT screening. The next screening is the only boundary that
    * works: the metadata lines under a date ("USA 1942 (102 min.)", "Reżyseria:
    * Michael Curtiz") are bold on the season pages, so stopping at the next bold
    * paragraph would stop before reading any of them. */
  private def directorAfter(paragraphs: Seq[Paragraph], index: Int): Seq[String] =
    paragraphs.drop(index + 1)
      .takeWhile(p => ScreeningLine.findFirstMatchIn(p.text).isEmpty)
      .collectFirst { case Paragraph(DirectorLine(names), _) => names }
      .toSeq
      .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))

  /** The dates in `p.event-details` (`12-06-2026, 13-06-2026, …`) — the only
    * place on the page that carries a year. */
  private def eventDates(document: Document): Seq[LocalDate] =
    Option(document.selectFirst("p.event-details")).toSeq.flatMap { element =>
      EventDetailsDate.findAllMatchIn(element.text).flatMap { matched =>
        Try(LocalDate.of(matched.group(3).toInt, matched.group(2).toInt, matched.group(1).toInt)).toOption
      }
    }

  /** Resolve a year-less `(day, month)` against the page's own dated list, then
    * fall back to the occurrence nearest `today`.
    *
    * Nearest, not NEXT: a cycle page lists the whole season, so in August it
    * still shows June's opening night. Rolling that forward the way a
    * next-occurrence rule would puts a screening eleven months into the future
    * that nobody is going to; reading it as this year's (and letting the caller
    * drop it as past) is what actually happened. */
  private def resolveDate(day: Int, month: Int, dated: Seq[LocalDate], today: LocalDate): Option[LocalDate] =
    dated.find(d => d.getDayOfMonth == day && d.getMonthValue == month)
      .orElse {
        Seq(today.getYear - 1, today.getYear, today.getYear + 1)
          .flatMap(year => Try(LocalDate.of(year, month, day)).toOption)
          .minByOption(candidate => math.abs(candidate.toEpochDay - today.toEpochDay))
      }

  /** Drop the programme-strand suffix a cycle page appends to a film's heading
    * ("MILCZENIE OWIEC – JONATHAN DEMME", "PIĘKNOŚĆ DNIA – BUÑUEL. NIECH ŻYJĄ
    * KAJDANY"). Without this the same film reads differently on the festival
    * umbrella page and on its own event page, and would render as two cards.
    *
    * Only the en/em-dash separates a strand — NOT the colon the old MSI-era slug
    * derivation also split on, which would truncate "Zamkowe Noce Filmowe:
    * Szkoda, że nareszcie" to the cycle's name.
    *
    * Applied to `<h1>` titles too, not just headings: the DKF strand names itself
    * in its page title ("Pociągi – Dyskusyjny Klub Filmowy Zamek") while the
    * umbrella page calls the same screening "POCIĄGI – …", and the two rendered
    * as two cards for one 18:00 showing until both sides were stripped. */
  private def stripStrand(title: String): String =
    title.split("[–—]").head.trim match {
      case stripped if stripped.nonEmpty => stripped
      case _                             => title
    }

  /** Collapse the runs of whitespace and non-breaking spaces the CMS leaves in
    * titles ("AMADEUSZ  (Amadeus)"). */
  private def collapse(text: String): String = text.replace(' ', ' ').replaceAll("\\s+", " ").trim
}
