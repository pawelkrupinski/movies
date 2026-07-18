package services.cinemas.pl

import tools.{HttpFetch, ParallelDetailFetch}
import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{Instant, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Muzeum — the cinema at the Museum of the Second World War in Gdańsk
 * (muzeum1939.pl), part of the Trójmiasto scope. The repertoire is a
 * server-rendered listing on a bespoke "Webset" CMS: each screening is a
 * `li.screening-it-wrapper` carrying title, genre/country/year/duration/director,
 * a `screening-meta-hour`, and a `bilety.muzeum1939.pl` booking link. The
 * listing only renders ONE day at a time (the requested day), but the calendar
 * widget links every `has-events` day at `repertuar,ts:<start>,te:<end>` —
 * fetching each full day page gives the multi-day programme, mirroring
 * NoveKino's day-walk.
 *
 * The per-screening date is rendered as `DD.MM` with no year, so the year is
 * taken from the day link's `timestamp` (the day-start Unix epoch in Europe/Warsaw),
 * which avoids any new-year wraparound guesswork. Runtime/genres/country/director
 * are read off the listing; TMDB still enriches the rest downstream.
 *
 * Only `li.screening-it-wrapper` is scoped, so the site's navigation menus
 * (which reuse the `attr-value-title` class) never leak in as "films"; the
 * `/repertuar` page is films only — non-film museum events live on a separate
 * `/wydarzenia` listing.
 */
class KinoMuzeumGdanskClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper with OnlyMovieEventsFilter {

  import KinoMuzeumGdanskClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val base     = http.get(RepertoireUrl)
    val dayLinks = parseDayLinks(base)
    val dayPages = ParallelDetailFetch.keyed("kino-muzeum-days", dayLinks, 1.minute)(l => s"$BaseUrl${l.href}") { url =>
      Try(http.get(url)).toOption
    }
    val slots = dayLinks.flatMap { link =>
      dayPages.getOrElse(link, None).toSeq.flatMap(html => parseDay(html, link.date))
    }
    group(slots, cinema)
  }

  /** Calendar `a.day.has-events` links, each with the day-start epoch (`timestamp`)
   *  that fixes the screening year. The base page is itself one requested day,
   *  but we walk every linked day uniformly rather than special-casing it. */
  def parseDayLinks(html: String): Seq[DayLink] =
    DayLinkPat.findAllMatchIn(html).map { m =>
      val timestamp   = m.group(2).toLong
      val date = Instant.ofEpochSecond(timestamp).atZone(Warsaw).toLocalDate
      DayLink(m.group(1), date)
    }.toList.distinctBy(_.href)
}

object KinoMuzeumGdanskClient {

  val BaseUrl       = "https://www.muzeum1939.pl"
  val RepertoireUrl = s"$BaseUrl/kino-muzeum/repertuar"

  private val Warsaw = ZoneId.of("Europe/Warsaw")

  // `<a href="/kino-muzeum/repertuar,ts:1780610400,te:1780696799" class="day has-events…"`
  // — group(1) is the relative href, group(2) the day-start epoch.
  private val DayLinkPat =
    """href="(/kino-muzeum/repertuar,ts:(\d+),te:\d+)" class="day has-events""".r

  final case class DayLink(href: String, date: java.time.LocalDate)

  private[cinemas] final case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    poster:    Option[String],
    countries: Seq[String],
    genres:    Seq[String],
    director:  Seq[String],
    runtime:   Option[Int],
    year:      Option[Int],
    filmUrl:   Option[String]
  )

  /** Screenings on one day page. The page's `screening-meta-date` is `DD.MM`
   *  with no year, so we pair the page's already-known `date` with the
   *  `screening-meta-hour` instead of re-parsing the displayed date. */
  private[cinemas] def parseDay(html: String, date: java.time.LocalDate): Seq[RawSlot] =
    Jsoup.parse(html).select("li.screening-it-wrapper").asScala.toSeq.flatMap { li =>
      val title = text(li, "div.attr-value-title")
      val hour  = text(li, "span.screening-meta-hour").flatMap(ScraperParse.parseHHmm)
      (title, hour) match {
        case (Some(t), Some(h)) =>
          val booking = Option(li.selectFirst("a.item-tickets[href]")).map(_.attr("href")).filter(_.nonEmpty)
          val poster  = Option(li.selectFirst("div.item-image-thumb img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                          .map(u => if (u.startsWith("http")) u else BaseUrl + u)
          val filmUrl = Option(li.selectFirst("a.h-underline[href]")).map(_.attr("href")).filter(_.nonEmpty)
                          .map(u => if (u.startsWith("http")) u else BaseUrl + u)
          Some(RawSlot(
            title     = t,
            dateTime  = h.atDate(date),
            booking   = booking,
            poster    = poster,
            countries = splitField(text(li, "span.movie-meta-country").map(stripLabel("Kraj"))),
            genres    = splitField(text(li, "span.movie-meta-categories")).map(tools.TextNormalization.titleCaseIfAllLower),
            director  = splitField(text(li, "span.movie-meta-director").map(stripDirectorLabel)),
            runtime   = text(li, "span.movie-meta-duration").flatMap(parseRuntime),
            year      = text(li, "span.movie-meta-year").flatMap(parseYear),
            filmUrl   = filmUrl
          ))
        case _ => Seq.empty
      }
    }

  /** One `CinemaMovie` per film title; showtimes deduped by (dateTime, booking)
   *  and sorted. Shares the listing metadata captured off the first slot. */
  private[cinemas] def group(slots: Seq[RawSlot], cinema: Cinema): Seq[CinemaMovie] =
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, rows, showtimes) =>
      val primary = rows.head
      CinemaMovie(
        movie     = Movie(
          title          = title,
          runtimeMinutes = rows.flatMap(_.runtime).headOption,
          releaseYear    = primary.year,
          countries      = primary.countries,
          genres         = primary.genres
        ),
        cinema    = cinema,
        posterUrl = rows.flatMap(_.poster).headOption,
        filmUrl   = rows.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = primary.director,
        showtimes = showtimes
      )
    }

  private def text(element: Element, selector: String): Option[String] =
    Option(element.selectFirst(selector)).map(_.text.trim).filter(_.nonEmpty)

  // `screening-meta-duration` reads "106 min." — pull the leading integer,
  // bounded to a sane feature-length window.
  private def parseRuntime(s: String): Option[Int] =
    """(\d+)""".r.findFirstMatchIn(s).flatMap(m => Try(m.group(1).toInt).toOption).filter(n => n >= 30 && n <= 300)

  // `span.movie-meta-year` folds in its `Rok produkcji` label, so `.text()`
  // reads "Rok produkcji 1976" — pull the 4-digit production year out of it.
  private[cinemas] def parseYear(s: String): Option[Int] =
    """\b(?:19|20)\d{2}\b""".r.findFirstMatchIn(s).map(_.matched.toInt)

  // Director is rendered as "Reż.: Elaine May"; drop the label.
  private def stripDirectorLabel(s: String): String =
    s.replaceFirst("""(?i)^Re[żz]\.?:?\s*""", "").trim

  // Country / year cells fold a `visually-hidden` label ("Kraj", "Rok…") into
  // their text; Jsoup's `.text()` surfaces it, so strip the leading label word.
  private def stripLabel(label: String)(s: String): String =
    s.replaceFirst(s"(?i)^$label\\s*", "").trim

  private def splitField(s: Option[String]): Seq[String] =
    s.toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
}
