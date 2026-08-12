package services.cinemas.pl

import scala.math.Ordering.Implicits.infixOrderingOps
import services.cinemas.common.CinemaScraper

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try
import services.movies.TitleNormalizer

/**
 * Kino Kinematograf w Łodzi — the cinema inside the Muzeum Kinematografii
 * w Łodzi (Palace of Poznański at pl. Zwycięstwa 1). The repertoire at
 * `/kino/repertuar-kina/` is a server-rendered WordPress page listing the
 * coming weeks' screenings, one `article.cwb-movie-item` per showing.
 * (The museum restructured its URLs in mid-2026; the old `/repertuar/`
 * still 301s here, but we address the page the site publishes today rather
 * than lean on a redirect that could be dropped.)
 *
 * Each `cwb-movie-item` carries:
 *   - `a.cwb-movie-card-link[title="Przejdź do seansu: <raw>"]` — the raw
 *     title, which follows the pattern `Film Name (Year), reż. Director` or
 *     `Programme Prefix: Film Name (Year)`. The director suffix is stripped
 *     so TMDB can match the bare title; programme prefixes (e.g. "Klasyk w
 *     kinie:", "Mały Kinematograf:", "Federico Fellini: ciao a tutti! –")
 *     are kept verbatim and handled by downstream `TitleNormalizer`.
 *   - `a.cwb-movie-card-link[href]` — the film's detail-page URL, used as
 *     `filmUrl`.
 *   - `div.date-time` — date + time in `DD.MM.YYYY HH:MM` format (rendered
 *     with extra whitespace between the two). The year is always present, so
 *     no year-inference is needed.
 *   - `img.wp-post-image[data-src]` — the poster image (lazy-loaded).
 *
 * Booking is via `sklep.kinomuzeum.pl/MSI/mvc/pl/` (general page) — the
 * listing does not carry per-screening booking URLs, so `bookingUrl` is
 * always `None`. TMDB enriches runtime, genres and synopsis downstream.
 *
 * The listing page is filtered to screenings on or after `today` so that
 * past events (which the CMS keeps in the listing for archive purposes)
 * are not included.
 *
 * A zero-screening parse is only reported as empty when the widget ACCOUNTS
 * for being empty — see `advertisedScreenings`. Otherwise it throws, so a
 * markup change surfaces RED on /uptime instead of white, where it would be
 * indistinguishable from a film-dormant venue.
 */
class KinematografLodzClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")),
  titles:           TitleNormalizer
) extends CinemaScraper {

  import KinematografLodzClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  // The scraped page itself, so /uptime's link opens what the parser read.
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl), today, cinema, titles)
}

object KinematografLodzClient {

  val BaseUrl       = "https://muzeumkinematografii.pl"
  val RepertoireUrl = s"$BaseUrl/kino/repertuar-kina/"

  // One screening's card.
  private val CardSelector = "article.cwb-movie-item"
  // The widget's own item counter ("19 wydarzeń" / "0 wydarzeń") and the day
  // strip above the cards, whose tabs read "3 seanse" or "brak seansów". Either
  // is enough to account for an empty parse; the day strip is kept as a second
  // signal so renaming just one of them cannot turn a broken page white again.
  private val ItemCounterSelector = "span.items-counte"
  private val DayTabSelector      = "a.cinema-day-item"
  private val DayCountSelector    = "div.day-count"
  private val LeadingCountPat     = """^\s*(\d+)""".r

  private val DateTimePat = """(\d{2}\.\d{2}\.\d{4})\s+(\d{2}:\d{2})""".r
  private val DateFmt     = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  // The raw title carries `(YYYY)` and, for most films, a `, reż. Director`
  // suffix that `cleanTitle` strips for display. Both are TMDB-identity hints,
  // so extract them before the strip.
  private val ParenYearPat = """\((?:19|20)\d{2}\)""".r
  // Director list after the `reż.` marker, bounded by the first `(` (a trailing
  // `(YYYY)`), `•` (an event/discussion suffix), or the string end. The bare
  // `, Director` form some rows use (no `reż.`) is deliberately not matched —
  // it's indistinguishable from a subtitle and risks false positives.
  private val DirectorPat  = """(?i)reż\.\s*([^(•]+)""".r

  /** The `(YYYY)` production year in the raw title, if present. */
  def parseYear(raw: String): Option[Int] =
    ParenYearPat.findFirstMatchIn(raw).map(_.matched.filter(_.isDigit).toInt)

  /** Director(s) from the `reż. …` suffix, comma-split, with any trailing
   *  sentence punctuation (`Maciej Drygas.`) and empty fragments dropped. Empty
   *  when the title carries no `reż.` marker. */
  def parseDirectors(raw: String): Seq[String] =
    DirectorPat.findFirstMatchIn(raw).map(_.group(1)).toSeq
      .flatMap(_.split(","))
      .map(_.trim.stripSuffix(".").trim)
      .filter(_.nonEmpty)

  /** Strips the `", reż. Director Name"` / `", Director Name"` director suffix
    * and the trailing `" (YYYY)"` release-year suffix the museum appends to the
    * raw title — now via the editable "kino-kinematograf" rules. Delegates so it
    * stays unit-testable here. Examples:
    *   "Znaki Pana Śliwki (2025), reż. Urszula Morga, …" → "Znaki Pana Śliwki"
    *   "Zawieście czerwone latarnie (1991), Zhang Yimou" → "Zawieście czerwone latarnie"
    *   "Klasyk w kinie: Rozmowa (1973)" → "Klasyk w kinie: Rozmowa"
    */
  private[cinemas] def cleanTitle(raw: String, titles: TitleNormalizer): String =
    titles.cinemaClean("kino-kinematograf", raw)

  /** How many screenings the repertoire widget SAYS it is showing, or `None` if
    * this page is not the repertoire widget at all.
    *
    * The widget states its own size twice: `span.items-counte` ("0 wydarzeń")
    * and the day strip, whose tabs each read "3 seanse" or "brak seansów". The
    * counter wins when present; the day-tab sum is the fallback. Anything that
    * is neither — the homepage, a soft-404, a slug rename landing elsewhere —
    * yields `None`, and its emptiness says nothing about the venue's programme.
    *
    * Deliberately keyed on those two markers and NOT on
    * `div.movies-tickets-inner` or `div.cwb-movie-empty-state`: the museum's
    * HOMEPAGE carries both of those for its own "coming soon" carousel, so
    * either one would accept the wrong page as a schedule. */
  def advertisedScreenings(document: Document): Option[Int] = {
    def leadingCount(text: String): Int =
      LeadingCountPat.findFirstMatchIn(text).map(_.group(1).toInt).getOrElse(0)

    val counter = Option(document.selectFirst(ItemCounterSelector)).map(e => leadingCount(e.text.trim))
    val dayTabs = document.select(DayTabSelector).asScala.toSeq
    counter.orElse {
      Option.when(dayTabs.nonEmpty) {
        dayTabs.flatMap(t => Option(t.selectFirst(DayCountSelector)))
          .map(e => leadingCount(e.text.trim))
          .sum
      }
    }
  }

  private[cinemas] def parseHtml(html: String, today: LocalDate, cinema: Cinema, titles: TitleNormalizer): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html)
    val cards    = document.select(CardSelector).asScala.toSeq

    // A zero-card page is only believable when the widget accounts for it. Same
    // guard as KinoSfinksClient / KinoStudioClient / MsiClient, plus the
    // count cross-check the counter makes possible: a page advertising
    // screenings it no longer renders as `CardSelector` is a restyle, not an
    // empty venue, and must go RED rather than silently white.
    if (cards.isEmpty) advertisedScreenings(document) match {
      case None =>
        throw new IllegalStateException(
          s"No schedule at $RepertoireUrl — the page rendered neither an `$CardSelector` " +
          s"nor the repertoire widget's own `$ItemCounterSelector` / `$DayTabSelector` " +
          "markers (CMS migration? soft-404? redirect to another page?)")
      case Some(advertised) if advertised > 0 =>
        throw new IllegalStateException(
          s"No screenings parsed at $RepertoireUrl, but the repertoire widget advertises " +
          s"$advertised — the screening card is no longer `$CardSelector` (restyle?)")
      case Some(_) => Seq.empty
    }
    else cards.flatMap(parseItem(_, today, cinema, titles))
  }

  private def parseItem(item: Element, today: LocalDate, cinema: Cinema, titles: TitleNormalizer): Option[CinemaMovie] = {
    val link = Option(item.selectFirst("a.cwb-movie-card-link[href]"))
    val rawTitle = link.map(_.attr("title"))
                     .map(_.replaceFirst("^Przejdź do seansu:\\s*", "").trim)
                     .filter(_.nonEmpty)
    val title   = rawTitle.map(cleanTitle(_, titles)).filter(_.nonEmpty)
    val filmUrl = link.map(_.attr("href")).filter(_.nonEmpty)

    val dtText = Option(item.selectFirst("div.date-time")).map(_.text.trim).getOrElse("")
    val dtOpt  = DateTimePat.findFirstMatchIn(dtText).flatMap { m =>
      Try {
        val date = LocalDate.parse(m.group(1), DateFmt)
        val time = ScraperParse.parseHHmm(m.group(2))
        time.map(LocalDateTime.of(date, _))
      }.toOption.flatten
    }

    // Filter out past events (the CMS keeps the full year in the listing).
    val dtFiltered = dtOpt.filter(_.toLocalDate >= today)

    val poster = Option(item.selectFirst("img.wp-post-image[data-src]"))
                   .map(_.attr("data-src"))
                   .filter(_.nonEmpty)
                   .orElse(Option(item.selectFirst("img.wp-post-image[src]"))
                     .map(_.attr("src")).filter(_.startsWith("http")))

    for {
      t  <- title
      dt <- dtFiltered
    } yield CinemaMovie(
      movie     = Movie(title = t, rawTitle = rawTitle, releaseYear = rawTitle.flatMap(parseYear)),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = filmUrl,
      synopsis  = None,
      cast      = Seq.empty,
      director  = rawTitle.toSeq.flatMap(parseDirectors),
      showtimes = Seq(Showtime(dt, bookingUrl = None))
    )
  }
}
