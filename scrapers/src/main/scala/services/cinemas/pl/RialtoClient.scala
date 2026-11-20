package services.cinemas.pl

import java.time.LocalDateTime
import java.util.Locale

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.ScraperParse
import services.cinemas.common.SlotsToMovies
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ListingPages}
import tools.{HttpFetch, HttpRead}

class RialtoClient(http: HttpFetch
) extends CinemaScraper with DetailEnricher {


  val cinema: Cinema = Rialto
  private val RepertoireUrl = "https://www.kinorialto.poznan.pl/repertuar/"
  private val BaseUrl       = "https://www.kinorialto.poznan.pl"


  private val DateTimePat = raw"""- (\d{4}-\d{2}-\d{2}) (${ScraperParse.IsoClockText}) -""".r
  // The event page (not the repertoire listing) heads the description with a
  // pipe-delimited parameters line whose first segment is the genre list:
  //   `<p class="movie-parameters">Dramat, Komedia | 120 min</p>`
  //   `<p class="movie-parameters">Przygodowy Horror Sci-Fi | Od lat 12 | 99 min</p>`
  //   `<p class="movie-parameters">Fantastyczny / romantyczny | 90 min</p>`
  //   `<p class="movie-parameters">Animowany | 55 min</p>`
  // Genres are delimited inconsistently across pages — by comma, by a bare
  // space, or by " / " — so split the first segment on any run of those. The
  // remaining segments are an optional age rating and the runtime. Capture the
  // first segment; the `|` is required so a duration-only line ("55 min") can't
  // masquerade as a genre.
  private val GenrePat    = """movie-parameters[^>]*>\s*([^<|]+?)\s*\|""".r
  // Genre separators: a run of commas, slashes, and/or whitespace. Hyphens stay
  // intact so "Sci-Fi" survives as one genre.
  private val GenreSepPat = """[,/\s]+""".r
  // A first segment that is itself an age rating ("Od lat 12", "b.o.") or a
  // runtime means the film simply has no genre — don't emit it as one. Nor does
  // the page's own placeholder "Gatunek nieznany" ("genre unknown"), which the
  // separator split read as the two genres "Gatunek" and "Nieznany" — and which,
  // on a re-read, overruled the real genres the page had given the day before.
  private val NonGenreSegmentPat = """(?i)^(?:od lat \d+|b\.o\.|\d+\s*min|gatunek\s+nieznany)$""".r

  private val NonFilmTitlePatterns = Seq("bilet podarunkowy", "karta podarunkowa", "voucher")

  private def isNonFilmEntry(title: String): Boolean =
    NonFilmTitlePatterns.exists(title.toLowerCase(Locale.ROOT).contains)

  private case class FilmEntry(
    title:          String,
    eventUrl:       String,
    posterUrl:      Option[String],
    synopsis:       Option[String],
    director:       Seq[String],
    runtimeMinutes: Option[Int]     = None,
    releaseYear:    Option[Int]     = None,
    countries:      Seq[String]     = Seq.empty
  )

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl, BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val filmEntries = parseRepertoire(HttpRead.page(http, RepertoireUrl))

    val eventDataByUrl: Map[String, Option[EventData]] =
      ListingPages.readMore("rialto-events", filmEntries.map(_.eventUrl).distinct, identity[String], maxConcurrent = 1, timeout = 1.minute) { url =>
        Option(HttpRead.page(http, url)).map(html => EventData(parseEventPage(html), parseGenres(html)))
      }.toMap

    def eventData(entry: FilmEntry) = eventDataByUrl.get(entry.eventUrl).flatten
    SlotsToMovies.foldBlocks(filmEntries, _.title.toUpperCase(Locale.ROOT), eventData(_).toSeq.flatMap(_.showtimes),
      distinctBy = SlotsToMovies.keepEvery, order = SlotsToMovies.MovieOrder.asGrouped) { (_, group, allShowtimes) =>
        val primary = group.head
        val genres  = group.flatMap(eventData).flatMap(_.genres).distinct
        Some(CinemaMovie(
          movie     = Movie(primary.title, primary.runtimeMinutes, primary.releaseYear, countries = primary.countries, genres = genres),
          cinema    = Rialto,
          posterUrl = primary.posterUrl,
          filmUrl   = Some(primary.eventUrl),
          synopsis  = primary.synopsis,
          director  = primary.director,
          showtimes = allShowtimes
        ))
    }
  }

  override val detailGroup: String = "rialto"

  /** Deferred per-event detail fetch — the EnrichDetails task calls this with
   *  the movie's filmUrl (the event page URL). Parses genres, synopsis,
   *  director, runtime, year, and countries from the event page. None on
   *  fetch failure so the task stays stale and is retried rather than
   *  recording an empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map { html =>
      val span     = Option(Jsoup.parse(html).selectFirst("span.text"))
      val lines    = span.map(RialtoClient.textLines).getOrElse(Array.empty[String])
      val fullText = span.map(_.text()).getOrElse("")
      FilmDetail(
        synopsis       = RialtoClient.synopsisOf(lines),
        cast           = Seq.empty,
        director       = RialtoClient.directorsOf(lines),
        runtimeMinutes = RialtoClient.runtimeIn(fullText),
        releaseYear    = RialtoClient.yearIn(fullText),
        countries      = RialtoClient.countriesOf(lines),
        genres         = parseGenres(html)
      )
    }

  private def parseRepertoire(html: String): Seq[FilmEntry] =
    html.split("""<hr class="table-seperator"/>""").toSeq.flatMap { blockHtml =>
      val block = Jsoup.parseBodyFragment(blockHtml)

      Option(block.selectFirst("""a[title^="Film:"]""")).map { link =>
        val rawEventUrl = link.attr("href")
        val eventUrl    = if (rawEventUrl.startsWith("http")) rawEventUrl else BaseUrl + rawEventUrl

        val rawTitle = Option(block.selectFirst("div.title")).map(_.text().trim).getOrElse("")
        val title    = RialtoClient.normalizeTitle(rawTitle)

        // Restrict to the actual movie image container — otherwise the first block
        // (which still includes the page header) picks up the Facebook-login icon.
        val posterUrl = Option(block.selectFirst(".list-item-image img[src], .image img[src]"))
                          .map(_.attr("src"))

        val lines    = Option(block.selectFirst("span.text")).map(RialtoClient.textLines).getOrElse(Array.empty[String])
        val fullText = lines.mkString(" ")

        FilmEntry(title, eventUrl, posterUrl, RialtoClient.synopsisOf(lines), RialtoClient.directorsOf(lines),
          RialtoClient.runtimeIn(fullText), RialtoClient.yearIn(fullText), RialtoClient.countriesOf(lines))
      }.filterNot(e => isNonFilmEntry(e.title))
    }

  private case class EventData(showtimes: Seq[Showtime], genres: Seq[String])

  /** Genres from the event page's `movie-parameters` line, when present.
   *  Split on the page's inconsistent comma / space / slash delimiters and
   *  title-cased to match the genres TMDB / Filmweb and the other cinemas
   *  contribute. Empty for pages without the marker or whose first segment is
   *  an age rating / runtime / the "genre unknown" placeholder rather than a genre. */
  def parseGenres(html: String): Seq[String] =
    GenrePat.findFirstMatchIn(html).map(_.group(1).trim).filterNot(NonGenreSegmentPat.matches).map { segment =>
      GenreSepPat.split(segment).map(_.trim).filter(_.nonEmpty)
        .map(tools.TextNormalization.titleCaseIfAllLower).toSeq
    }.getOrElse(Seq.empty)

  private def parseEventPage(html: String): Seq[Showtime] = {
    val document = Jsoup.parse(html)
    document.select("a.b24-button.show[href][title]").asScala.flatMap { a =>
      val href  = a.attr("href")
      val title = a.attr("title")
      val url   = if (href.startsWith("http")) href else BaseUrl + href
      DateTimePat.findFirstMatchIn(title).flatMap { dt =>
        Try(LocalDateTime.parse(s"${dt.group(1)}T${dt.group(2)}")).toOption
          .map(dateTime => Showtime(dateTime, Some(url)))
      }
    }.toSeq.distinctBy(_.dateTime)
  }
}

object RialtoClient {

  private val RuntimePat = """(\d+)\s*min""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r
  // The metadata line on Rialto looks like "Ukraina 2026, 90 minut" or "Polska, Niemcy, 2026, 124 min" — everything
  // before the first 4-digit year is the production country (or comma-separated countries).
  private val CountriesPat = """(?s)^(.+?)\s*,?\s+(?:19|20)\d{2}\b""".r

  /** A film's `span.text` description as its `<br>`-separated lines, each as plain text: the director line
   *  (`Reż. ...`), the countries-year-runtime line, then after a blank line the synopsis. */
  private def textLines(span: Element): Array[String] =
    span.html().split("(?i)<br\\s*/?>").map(l => Jsoup.parseBodyFragment(l).body().text().trim)

  /** Every non-blank line after the first blank one. */
  private def synopsisOf(lines: Array[String]): Option[String] = {
    val emptyIndex = lines.indexWhere(_.isEmpty)
    val synopsisLines = if (emptyIndex >= 0) lines.drop(emptyIndex + 1) else Array.empty[String]
    Option(synopsisLines.filter(_.nonEmpty).mkString("\n").trim).filter(_.nonEmpty)
  }

  private def directorsOf(lines: Array[String]): Seq[String] =
    lines.find(_.startsWith("Reż. ")).toSeq.flatMap(l => ScraperParse.commaItems(l.stripPrefix("Reż. ")))

  /** The countries before the year on the first line naming a year that is not the director line; the optional
   *  trailing comma of the "..Niemcy, 2026" form dropped. */
  private def countriesOf(lines: Array[String]): Seq[String] =
    lines.find(l => YearPat.findFirstMatchIn(l).isDefined && !l.toLowerCase(Locale.ROOT).startsWith("reż"))
      .flatMap(l => CountriesPat.findFirstMatchIn(l).map(_.group(1).trim.stripSuffix(",")))
      .toSeq.flatMap(ScraperParse.commaItems)

  private def runtimeIn(text: String): Option[Int] = RuntimePat.findFirstMatchIn(text).flatMap(m => Try(m.group(1).toInt).toOption)

  private def yearIn(text: String): Option[Int] = YearPat.findFirstMatchIn(text).flatMap(m => Try(m.group(1).toInt).toOption)

  /** Rialto-specific title cleanup: drop the cycle prefix and the event-
   *  decoration suffix. Casing is NOT done here — the shared, banner-aware
   *  `TitleNormalizer.recase` cases every cinema's title once at the scrape
   *  choke point (`MovieCache.recordCinemaScrape`). Public so it can be
   *  unit-tested directly. */
  def normalizeTitle(raw: String): String = stripPreviewSuffix(stripCyclePrefix(raw))

  // Recurring-programme prefixes whose screenings are their OWN row, not folded
  // into the base film — a senior-club showing targets a distinct audience and
  // is listed separately. Matched on the registered programme set so the row
  // still enriches off the clean base title (see TitleNormalizer.ProgrammePrefix).
  private def stripCyclePrefix(title: String): String = {
    val colonIndex = title.indexOf(": ")
    if (colonIndex > 0 && colonIndex < 30) {
      val prefix = title.substring(0, colonIndex)
      if (prefix.equalsIgnoreCase("Filmowy Klub Seniora")) title
      else if (prefix != prefix.toUpperCase(Locale.ROOT)) title.substring(colonIndex + 2) else title
    } else title
  }

  // Promo decoration the page tacks onto a film's title for preview screenings
  // ("Ojczyzna - pokaz przedpremierowy"). Strip it so the entry merges with the
  // same film's regular run here and at the other cinemas. Case-insensitive —
  // raw titles arrive upper-cased — and tolerant of either dash the page uses.
  private val PreviewSuffixPat = """(?i)\s*[-–]\s*pokaz przedpremierowy\s*$""".r

  private def stripPreviewSuffix(title: String): String =
    PreviewSuffixPat.replaceFirstIn(title, "")
}
