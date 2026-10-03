package services.cinemas.pl

import java.util.Locale

import services.cinemas.common.ScraperParse
import tools.{HttpFetch, HttpRead}
import models._
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, ListingPages, ScrapeHorizon, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * MCSW Elektrownia cinema (Radom) — the film screen of the Mazowieckie
 * Centrum Sztuki Współczesnej "Elektrownia".  The site uses an ASP.NET MSI
 * ticketing system that renders its schedule as a static HTML page per day at:
 *
 *   https://kino.mcswelektrownia.pl/MSI/mvc/pl?sort=Date&date=YYYY-MM-DD&datestart=0
 *
 * The day pages are walked forward for as long as the programme lasts and
 * merged (see `fetch`).  Each day page lists all currently-running films as
 * `div.js-event-details-filter.movies-movie__single` blocks.  Within each block:
 *
 *   - `.movies-movie__single__title` (an `h2` or, since 2026-06-16, an `h3`) —
 *     a composite string, read by [[McswElektrowniaCinemaClient.parseTitle]]:
 *     "CLEAN TITLE, Country, genres, rating   KS N 2026D2D4410" — and since
 *     autumn 2026 also "OBCY-kryminał/Francja/15lat N, KS 2025T2D10184", the
 *     genre / country / age glued to the title by a dash. The countries, genres
 *     and age become the film's; the catalogue code's year is its production
 *     year and its letter the version (D dubbing, T subtitles, O the Polish
 *     original), beside its "2D".
 *   - `li[event-filter]` / `a[href^="/MSI/Default.aspx?event_id="]` — each
 *     list item is ONE screening occurrence; the anchor text is its time
 *     ("HH:MM") and the href is the per-occurrence booking URL.
 *     The list is rendered twice (desktop + mobile), so deduplicate by
 *     (time, event_id) before emitting showtimes.
 *   - `img[src^="/MSI/ImageData.ashx"]` — poster thumbnail.
 *
 * Films that appear on multiple days are aggregated by their normalised title
 * (trimmed, lowercased, before the first comma) so that the same film shown
 * on Tuesday and Thursday appears as one `CinemaMovie` with multiple
 * showtimes.
 *
 * This is an MSI portal, so [[MsiClient]]'s month route serves it too — but not
 * as well: measured 2026-08-05, `?sort=Name&date=2026-08` listed only 05–11
 * while the day route had four films on both the 13th and the 16th.  The
 * per-day walk stays.
 */
class McswElektrowniaCinemaClient(
  http:              HttpFetch,
  override val cinema: Cinema = McswElektrowniaCinema,
  today:             => LocalDate
) extends CinemaScraper {

  import McswElektrowniaCinemaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    // Follow the programme rather than assume a week of it: on 2026-08-05 the
    // day route had films on the 13th and the 16th, both past the today+6 window
    // this used to ask for, so a fortnight of the schedule was invisible.
    // See [[ScrapeHorizon.liveDays]] — same walk as the other per-day clients.
    val byDate = scala.collection.mutable.LinkedHashMap.empty[LocalDate, Try[Seq[RawSlot]]]
    ScrapeHorizon.liveDays(today) { date =>
      byDate.getOrElseUpdate(date, Try(HttpRead.page(http, dayUrl(date))).map(parseDayPage(_, date))).toOption.exists(_.nonEmpty)
    }
    ListingPages.requireAnyReached(byDate.values)
    val slots: Seq[RawSlot] = byDate.values.toSeq.flatMap(_.toOption).flatten

    // Group by normalised title and merge showtimes across days.
    SlotsToMovies.fold(slots, _.normTitle, s => Showtime(s.dateTime, Some(BookingBase + s.eventPath), format = s.parts.format)) {
      (_, group, showtimes) =>
        val head = group.head
        CinemaMovie(
          movie     = Movie(head.displayTitle, releaseYear = head.parts.year, countries = head.parts.countries,
            genres = head.parts.genres),
          cinema    = cinema,
          posterUrl = head.posterUrl,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes,
          ageRating = head.parts.ageRating
        )
    }
  }
}

object McswElektrowniaCinemaClient {

  val BaseUrl    = "https://kino.mcswelektrownia.pl"
  val BookingBase = BaseUrl

  private val DateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def dayUrl(date: LocalDate): String =
    s"$BaseUrl/MSI/mvc/pl?sort=Date&date=${date.format(DateFmt)}&datestart=0"

  /** A raw (date + time) screening extracted from one day page. */
  private[cinemas] case class RawSlot(
    displayTitle: String,  // cleaned title for user display
    parts:        TitleParts,
    normTitle:    String,  // lowercased key for cross-day grouping
    posterUrl:    Option[String],
    dateTime:     LocalDateTime,
    eventPath:    String   // e.g. "/MSI/Default.aspx?event_id=14125&typetran=0&..."
  )

  /** What one composite title says: the film's title, and what its tail and catalogue code state. */
  private[cinemas] final case class TitleParts(title: String, year: Option[Int] = None, countries: Seq[String] = Nil,
                                               genres: Seq[String] = Nil, ageRating: Option[String] = None, format: List[String] = Nil)

  // "2026D2D4410" / "2025T2D10184" / "19532DT0004": production year, version letter, dimension, number.
  private val CatalogueCode = """\s*\b(\d{4})(?:([A-Z])([23]D)|([23]D)([A-Z]))\d+\s*$""".r
  // The venue's markers before the code ("KS N"), and a note in brackets ("(kopia cyfrowa 4K, …)").
  private val Markers       = """(?:\s*,?\s*\b(?:KS|N)\b)+\s*$""".r
  private val Note          = """\s*\([^)]*\)""".r
  private val Age           = """(?i)^(?:od\s*)?(\d{1,2})\s*lat(?:\s+(?:KS|N))*$""".r
  private val VersionOf     = Map('D' -> "DUB", 'T' -> "NAP")
  private val VersionWords  = Map("dubbing" -> "DUB", "napisy" -> "NAP", "lektor" -> "LEK")
  /** The genres this venue writes (lower-cased), "fatasy" its own spelling. */
  private val Genres = Set("animowany", "animacja", "dramat", "obyczajowy", "komedia", "romans", "thriller", "horror",
    "kryminał", "familijny", "fantasy", "fatasy", "sci-fi", "przygodowy", "dokumentalny", "biograficzny", "historyczny",
    "kostiumowy", "muzyczny", "musical", "wojenny", "western", "akcja", "sensacyjny", "psychologiczny", "polityczny",
    "katastroficzny", "baśń", "romantyczna", "romantyczny", "kryminalny", "mystery")

  private enum Piece { case Country(name: String); case Genre(name: String); case AgeOf(years: String); case Version(token: String) }

  /** One tail segment as what it states, or `None` when it states nothing this venue writes. */
  private def piece(raw: String): Option[Seq[Piece]] = {
    val t = raw.trim
    val country = services.cinemas.CountryNames.canonical(t)
    if (t.isEmpty) Some(Nil)
    else if (services.identity.IdentityMeasures.countryCode(country).isDefined) Some(Seq(Piece.Country(country)))
    else Age.findFirstMatchIn(t).map(m => Seq(Piece.AgeOf(m.group(1))))
      .orElse(VersionWords.get(t.toLowerCase(Locale.ROOT)).map(v => Seq(Piece.Version(v))))
      .orElse(Option.when(t.toLowerCase(Locale.ROOT).split("\\s+").forall(Genres))(t.toLowerCase(Locale.ROOT).split("\\s+").toSeq.map(Piece.Genre(_))))
  }

  /** Read a composite title (see the class doc). A dash tail is the title's only when one of its
   *  segments states nothing this venue writes ("SZTUKA NA EKRANIE-HAUSER" keeps its performer). */
  private[cinemas] def parseTitle(raw: String): TitleParts = {
    val code    = CatalogueCode.findFirstMatchIn(raw.trim)
    val body    = Note.replaceAllIn(code.fold(raw.trim)(m => raw.trim.take(m.start)), "")
    val unmarked = Markers.replaceFirstIn(body, "").trim
    val segments = unmarked.split(",").map(_.trim).toSeq
    val (head, rest) = (segments.headOption.getOrElse(""), segments.drop(1))
    val dashed = """^(.+?\S)\s*[-–]\s*(\S.*)$""".r.findFirstMatchIn(head).flatMap { m =>
      val tail = m.group(2).split("/").toSeq.map(piece)
      Option.when(tail.forall(_.isDefined))(m.group(1).trim -> tail.flatten.flatten)
    }
    val (title, fromDash) = dashed.getOrElse(head.trim -> Nil)
    val pieces = fromDash ++ rest.flatMap(_.split("/")).flatMap(piece(_).getOrElse(Nil))
    val version = code.flatMap(m => Option(m.group(2)).orElse(Option(m.group(5)))).flatMap(l => VersionOf.get(l.head))
    val dim     = code.flatMap(m => Option(m.group(3)).orElse(Option(m.group(4))))
    TitleParts(
      title     = title,
      year      = code.map(_.group(1).toInt).filter(y => y >= 1888 && y <= 2100),
      countries = pieces.collect { case Piece.Country(c) => c }.distinct,
      genres    = pieces.collect { case Piece.Genre(g) => g }.distinct,
      ageRating = pieces.collectFirst { case Piece.AgeOf(a) => a },
      format    = (dim.toList ++ version.toList ++ pieces.collect { case Piece.Version(v) => v }).distinct)
  }

  private[cinemas] def parseDayPage(html: String, date: LocalDate): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    document.select("div.js-event-details-filter.movies-movie__single").asScala.toSeq.flatMap { block =>
      // The title sits on `.movies-movie__single__title`; the site has rendered
      // this as both `h2` (2026-06 capture) and `h3` (2026-06-16 onward), so
      // match on the class alone rather than pinning the heading level.
      val rawTitle = Option(block.selectFirst(".movies-movie__single__title"))
        .map(_.text.trim).getOrElse("")
      if (rawTitle.isEmpty) Seq.empty
      else {
        val parts        = parseTitle(rawTitle)
        val displayTitle = parts.title
        val normTitle    = displayTitle.trim.toLowerCase(Locale.ROOT)

        val posterUrl = Option(block.selectFirst("img[src]"))
          .map(_.attr("src").trim)
          .filter(_.startsWith("/MSI/ImageData.ashx"))
          .map(BaseUrl + _)

        // Each `li[event-filter]` is one screening slot; the anchor carries the
        // time as its visible text and the booking path as href.  The list is
        // rendered twice (desktop + mobile), so deduplicate by (eventPath, timeStr).
        val seenKeys = collection.mutable.Set.empty[(String, String)]
        block.select("li[event-filter] a[href^=\"/MSI/Default.aspx?event_id=\"]").asScala.toSeq.flatMap { anchor =>
          val timeStr  = anchor.text.trim
          val timeOpt  = ScraperParse.parseHHmm(timeStr)
          val path     = anchor.attr("href").trim
          val key      = (path, timeStr)
          if (timeOpt.isEmpty || !seenKeys.add(key)) Nil
          else {
            val dateTime = LocalDateTime.of(date, timeOpt.get)
            Seq(RawSlot(displayTitle, parts, normTitle, posterUrl, dateTime, path))
          }
        }
      }
    }
  }
}
