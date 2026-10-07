package services.cinemas.pl

import java.util.Locale

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.CountryNames
import services.cinemas.common.{AgeRating, CinemaScraper, ListingPages, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Kino Iskra (Augustów). The venue's own site replaced Filmweb as the source:
 * Filmweb carried a thin slice of the programme, while the site lists every
 * screening months ahead, and its per-film record names year, countries and
 * director — the signals TMDB resolution needs.
 *
 * Two endpoints of the site's hand-rolled PHP CMS:
 *
 *   - `/repertuar/` — the "full repertoire" page. Its `div.list-by-movie`
 *     section holds one block per film VERSION (a dubbed and a subtitled
 *     Avengers are two blocks with two movie ids), each with:
 *       - `h2.movie-title[data-movie]` → the title as own text, the version
 *         (`2D`, `dubbing`, `napisy`) in `<sup>` children, and the movie id
 *       - `button.event-button`        → one per screening: a yearless
 *         "27 września" `<span>` then an `HH:MM` `<span>`; the year comes
 *         from the injected `today` ([[ScraperParse.upcomingDate]])
 *     (The same screenings are repeated in a hidden `div.list-by-day`, which is
 *     ignored.) Live events — stand-up, "Spektakl …" — share the page; they are
 *     dropped by title before their record is fetched.
 *   - `/ajax/user/get_movie.php?movie=<id>` — the per-film record, fetched once
 *     per movie id:
 *       - the first `p.text-dark` line: "Genres / …, Countries, Year"
 *         ("Przygodowy / Akcja , Wielka Brytania, USA, 2026"); some records
 *         put a studio where the countries go ("Marvel / Disney"), so only
 *         names [[CountryNames]] recognises are kept
 *       - `<b>Czas trwania:</b>`, `<b>Reżyseria:</b>`, `<b>Obsada:</b>`,
 *         `<b>Od lat:</b>` labelled lines
 *       - `img.poster-to-zoom` and the synopsis paragraph
 *
 * Booking runs through the venue's MSI portal, which the own site only links
 * per screening from a click-loaded tooltip. Rather than one more request per
 * screening, each showtime links the portal's page for that day, which lists
 * that day's screenings with their buy buttons.
 */
class KinoIskraClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoIskra,
  today: => LocalDate
) extends CinemaScraper {

  import KinoIskraClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  // A failed listing propagates: swallowed, it would read as a venue with no
  // screenings — a white scrape instead of a red one. A film's record only adds
  // metadata to what the listing already shows, so one that fails to load leaves
  // that film bare rather than failing the venue. Records load side by side.
  // Live events are dropped after the merge, so a film whose title is event
  // vocabulary keeps itself by its record's director and year — and one dropped
  // for want of a record that failed leaves the listing incomplete, so the cache
  // does not prune it until a scrape that reads its record.
  def fetch(): Seq[CinemaMovie] = {
    val slots   = listing(HttpRead.page(http, RepertoireUrl), today)
    val reads   = ListingPages.readEnrichment("kino-iskra", slots.map(_.movieId), movieUrl, maxConcurrent = 2)(url =>
      record(HttpRead.page(http, url))).toMap
    val records = reads.flatMap { case (id, attempt) => attempt.toOption.map(id -> _) }
    val films = SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val record = group.map(_.movieId).distinct.flatMap(records.get).foldLeft(Record())((known, next) => known.orElse(next))
      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = record.runtime,
          releaseYear    = record.year,
          countries      = record.countries,
          genres         = record.genres
        ),
        cinema    = cinema,
        posterUrl = record.poster,
        filmUrl   = Some(s"$BaseUrl/zapowiedzi/#${group.head.movieId}"),
        synopsis  = record.synopsis,
        cast      = record.cast,
        director  = record.director,
        showtimes = showtimes,
        ageRating = record.ageRating
      )
    }
    val (events, kept) = films.partition(NonMovieEventClassifier.isLiveEvent)
    val dropped = events.map(_.movie.title).toSet
    ListingPages.reportFailed(slots.filter(s => dropped(s.title)).map(_.movieId).distinct.flatMap(reads.get))
    kept
  }
}

object KinoIskraClient {

  val BaseUrl       = "https://kino-iskra.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"
  val TicketingUrl  = "https://bilety.kino-iskra.pl/MSI/mvc/pl"

  def movieUrl(movieId: String): String = s"$BaseUrl/ajax/user/get_movie.php?movie=$movieId"

  /** The MSI portal's page for one day's screenings. */
  def ticketingDayUrl(date: LocalDate): String = s"$TicketingUrl?sort=Name&date=$date"

  private[pl] case class Slot(movieId: String, title: String, showtime: Showtime)

  /** What a film's `get_movie.php` record states. */
  private[pl] case class Record(
    genres:    Seq[String]    = Seq.empty,
    countries: Seq[String]    = Seq.empty,
    year:      Option[Int]    = None,
    runtime:   Option[Int]    = None,
    director:  Seq[String]    = Seq.empty,
    cast:      Seq[String]    = Seq.empty,
    ageRating: Option[String] = None,
    poster:    Option[String] = None,
    synopsis:  Option[String] = None
  ) {
    /** This record, with each field it lacks taken from `other` — two versions
     *  of one film (dubbed / subtitled) are two records of the same film. */
    def orElse(other: Record): Record = Record(
      genres    = if (genres.nonEmpty) genres else other.genres,
      countries = if (countries.nonEmpty) countries else other.countries,
      year      = year.orElse(other.year),
      runtime   = runtime.orElse(other.runtime),
      director  = if (director.nonEmpty) director else other.director,
      cast      = if (cast.nonEmpty) cast else other.cast,
      ageRating = ageRating.orElse(other.ageRating),
      poster    = poster.orElse(other.poster),
      synopsis  = synopsis.orElse(other.synopsis)
    )
  }

  private val Minutes = """(\d{2,3})\s*min""".r

  /** Every screening on the `/repertuar/` page; live events are dropped once
   *  each listing carries its record (see `fetch`). */
  private[pl] def listing(html: String, today: LocalDate): Seq[Slot] =
    Jsoup.parse(html, BaseUrl).select("div.list-by-movie h2.movie-title[data-movie]").asScala.toSeq.flatMap { heading =>
      val title  = heading.ownText.trim
      val format = ScraperParse.formatTokensIn(heading.select("sup").asScala.map(_.text).mkString(" ").toLowerCase(Locale.ROOT))
      val block  = heading.closest("div.bg-gray-light")
      if (title.isEmpty || block == null) Seq.empty
      else block.select("button.event-button").asScala.toSeq.flatMap { button =>
        val spans = button.select("span").asScala.toSeq.map(_.text.trim)
        for {
          dayMonth <- spans.headOption.flatMap(ScraperParse.parseDayMonth)
          date     <- ScraperParse.upcomingDate(dayMonth, today)
          time     <- spans.lift(1).flatMap(ScraperParse.parseHHmm)
        } yield Slot(heading.attr("data-movie"), title,
          Showtime(LocalDateTime.of(date, time), Some(ticketingDayUrl(date)), format = format))
      }
    }

  private[pl] def record(html: String): Record = {
    val document = Jsoup.parse(html, BaseUrl)
    val lines    = document.select("p.text-dark").asScala.toSeq
    val production = lines.find(_.selectFirst("b") == null).map(_.text.trim).getOrElse("")
    val (genrePart, rest) = production.split(",", 2) match {
      case Array(genres, tail) => (genres, tail)
      case Array(genres)       => (genres, "")
    }
    val (countries, year) = ScraperParse.productionMeta(rest)
    Record(
      genres    = genrePart.split("/").toSeq.map(_.trim).filter(_.nonEmpty),
      countries = countries.filter(CountryNames.isPolish),
      year      = year,
      runtime   = labelled(lines, "Czas trwania").flatMap(Minutes.findFirstMatchIn(_)).map(_.group(1).toInt),
      director  = labelled(lines, "Reżyseria").toSeq.flatMap(names),
      cast      = labelled(lines, "Obsada").toSeq.flatMap(names),
      ageRating = labelled(lines, "Od lat").map(_.trim).filter(_.nonEmpty).flatMap(age => AgeRating.normalize(s"$age+")),
      poster    = Option(document.selectFirst("img.poster-to-zoom")).map(_.attr("abs:src")).filter(_.nonEmpty),
      synopsis  = synopsisOf(document)
    )
  }

  /** The text after a `<b>Label:</b>` on a record line. */
  private def labelled(lines: Seq[Element], label: String): Option[String] =
    lines.find(p => Option(p.selectFirst("b")).exists(_.text.replace(' ', ' ').trim.startsWith(label)))
      .map(_.ownText.replace(' ', ' ').trim).filter(_.nonEmpty)

  private def names(list: String): Seq[String] = list.split(",").toSeq.map(_.trim).filter(_.nonEmpty)

  /** The synopsis: the record's small paragraph that carries no `<b>` label. */
  private def synopsisOf(document: Document): Option[String] =
    document.select("p.small.mb-3").asScala.find(_.selectFirst("b") == null)
      .map(ScraperParse.cleanSynopsis(_)).filter(_.nonEmpty)
}
