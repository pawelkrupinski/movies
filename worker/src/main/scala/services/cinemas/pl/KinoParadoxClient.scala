package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail}

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Paradox (Kraków) — independent arthouse cinema. The venue publishes a
 * server-rendered weekly repertoire at `/repertuar/` built on WordPress.
 *
 * Each screening is a `div.list-item__content__row[data-date="DD.MM.YYYY"]`
 * element. Within it:
 *   - `div.item-time` — start time, e.g. "16:45".
 *   - `a.item-title` — the film title (as the anchor's own text node, before
 *     the nested `div.item-photo` child) and a cinema-relative film URL
 *     (`/naekranie/<slug>`).
 *   - `a.btn[href]` — a "kup bilet" booking URL on
 *     `bilety.kinoparadox.pl/index.php/repertoire.html?id=N`.
 *
 * The date is parsed from the `data-date` attribute (`DD.MM.YYYY`), so the
 * year is unambiguous and no inference is needed.
 */
class KinoParadoxClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper with DetailEnricher {

  import KinoParadoxClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html  = http.get(RepertoireUrl)
    val slots = parseDocument(html)

    val byFilmUrl = slots.groupBy(s => (s.title, s.filmUrl))
    byFilmUrl.toSeq.flatMap { case ((title, filmUrl), group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      val directors = group.map(_.directors).find(_.nonEmpty).getOrElse(Seq.empty)
      val year      = group.flatMap(_.year).headOption
      val runtime   = group.flatMap(_.runtime).headOption
      val countries = group.map(_.countries).find(_.nonEmpty).getOrElse(Seq.empty)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title, runtimeMinutes = runtime, releaseYear = year, countries = countries),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = filmUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = directors,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  // The listing already carries director + release year (TMDB-identity hints),
  // so the film resolves immediately from the listing; the detail page adds the
  // synopsis, poster, genres, and the per-film language badge (`Wersja
  // językowa`), merged in asynchronously by the EnrichDetails task.
  override val detailGroup: String = "kino-paradox"
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail — the EnrichDetails task calls this with the slot's
   *  `/naekranie/<slug>` filmUrl (UTF-8). None when nothing useful parsed, so the
   *  task stays stale and retries rather than recording an empty result fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(http.get(ref)).toOption.map(parseDetail)
      .filter(d => d.synopsis.nonEmpty || d.director.nonEmpty || d.runtimeMinutes.nonEmpty || d.genres.nonEmpty)
}

object KinoParadoxClient {

  val BaseUrl       = "https://kinoparadox.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val DateFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  private val YearPat = """\b(?:19|20)\d{2}\b""".r
  // A bare year slash-part ("2026"); used to tell the year part apart from the
  // countries part when neither carries a marker.
  private val YearOnlyPat = """^(?:19|20)\d{2}$""".r
  // A runtime slash-part ("125’" / "106'"), the minutes followed by a prime.
  private val RuntimePat  = """^(\d{1,3})\s*['’]""".r

  private[cinemas] case class Meta(
    directors: Seq[String]  = Seq.empty,
    countries: Seq[String]  = Seq.empty,
    year:      Option[Int]  = None,
    runtime:   Option[Int]  = None
  )

  private[cinemas] case class RawSlot(
    title:      String,
    filmUrl:    Option[String],
    dateTime:   LocalDateTime,
    bookingUrl: Option[String],
    directors:  Seq[String],
    countries:  Seq[String],
    year:       Option[Int],
    runtime:    Option[Int]
  )

  /** The `div.item-director` line is slash-delimited:
   *  `reż. <dirs> / <countries> / <year> / <runtime>’`. Each part is optional —
   *  some films carry no `reż.` prefix and some no year — so pick the director
   *  part by its `reż` marker, the year by the only bare 4-digit part, the
   *  runtime by the prime-suffixed part, and read whatever non-marker part is
   *  left as the (comma-separated) production countries. */
  private[cinemas] def parseMeta(itemDirector: String): Meta = {
    val parts = itemDirector.split("/").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    val directors = parts.find(_.toLowerCase.startsWith("reż"))
      .map(_.replaceFirst("(?i)^reż\\.?\\s*", "").split(",").map(_.trim).filter(_.nonEmpty).toSeq)
      .getOrElse(Seq.empty)
    val year    = parts.iterator.flatMap(p => YearPat.findFirstIn(p)).map(_.toInt).nextOption()
    val runtime = parts.iterator.flatMap(p => RuntimePat.findFirstMatchIn(p)).map(_.group(1).toInt).nextOption()
    // Countries are the leftover part: not the director (reż) part, not the bare
    // year, not the runtime prime. Comma-split, verbatim — canonicalised later
    // in recordCinemaScrape.
    val countries = parts.iterator
      .filterNot(_.toLowerCase.startsWith("reż"))
      .filterNot(p => YearOnlyPat.matches(p))
      .filterNot(p => RuntimePat.matches(p))
      .flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSeq
    Meta(directors, countries, year, runtime)
  }

  private[cinemas] def parseDocument(html: String): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    document.select("div.list-item__content__row[data-date]").asScala.toSeq.flatMap { row =>
      val dateAttr = row.attr("data-date").trim
      val date     = Try(LocalDate.parse(dateAttr, DateFmt)).toOption
      val time     = Option(row.selectFirst("div.item-time")).flatMap(e => ScraperParse.parseHHmm(e.text.trim))

      // The title anchor contains both the text title and a nested img div;
      // ownText() returns only the direct text node, which is the film title.
      val titleAnchor = Option(row.selectFirst("a.item-title"))
      val title       = titleAnchor.map(_.ownText().trim).filter(_.nonEmpty)
      val filmHref    = titleAnchor.map(_.attr("href").trim).filter(_.nonEmpty)
      val filmUrl     = filmHref.map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

      val bookingUrl = Option(row.selectFirst("a.btn[href]"))
        .map(_.attr("href").trim).filter(_.nonEmpty)
        .map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

      val meta = Option(row.selectFirst("div.item-director"))
        .map(e => parseMeta(e.text.trim)).getOrElse(Meta())

      for {
        d <- date
        t <- time
        n <- title
      } yield RawSlot(n, filmUrl, LocalDateTime.of(d, t), bookingUrl,
                      meta.directors, meta.countries, meta.year, meta.runtime)
    }
  }

  /** Parse a `/naekranie/<slug>` detail page into a [[FilmDetail]]. The film
   *  facts sit in a repeated `div.single-os-hero__item` block, each a
   *  `.single-os-hero__label` + `.single-os-hero__value`; the synopsis is
   *  `div.single-os-hero__desc-col`; the poster is the `div.single-os-hero__img-col`
   *  image. The `Wersja językowa` row carries the film's language, surfaced as a
   *  format badge on its showings. */
  private[cinemas] def parseDetail(html: String): FilmDetail = {
    val doc = Jsoup.parse(html)
    // The layout duplicates the item block (desktop + mobile) with identical
    // values, so a simple label -> value map (last wins == first) is fine.
    val fields = doc.select("div.single-os-hero__item").asScala.iterator.flatMap { item =>
      val label = Option(item.selectFirst(".single-os-hero__label")).map(_.text.trim.toLowerCase(Locale.ROOT))
      val value = Option(item.selectFirst(".single-os-hero__value")).map(_.text.trim).filter(_.nonEmpty)
      for { l <- label; v <- value } yield l -> v
    }.toMap
    def people(label: String): Seq[String] =
      fields.get(label).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val (countries, year) = fields.get("produkcja").map(ScraperParse.productionMeta).getOrElse((Nil, None))
    val runtime  = fields.get("czas trwania").flatMap(v => """(\d+)""".r.findFirstIn(v)).map(_.toInt).filter(_ > 0)
    val format   = fields.get("wersja językowa").map(ScraperParse.formatTokensIn).getOrElse(Nil)
    val synopsis = Option(doc.selectFirst("div.single-os-hero__desc-col"))
      .map(el => ScraperParse.cleanSynopsis(el)).filter(_.length > 20)
    val poster   = Option(doc.selectFirst("div.single-os-hero__img-col img[src]"))
      .map(_.attr("src").trim).filter(_.nonEmpty)
    FilmDetail(
      synopsis       = synopsis,
      director       = people("reżyseria"),
      runtimeMinutes = runtime,
      releaseYear    = year,
      countries      = countries,
      genres         = people("gatunek"),
      posterUrl      = poster,
      format         = format
    )
  }
}
