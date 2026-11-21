package services.cinemas.pl

import java.time.LocalDateTime
import java.util.Locale

import scala.jdk.CollectionConverters.*

import models.*
import org.jsoup.Jsoup
import services.cinemas.common.SynopsisProse
import services.cinemas.common.{CreditedScreening, ListingFacts, ListingPageScraper, ScraperParse, SlotsToMovies}
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome, FilmDetail}
import services.titles.FormatTags
import tools.HttpFetch

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
class KinoParadoxClient(http: HttpFetch, override val cinema: Cinema)
    extends ListingPageScraper(http, KinoParadoxClient.RepertoireUrl, KinoParadoxClient.BaseUrl) with DetailEnricher {

  import KinoParadoxClient._

  protected def parsePage(html: String): Seq[CinemaMovie] = SlotsToMovies.byTitleAndFilmPage(parseDocument(html), cinema)

  // The listing already carries director + release year (TMDB-identity hints),
  // so the film resolves immediately from the listing; the detail page adds the
  // synopsis, poster, genres, and the per-film language badge (`Wersja
  // językowa`), merged in asynchronously by the EnrichDetails task.
  override val detailGroup: String = "kino-paradox"
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail — the EnrichDetails task calls this with the slot's
   *  `/naekranie/<slug>` filmUrl (UTF-8). None when nothing useful parsed, so the
   *  task stays stale and retries rather than recording an empty result fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]].
   *
   *  A page that LOADED is a detail even when it carries no fields. Filtering the
   *  empty ones out folded them into `Failed`, which is never stamped — so
   *  `DueWindow.isDue` stayed true and `DetailReaper` re-enqueued the film every
   *  tick, forever (Kino Bulgarska: 1,438 failures to 56 successes in 24h on one
   *  film). An empty `FilmDetail` merges as a no-op, so keeping it costs nothing
   *  and stamps the film back onto the normal refresh window. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(parseDetail)
}

object KinoParadoxClient {

  val BaseUrl       = "https://kinoparadox.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val YearPat = """\b(?:19|20)\d{2}\b""".r
  // A bare year slash-part ("2026"); used to tell the year part apart from the
  // countries part when neither carries a marker.
  private val YearOnlyPat = """^(?:19|20)\d{2}$""".r
  // A runtime slash-part ("125’" / "106'"), the minutes followed by a prime.
  private val RuntimePat  = """^(\d{1,3})\s*['’]""".r

  /** The `div.item-director` line is slash-delimited:
   *  `reż. <dirs> / <countries> / <year> / <runtime>’`. Each part is optional —
   *  some films carry no `reż.` prefix and some no year — so pick the director
   *  part by its `reż` marker, the year by the only bare 4-digit part, the
   *  runtime by the prime-suffixed part, and read whatever non-marker part is
   *  left as the (comma-separated) production countries. */
  private[cinemas] def parseMeta(itemDirector: String): ListingFacts = {
    val parts = itemDirector.split("/").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    val directors = parts.find(_.toLowerCase(Locale.ROOT).startsWith("reż"))
      .map(d => ScraperParse.commaItems(d.replaceFirst("(?i)^reż\\.?\\s*", "")))
      .getOrElse(Seq.empty)
    val year    = parts.iterator.flatMap(p => YearPat.findFirstIn(p)).map(_.toInt).nextOption()
    val runtime = parts.iterator.flatMap(p => RuntimePat.findFirstMatchIn(p)).map(_.group(1).toInt).nextOption()
    // Countries are the leftover part: not the director (reż) part, not the bare
    // year, not the runtime prime. Comma-split, verbatim — canonicalised later
    // in recordCinemaScrape.
    val countries = parts.iterator
      .filterNot(_.toLowerCase(Locale.ROOT).startsWith("reż"))
      .filterNot(p => YearOnlyPat.matches(p))
      .filterNot(p => RuntimePat.matches(p))
      .flatMap(ScraperParse.commaItems).toSeq
    ListingFacts(directors, countries, year, runtime)
  }

  private[cinemas] def parseDocument(html: String): Seq[CreditedScreening] = {
    val document = Jsoup.parse(html)
    document.select("div.list-item__content__row[data-date]").asScala.toSeq.flatMap { row =>
      val dateAttr = row.attr("data-date").trim
      val date     = ScraperParse.parseDate(dateAttr)
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
        .map(e => parseMeta(e.text.trim)).getOrElse(ListingFacts())

      for {
        d <- date
        t <- time
        n <- title
      } yield CreditedScreening(n, filmUrl, LocalDateTime.of(d, t), bookingUrl, meta)
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
      fields.get(label).toSeq.flatMap(ScraperParse.commaItems)
    val (countries, year) = fields.get("produkcja").map(ScraperParse.productionMeta).getOrElse((Nil, None))
    val runtime  = fields.get("czas trwania").flatMap(v => """(\d+)""".r.findFirstIn(v)).map(_.toInt).filter(_ > 0)
    val format   = fields.get("wersja językowa").map(FormatTags.formatTokensIn).getOrElse(Nil)
    val synopsis = Option(doc.selectFirst("div.single-os-hero__desc-col"))
      .map(el => SynopsisProse.cleanSynopsis(el)).filter(_.length > 20)
    val poster   = Option(doc.selectFirst("div.single-os-hero__img-col img[src]"))
      .flatMap(ScraperParse.largestImageSource)
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
