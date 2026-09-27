package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{AgeRating, CinemaScraper, ScraperParse}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Gwiazda (Kętrzyn). The venue's own Statamic site replaced Filmweb as the
 * source: Filmweb carried a thin, often empty slice of its programme, while the
 * site lists every screening that is on sale.
 *
 * Two page shapes, both server-rendered (the Alpine.js on the listing only
 * filters cards that are already in the HTML):
 *
 *   - `/repertuar` — one `div.movie-item` card per film. The card is read for
 *     what the film page lacks: the poster (`a[href^=/filmy/] img`, a Glide
 *     `/img/asset/…` URL) and the age badge (`13+`, `B.O.`).
 *   - `/filmy/<slug>` — the film page, which holds the showtimes, so it is
 *     fetched inline for every card (showtimes are identity-bearing and cannot be
 *     deferred to a detail enricher). It carries:
 *       - `h1`                                  → title (ALL CAPS, verbatim; the
 *         scrape-time recase handles case; the `xtra-kino-konesera-suffix` and
 *         `xtra-senior-w-kinie-suffix` search strips take the " - KINO
 *         KONESERA" / "SENIOR W KINIE" programme suffixes off the enrichment
 *         query — the screening keeps its own card)
 *       - the meta spans under `h1`             → `NNN min`, a comma-separated
 *         genre list, and the language version ("dubbing PL", "napisy PL",
 *         "Wersja oryginalna")
 *       - `dl` `dt`/`dd` pairs                  → "Rezyseria" (director) and
 *         "Obsada" (cast, each name followed by a „film credits” parenthetical)
 *       - the "Zwiastun" link                   → YouTube trailer
 *       - `section[aria-labelledby=description-heading]` → synopsis
 *       - `section#seanse tbody tr`             → one screening per row: a
 *         `<time datetime="YYYY-MM-DD">`, a `<time datetime="HH:MM">` and the
 *         eurobilet booking deep link
 *
 * The site exposes no release year, country or original title.
 */
class KinoGwiazdaKetrzynClient(http: HttpFetch, override val cinema: Cinema = KinoGwiazdaKetrzyn) extends CinemaScraper {

  import KinoGwiazdaKetrzynClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  // A failed listing or film-page fetch propagates: swallowed, it would read as a
  // venue with no screenings — a white scrape instead of a red one.
  def fetch(): Seq[CinemaMovie] =
    cards(http.get(RepertoireUrl)).flatMap(card => film(card, http.get(card.filmUrl), cinema))
}

object KinoGwiazdaKetrzynClient {

  val BaseUrl       = "https://kino.ketrzyn.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar"

  /** What the listing card knows about a film that its own page doesn't show. */
  private[pl] case class Card(filmUrl: String, posterUrl: Option[String], ageRating: Option[String])

  private val FilmHref  = "a[href^=/filmy/]"
  private val AgeBadge  = """^(?:\d{1,2}\+|B\.?O\.?)$""".r
  private val Runtime   = """^(\d{2,3})\s*min$""".r
  /** A cast member's „film credits” parenthetical ("Marcin Dorociński („Chłopi”, „Psy”)"). */
  private val Credits   = """\s*\([^)]*\)""".r

  /** One card per film page, in listing order, deduplicated by URL (a card links
   *  its film twice — poster and title — and "#seanse" anchors elsewhere). */
  private[pl] def cards(html: String): Seq[Card] = {
    val document = Jsoup.parse(html, BaseUrl)
    document.select("div.movie-item").asScala.toSeq.flatMap { item =>
      Option(item.selectFirst(FilmHref)).map { link =>
        Card(
          filmUrl   = link.attr("abs:href").takeWhile(_ != '#'),
          posterUrl = Option(item.selectFirst(s"$FilmHref img")).map(_.attr("abs:src")).filter(_.nonEmpty),
          ageRating = item.select("span").asScala.map(_.text.trim).collectFirst { case t @ AgeBadge() => t }
            .flatMap(badge => AgeRating.normalizeDroppingNoRestriction(Some(badge)))
        )
      }
    }.distinctBy(_.filmUrl)
  }

  /** The film on one `/filmy/<slug>` page, or `None` when the page names no
   *  title or lists no screening. */
  private[pl] def film(card: Card, html: String, cinema: Cinema): Option[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val meta     = Option(document.selectFirst("h1")).map(_.parent).toSeq
      .flatMap(_.select("h1 + div span > span").asScala).map(_.text.trim).filter(_.nonEmpty)
    val format   = ScraperParse.formatTokensIn(meta.mkString(" ").toLowerCase)
    for {
      title <- Option(document.selectFirst("h1")).map(_.text.trim).filter(_.nonEmpty)
      showtimes = screenings(document, format) if showtimes.nonEmpty
    } yield CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = meta.collectFirst { case Runtime(minutes) => minutes.toInt },
        genres         = meta.find(isGenreList).toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
      ),
      cinema     = cinema,
      posterUrl  = card.posterUrl,
      filmUrl    = Some(card.filmUrl),
      synopsis   = Option(document.selectFirst("section[aria-labelledby=description-heading] h2 + div"))
        .map(ScraperParse.cleanSynopsis(_)).filter(_.nonEmpty),
      cast       = definition(document, "Obsada").toSeq
        .flatMap(dd => Credits.replaceAllIn(dd, "").split(",")).map(_.trim).filter(_.nonEmpty),
      director   = definition(document, "Rezyseria").toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty),
      showtimes  = showtimes,
      trailerUrl = document.select("a[href]").asScala.find(_.text.trim == "Zwiastun")
        .flatMap(a => ScraperParse.canonicalTrailer(a.attr("href"))),
      ageRating  = card.ageRating
    )
  }

  /** A meta span that is neither the runtime nor the language version is the
   *  genre list. */
  private def isGenreList(span: String): Boolean =
    Runtime.findFirstIn(span).isEmpty && ScraperParse.formatTokensIn(span.toLowerCase).isEmpty &&
      !span.toLowerCase.startsWith("wersja")

  /** The `dd` text of the `dt` labelled `label` ("Rezyseria", "Obsada"). */
  private def definition(document: Document, label: String): Option[String] =
    document.select("dl dt").asScala.find(_.text.trim.equalsIgnoreCase(label))
      .flatMap(dt => Option(dt.nextElementSibling)).map(_.text.trim).filter(_.nonEmpty)

  private def screenings(document: Document, format: List[String]): Seq[Showtime] =
    document.select("section#seanse tbody tr").asScala.toSeq.flatMap(screening(_, format))
      .distinctBy(_.dateTime).sortBy(_.dateTime)

  private def screening(row: Element, format: List[String]): Option[Showtime] = {
    val times = row.select("time[datetime]").asScala.toSeq.map(_.attr("datetime").trim)
    for {
      date <- times.headOption.flatMap(d => Try(LocalDate.parse(d)).toOption)
      time <- times.lift(1).flatMap(ScraperParse.parseHHmm)
    } yield Showtime(
      dateTime   = LocalDateTime.of(date, time),
      bookingUrl = Option(row.selectFirst("a[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
      format     = format
    )
  }
}
