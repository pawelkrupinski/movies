package services.cinemas.pl

import tools.{HttpFetch, ParallelDetailFetch}
import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.CinemaScraper

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Teatr Ziemi Rybnickiej runs a cinema programme on its own site rather than
 * through a ticketing platform we already scrape. The film listing lives at
 * `/wydarzenia?type[]=film&month=default` ("najbliższe wydarzenia" — the
 * upcoming films); each tile links to a `/wydarzenia/<slug>.html` detail page
 * carrying the dates, booking link, synopsis and (for most films) a metadata
 * block with runtime / director / country / genre / cast.
 *
 * The detail pages come in two metadata shapes:
 *   - the in-house "kino nie tylko dla seniora" screenings use labelled lines
 *     "Czas trwania: … / Reż: … / Prod. … / Gatunek: … / Obsada: …";
 *   - the "dkf ekran" screenings use "Czas: … / Gatunek - … / Scenariusz i
 *     reżyseria: … Produkcja: …" and carry no booking link.
 * `parseDetail` reads both: it collects every `<br>`-separated line from the
 * description blocks, keeps the ones that open with a known label, and pulls
 * each field by label so a film missing a field (or a whole block) just yields
 * `None`/empty for it rather than dropping the film.
 */
class TeatrZiemiRybnickiejClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoTeatrZiemiRybnickiej

  private val BaseUrl    = "https://www.teatrziemirybnickiej.pl"
  // `[` / `]` must be percent-encoded — `new URI(url)` (RealHttpFetch and the
  // test FakeHttpFetch both go through it) rejects the raw brackets.
  private val ListingUrl = s"$BaseUrl/wydarzenia?type%5B%5D=film&month=default"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val detailUrls = parseListing(http.get(ListingUrl))
    val byUrl = ParallelDetailFetch("teatr-ziemi-rybnickiej", detailUrls, 1.minute) { url =>
      Try(http.get(url)).toOption.flatMap(html => parseDetail(html, url))
    }
    detailUrls.flatMap(byUrl.get).flatten
  }

  /** Absolute, de-duplicated detail-page URLs for the film tiles we keep, in
   *  document order. A film with several upcoming dates appears in the listing
   *  once per date but links to a single detail page (which lists all its
   *  dates), so de-duplication keeps us from fetching it twice.
   *
   *  The "kino nie tylko dla seniora" programme — a senior matinée series whose
   *  tile carries that label in its `.info` — is excluded; the "dkf ekran" (and
   *  any other) film programmes are kept. Filtering here means we never fetch
   *  the dropped detail pages. */
  def parseListing(html: String): Seq[String] =
    Jsoup.parse(html).select("div.events-list a.item[href]").asScala
      .filterNot(tile => isSeniorScreening(Option(tile.selectFirst(".info")).map(_.text()).getOrElse("")))
      .map(a => absolute(a.attr("href")))
      .filter(_.nonEmpty)
      .distinct
      .toSeq

  private def isSeniorScreening(programme: String): Boolean =
    programme.toLowerCase.contains("kino nie tylko")

  /** A single film's detail page → a `CinemaMovie`, or `None` when it carries
   *  no title or no parseable showtime (so we never emit an empty row).
   *  Public for tests. */
  def parseDetail(html: String, url: String): Option[CinemaMovie] =
    for {
      root      <- Option(Jsoup.parse(html).selectFirst("div.event-show"))
      title     <- Option(root.selectFirst(".section-title h1")).map(_.ownText().trim).filter(_.nonEmpty)
      showtimes  = parseShowtimes(root)
      if showtimes.nonEmpty
    } yield {
      // Description blocks (everything but the "Terminy" date list). Each <p>
      // is split into its <br>-separated lines; meta lines open with a known
      // label, the synopsis is the first prose line that doesn't (and isn't a
      // price row).
      val lines = root.select(".cmp.cmp-text:not(.dates)").asScala.toSeq
        .flatMap(_.select("p").asScala)
        .flatMap(splitLines)

      val metaText = lines.filter(isMetaLine).mkString(" | ")
      val synopsis = lines.find(l => !isMetaLine(l) && !l.contains("zł") && l.length > 40)

      val (countries, year) = parseProduction(field(metaText, "Produkcja|Prod"))

      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = field(metaText, "Czas trwania|Czas").flatMap(parseRuntime),
          releaseYear    = year,
          countries      = countries,
          genres         = field(metaText, "Gatunek").map(splitList).getOrElse(Seq.empty)
        ),
        cinema    = cinema,
        posterUrl = Option(root.selectFirst(".cmp-image img[src]"))
          .map(img => absolute(img.attr("src"))).filter(_.nonEmpty),
        filmUrl   = Some(url),
        synopsis  = synopsis,
        cast      = field(metaText, "Obsada").map(splitList).getOrElse(Seq.empty),
        director  = field(metaText, "Scenariusz i reżyseria|Reżyseria|Reż")
          .map(d => splitList(d.stripSuffix("."))).getOrElse(Seq.empty),
        showtimes = showtimes
      )
    }

  // ── parsing helpers ─────────────────────────────────────────────────────

  private def absolute(href: String): String =
    if (href.isEmpty || href.startsWith("http")) href else s"$BaseUrl$href"

  /** Split a `<p>`'s inner HTML on `<br>` into trimmed, non-empty text lines. */
  private def splitLines(p: Element): Seq[String] =
    p.html().split("(?i)<br\\s*/?>").iterator
      .map(frag => Jsoup.parse(frag).text().trim)
      .filter(_.nonEmpty)
      .toSeq

  // Labels a metadata line can open with. Longer variants first so a value
  // boundary lands on the full label ("Czas trwania" before "Czas").
  private val AllLabels =
    "Czas trwania|Czas|Scenariusz i reżyseria|Reżyseria|Reż|Produkcja|Prod|Gatunek|Obsada|Bilety"
  // Anchor on the label's `:`/`.`/`-` delimiter rather than `\b` — Java's
  // default `\w` excludes Polish letters, so a `\b` after "Reż" never matches.
  private val MetaLineStart = ("""(?i)^(?:""" + AllLabels + """)\s*[:.\-]""").r

  private def isMetaLine(line: String): Boolean =
    MetaLineStart.findPrefixMatchOf(line).isDefined

  /** Value for `labels` from the joined meta text: everything after the label's
   *  `:`/`.`/`-` delimiter up to the next known label, a ` | ` block boundary,
   *  or the end. Returns `None` when the label is absent. */
  private def field(metaText: String, labels: String): Option[String] = {
    val pat = ("""(?i)(?:""" + labels + """)\s*[:.\-]\s*(.+?)\s*""" +
      """(?=(?:""" + AllLabels + """)\s*[:.\-]|\s*\||$)""").r
    pat.findFirstMatchIn(metaText).map(_.group(1).trim).filter(_.nonEmpty)
  }

  /** "1h 45m" / "1 h 45 min" / "105 min" → total minutes. */
  private def parseRuntime(s: String): Option[Int] = {
    val hours   = """(\d+)\s*h""".r.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    val minutes = """(\d+)\s*m""".r.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    val total   = hours * 60 + minutes
    if (total > 0) Some(total)
    else """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)
  }

  /** "Francja, Wielka Brytania, USA 2026" → (countries, Some(year)).
   *  Restorations carry "1991/2026"; we keep the first (original release)
   *  year so the row matches the canonical film upstream. */
  private def parseProduction(value: Option[String]): (Seq[String], Option[Int]) =
    value match {
      case None => (Seq.empty, None)
      case Some(s) =>
        val year = """\b(\d{4})\b""".r.findFirstMatchIn(s).map(_.group(1).toInt)
        val countries = splitList(s.replaceAll("""\b\d{4}\b(?:\s*/\s*\d{4})?""", ""))
          .map(_.stripSuffix("/").trim).filter(_.nonEmpty)
        (countries, year)
    }

  private def splitList(s: String): Seq[String] =
    s.split("[,/]").map(_.trim).filter(_.nonEmpty).toSeq

  // "8.06.2026, godz. 15.30" (detail page uses a dot in the time;
  // the listing uses a colon — accept both).
  private val DateTimePat =
    """(\d{1,2})\.(\d{1,2})\.(\d{4})\s*,?\s*godz\.?\s*(\d{1,2})[.:](\d{2})""".r

  private def parseShowtimes(root: Element): Seq[Showtime] =
    root.select(".dates li").asScala.toSeq.flatMap { li =>
      val text = Option(li.selectFirst("span")).map(_.text()).getOrElse(li.text())
      DateTimePat.findFirstMatchIn(text).flatMap { m =>
        Try(LocalDateTime.of(
          m.group(3).toInt, m.group(2).toInt, m.group(1).toInt,
          m.group(4).toInt, m.group(5).toInt
        )).toOption.map { dateTime =>
          val booking = Option(li.selectFirst("a[href]")).map(_.attr("href")).filter(_.nonEmpty)
          Showtime(dateTime, booking)
        }
      }
    }.sortBy(_.dateTime)
}
