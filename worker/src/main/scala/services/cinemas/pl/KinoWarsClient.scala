package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{AgeRating, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead}

import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

/**
 * Kino Wars (Wysokie Mazowieckie). Filmweb carries only a sliver of this venue;
 * its own site — a Joomla/Gantry "cinema-blog" at `kino.wysokiemazowieckie.pl/repertuar`
 * — lists the whole programme as one blog post per film, 15 posts per page, with
 * `?start=15`, `?start=30`, … pages linked from `.pagination-next a`.
 *
 * One post (`div.item[itemprop=blogPost]`) carries:
 *   - `h2[itemprop=name]`           → "Gwiazdozbiór psa / napisy", "Lalka / PL",
 *     "Totalna magia 2" — the title, then a `/ <version>` suffix: a language
 *     version (napisy/dubbing → NAP/DUB format tokens) or `PL` (a Polish film).
 *   - `.item-image a` / `img`       → the film's own page and its poster.
 *   - a pipe-delimited meta line    → "Akcja, komedia | Od lat 8 | 113 min." —
 *     genres, the age rating, the runtime (a stage show's line reads
 *     "Spektakl teatralny | 2 godz. z przerwą | Cena biletu 140 zł" instead).
 *   - the synopsis paragraph(s)     → between the meta line and the ticket link.
 *   - `a[href*=bilety.]`            → the per-film iKsoris ticketing page
 *     (`rezerwacja/termin.html?…idw=N`), used as every showtime's booking link.
 *   - the dates paragraph           → "25.09.2026 r. - godz. 17:00" per line, or
 *     "02.10.2026 r. godz. 17:00 i 20:00" for two shows on a day. A "JUŻ WKRÓTCE"
 *     line (date announced, time not yet) has no time and yields nothing.
 *   - a YouTube "ZWIASTUN" link     → the trailer.
 *
 * Every date carries its year, so no `today` is needed. The listing shows only
 * each post's intro; the per-film page (`filmUrl`) carries the full article,
 * where foreign titles add plain-text credit lines below the fold —
 * "Występują: Austin Abrams, Zach Cherry, Kali Reis i Paul Walter Hauser." and
 * "Reżyseria: Zach Cregger". The deferred [[fetchFilmDetail]] reads those (one
 * page per film, via the `EnrichDetails` task, never inline in the scrape);
 * Polish titles carry no such lines and stay empty. Stage shows in the same
 * listing ("Królowe życia - spektakl") are left to [[NonMovieEventClassifier]]
 * at the scrape seam.
 */
class KinoWarsClient(http: HttpFetch, override val cinema: Cinema = KinoWars) extends CinemaScraper with DetailEnricher {

  import KinoWarsClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  // A standalone venue: its own slug is the dedup/freshness scope.
  override def detailGroup: String = cinema.slug

  /** Deferred per-film detail — the director and cast lines off the film's own
   *  page. None on a transient fetch failure so the task stays stale and retries;
   *  a durable 404/410 escapes (see [[DetailFetchOutcome]]). */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(html => parseDetail(Jsoup.parse(html, BaseUrl)))

  // The first page's failure propagates (a red scrape, never a white "0 films");
  // a later page failing does too — a half-read programme is not a scrape result.
  def fetch(): Seq[CinemaMovie] = parse(pages(RepertoireUrl, Vector.empty), cinema)

  @tailrec
  private def pages(url: String, read: Vector[Document]): Seq[Document] = {
    val document = Jsoup.parse(HttpRead.page(http, url), BaseUrl)
    val all      = read :+ document
    nextPageUrl(document) match {
      case Some(_) if all.size >= MaxPages =>
        throw new IllegalStateException(s"$RepertoireUrl did not end after $MaxPages pages")
      case Some(next) => pages(next, all)
      case None       => all
    }
  }
}

object KinoWarsClient {

  val BaseUrl       = "https://kino.wysokiemazowieckie.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar"

  /** A runaway-pagination backstop; the live programme spans two pages. Hitting it
   *  fails the scrape: a programme cut here would have its later pages pruned. */
  private val MaxPages = 10

  /** A screening-day line opens with `DD.MM.YYYY r.`; its `HH:MM` tokens are
   *  that day's shows ("godz. 17:00 i 20:00"). Anchored so a date quoted inside
   *  the synopsis prose is never read as a screening. */
  private val ScreeningDay = """^\d{1,2}\.\d{1,2}\.\d{4}(?=\s*r\.)""".r
  private val TimeToken = raw"""\b(${ScraperParse.ClockText})\b""".r
  private val Runtime   = """(\d+)\s*min""".r
  /** The trailing `/ PL` (Polish film) or bare `/` left once a version word is peeled. */
  private val TrailingVersionSlash = """\s*/\s*(?:PL)?\s*$""".r

  private case class Post(
    title:     String,
    rawTitle:  String,
    format:    List[String],
    runtime:   Option[Int],
    genres:    Seq[String],
    ageRating: Option[String],
    synopsis:  Option[String],
    poster:    Option[String],
    filmUrl:   Option[String],
    booking:   Option[String],
    trailer:   Option[String],
    dateTimes: Seq[LocalDateTime]
  )

  private[pl] def nextPageUrl(document: Document): Option[String] =
    Option(document.selectFirst(".pagination-next a[href]")).map(_.attr("abs:href")).filter(_.nonEmpty)

  def parse(pages: Seq[Document], cinema: Cinema): Seq[CinemaMovie] = {
    val posts = pages.flatMap(_.select("div.item[itemprop=blogPost]").asScala).flatMap(parsePost)
    val slots = posts.flatMap(post => post.dateTimes.map(post -> _))

    SlotsToMovies.fold(
      slots,
      titleOf    = _._1.title,
      showtimeOf = { case (post, dateTime) => Showtime(dateTime, post.booking, None, post.format) }
    ) { (title, group, showtimes) =>
      val post = group.head._1
      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = post.runtime,
          genres         = post.genres,
          rawTitle       = Some(post.rawTitle).filter(_ != title)
        ),
        cinema     = cinema,
        posterUrl  = post.poster,
        filmUrl    = post.filmUrl,
        synopsis   = post.synopsis,
        cast       = Seq.empty,
        director   = Seq.empty,
        showtimes  = showtimes,
        trailerUrl = post.trailer,
        ageRating  = post.ageRating
      )
    }
  }

  /** A per-film page's credit-line labels: "Reżyseria: Zach Cregger",
   *  "Występują: A, B i C." */
  private val DirectorLabel = "Reżyseria"
  private val CastLabel     = "Występują"
  /** A credit list's separators: commas and the final Polish " i " ("and"). */
  private val PeopleSeparator = """\s*,\s*|\s+i\s+""".r

  /** Director and cast off a per-film page's article body — each a
   *  `Label: names` line, present on foreign titles only. */
  private[pl] def parseDetail(document: Document): FilmDetail = {
    val lines = document.select("[itemprop=articleBody] p").asScala.toSeq.flatMap(ScraperParse.linesOf)
    def people(label: String): Seq[String] =
      lines.collectFirst { case line if line.startsWith(s"$label:") => line.stripPrefix(s"$label:") }.toSeq
        .flatMap(PeopleSeparator.split)
        .map(_.trim.stripSuffix(".").trim)
        .filter(_.nonEmpty)
    FilmDetail(director = people(DirectorLabel), cast = people(CastLabel))
  }

  private def parsePost(item: Element): Option[Post] =
    Option(item.selectFirst("h2[itemprop=name]")).map(_.text.trim).filter(_.nonEmpty).map { rawTitle =>
      val (stripped, format) = ScraperParse.extractFormatTags(rawTitle)
      val title      = TrailingVersionSlash.replaceFirstIn(stripped, "").trim
      val paragraphs = item.select(".blog-content-wrapper > p, .blog-content-wrapper > div").asScala.toSeq
      val meta       = paragraphs.find(_.text.contains("|")).map(_.text)
      Post(
        title     = title,
        rawTitle  = rawTitle,
        format    = format,
        runtime   = meta.flatMap(Runtime.findFirstMatchIn).flatMap(_.group(1).toIntOption),
        genres    = meta.toSeq.flatMap(genresOf),
        ageRating = meta.flatMap(AgeRating.polishMinimumAge),
        synopsis  = synopsisOf(paragraphs),
        poster    = Option(item.selectFirst(".item-image img")).map(_.attr("abs:src")).filter(_.nonEmpty),
        filmUrl   = Option(item.selectFirst(".item-image a[href], a.readmore[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
        booking   = Option(item.selectFirst("a[href*=bilety.]")).map(_.attr("abs:href")).filter(_.nonEmpty),
        trailer   = item.select("a[href]").asScala.iterator.map(_.attr("abs:href")).flatMap(ScraperParse.canonicalTrailer).nextOption(),
        dateTimes = paragraphs.flatMap(p => ScraperParse.linesOf(p)).flatMap(dateTimesOf)
      )
    }

  /** The genre list is the meta line's first segment ("Akcja, przygodowy, sci-fi");
   *  a segment that is itself an age rating or a runtime means there is none. */
  private def genresOf(meta: String): Seq[String] =
    meta.split('|').headOption.toSeq
      .filterNot(segment => AgeRating.polishMinimumAge(segment).isDefined || Runtime.findFirstIn(segment).isDefined)
      .flatMap(_.split(','))
      .map(_.trim)
      .filter(_.nonEmpty)

  /** The ">>> KUP BILET <<<" paragraph closes the synopsis; it carries the ticket
   *  link once sale opens and is plain text ("JUŻ WKRÓTCE!") before. */
  private val TicketParagraphMarker = "KUP BILET"

  /** The prose between the meta line and the ticket paragraph. */
  private def synopsisOf(paragraphs: Seq[Element]): Option[String] =
    Some(
      paragraphs
        .dropWhile(!_.text.contains("|"))
        .drop(1)
        .takeWhile(p => p.selectFirst("a[href*=bilety.]") == null && !p.text.contains(TicketParagraphMarker))
        .map(_.text.trim)
        .filterNot(t => t.isEmpty || t.startsWith("UWAGA") || ScreeningDay.findFirstIn(t).isDefined)
        .mkString("\n\n")
    ).filter(_.nonEmpty)

  /** Every screening on one `DD.MM.YYYY r. …` line — none for a "JUŻ WKRÓTCE"
   *  line that names a date but no time yet. */
  private def dateTimesOf(line: String): Seq[LocalDateTime] =
    ScreeningDay.findFirstIn(line).flatMap(ScraperParse.parseDate).toSeq.flatMap { date =>
      TimeToken.findAllIn(line).flatMap(ScraperParse.parseHHmm).map(LocalDateTime.of(date, _))
    }
}
