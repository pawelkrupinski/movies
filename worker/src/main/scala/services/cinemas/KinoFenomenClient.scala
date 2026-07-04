package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Fenomen — the art-house cinema of Wojewódzki Dom Kultury im. Józefa
 * Piłsudskiego in Kielce. The venue's repertoire is published on a
 * server-rendered biletyna.pl iframe (venue id 639) at
 * `https://iframe639.biletyna.pl/?display=events`.
 *
 * Each event row (`div.iframe_all`) carries:
 *   - Date: `span.B-font-weight--bold` — `DD.MM.YYYY`
 *   - Time: the second `span.B-font-weight--bold` — `HH:MM`
 *   - Title: `div.iframe_all_event_title a[href^="/artist/view/id/"]` — raw title
 *     with a trailing `(2D/napisy)` / `(2D/oryginalny)` format tag (stripped) and
 *     an optional `(YYYY)` paren year for older films. (Biletyna used to append
 *     `| reżyseria: … | Country YYYY` pipe metadata here; it has since dropped it,
 *     so [[parseDirectors]] / the pipe branch of [[parseYear]] are now defensive —
 *     the director now comes from the artist detail page instead.)
 *   - Booking URL: `a.B-btn--accent[href^="/event/view/id/"]`
 *   - Poster: `img.img-responsive[src^="/file/get/"]` (first one, optional)
 *
 * The listing covers the next few weeks in one page — no pagination needed.
 */
class KinoFenomenClient(
  http: HttpFetch,
  override val cinema: Cinema = KinoFenomen
) extends CinemaScraper with DetailEnricher {

  import KinoFenomenClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(ListingUrl)
    val document  = Jsoup.parse(html)

    val slots = document.select("div.iframe_all").asScala.toSeq.flatMap(parseSlot)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title, releaseYear = head.year),
        cinema    = cinema,
        posterUrl = head.poster,
        // The `/artist/view/id/` page is the film's detail page (fetched below);
        // use it as the filmUrl. Each showing keeps its own `/event/view/id/`
        // booking link.
        filmUrl   = head.artistUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = head.directors,
        showtimes = showtimes
      )
    }
  }

  // The listing carries the title (+ a `(YYYY)` paren year for older films), so the
  // film resolves from the listing; the artist detail page adds director, cast,
  // synopsis, poster, runtime, countries/genres, merged in asynchronously by the
  // EnrichDetails task. That detail fetch 403s our datacenter IP behind biletyna's
  // Cloudflare, so this client is wired through the residential-proxy `bnFetch` seam
  // in CinemaScraperCatalog — not the shared `http` — or every detail fetch fails.
  override val detailGroup: String = "kino-fenomen"
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail — the EnrichDetails task calls this with the slot's
   *  `/artist/view/id/<id>` filmUrl. None when nothing useful parsed. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(http.get(ref)).toOption.map(parseDetail)
      .filter(d => d.synopsis.nonEmpty || d.cast.nonEmpty || d.director.nonEmpty)
}

object KinoFenomenClient {

  val BaseUrl    = "https://iframe639.biletyna.pl"
  val ListingUrl = s"$BaseUrl/?display=events"

  private val DateFmt   = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")
  // Format tag at the end of artist names: "(2D/napisy)", "(3D/dubbing)", etc.
  private val FormatPat = """\s*\((\d[Dd]/[^)]+)\)\s*$""".r
  // The `| reżyseria: <names> |` segment of the artist-link text; names are a
  // comma-separated list bounded by the next pipe (or the end).
  private val DirectorPat = """(?i)reżyseria:\s*([^|]+)""".r
  // A `(YYYY)` paren year in the title itself (e.g. "Mikey i Nicky (1976)").
  private val ParenYearPat = """\((?:19|20)\d{2}\)""".r
  // Any in-range 4-digit year token (used inside the trailing metadata segment,
  // where it follows the production countries: "… Węgry 2025").
  private val YearTokenPat = """\b(?:19|20)\d{2}\b""".r

  /** Director(s) from the `| reżyseria: … |` metadata segment, comma-split.
   *  Empty when the listing carries no director metadata for the film. */
  def parseDirectors(rawTitle: String): Seq[String] =
    DirectorPat.findFirstMatchIn(rawTitle).map(_.group(1).trim).toSeq
      .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))

  /** Production year — from the trailing `| … Country YYYY` metadata segment, or
   *  a `(YYYY)` paren in the title. Only these two structured positions count, so
   *  a stray number in a bare title (no pipe metadata, no paren) is not mistaken
   *  for a year. None when neither is present. */
  def parseYear(rawTitle: String): Option[Int] = {
    val parts    = rawTitle.split('|')
    val metaYear = if (parts.length > 1)
      YearTokenPat.findFirstMatchIn(parts.drop(1).mkString(" ")).map(_.matched.toInt)
    else None
    metaYear.orElse(ParenYearPat.findFirstMatchIn(rawTitle).map(_.matched.filter(_.isDigit).toInt))
  }

  private[cinemas] case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    poster:    Option[String],
    format:    List[String],
    directors: Seq[String],
    year:      Option[Int],
    artistUrl: Option[String]
  )

  private[cinemas] def parseSlot(row: org.jsoup.nodes.Element): Option[RawSlot] = {
    // Date + time — biletyna renders two bold spans: date then "godz." label
    // then the time bold span; select the event-date sub-block specifically.
    val dateBold = Option(row.selectFirst("div.event-date span.B-font-weight--bold"))
      .map(_.text.trim)
    val timeBold = row.select("div.event-date .B-text--nowrap span.B-font-weight--bold")
      .asScala.headOption.map(_.text.trim)

    val dt = for {
      d <- dateBold
      t <- timeBold
      ldt <- Try(LocalDateTime.parse(s"$d $t", DateFmt)).toOption
    } yield ldt

    // Raw title from the artist link — strip pipe-separated metadata + format tag.
    // The same link's href is the film's detail (artist) page.
    val artistLink = Option(row.selectFirst("div.iframe_all_event_title a[href^=\"/artist/view/id/\"]"))
    val rawTitle   = artistLink.map(_.text.trim).filter(_.nonEmpty)
    val artistUrl  = artistLink.map(a => BaseUrl + a.attr("href"))

    val title = rawTitle.map { t =>
      val beforePipe = t.split('|').head.trim
      FormatPat.replaceFirstIn(beforePipe, "")
    }

    val format = rawTitle.toList.flatMap { t =>
      FormatPat.findFirstMatchIn(t).toList.flatMap { m =>
        m.group(1).split("/").map(_.trim).filter(_.nonEmpty).toList
      }
    }

    val booking = Option(row.selectFirst("a.B-btn--accent[href^=\"/event/view/id/\"]"))
      .map(a => BaseUrl + a.attr("href"))

    val poster = Option(row.selectFirst("img.img-responsive[src^=\"/file/get/\"]"))
      .map(img => BaseUrl + img.attr("src"))

    val directors = rawTitle.toSeq.flatMap(parseDirectors)
    val year      = rawTitle.flatMap(parseYear)

    for {
      t  <- title
      ldt <- dt
    } yield RawSlot(t, ldt, booking, poster, format, directors, year, artistUrl)
  }

  private val RuntimeMinPat = """(\d+)\s*min""".r

  private val HeadMarkerPat = """(?i)^(reżyseria|występują):""".r

  /** Normalise one description line into the per-marker lines the parser expects.
   *  The newer detail layout runs the structured head together in a single `<p>`
   *  ("reżyseria: X występują: Y / country / genre / year / N min."); split it
   *  before "występują:" and then, on a marker segment (never on synopsis prose),
   *  before the first " / " that starts the trailing meta run. A line in the older
   *  separate-`<p>` layout (or plain prose) has no such boundary and passes through
   *  whole — prose keeping any internal " / " intact. */
  private[cinemas] def splitHead(line: String): Seq[String] =
    line.split("(?i)(?=występują:)").toSeq.flatMap { seg =>
      val trimmed = seg.trim
      val metaAt  = if (HeadMarkerPat.findPrefixMatchOf(trimmed).isDefined) trimmed.indexOf(" / ") else -1
      if (metaAt >= 0) Seq(trimmed.substring(0, metaAt).trim, trimmed.substring(metaAt + 1).trim)
      else Seq(trimmed)
    }.filter(_.nonEmpty)

  /** Parse an `/artist/view/id/<id>` detail page into a [[FilmDetail]]. The
   *  `#artist-view-description` block holds `<p>`/`<br>`-separated lines: an
   *  optional "reżyseria: …" / "występują: …" pair, an optional
   *  "/ <countries> / <genre> / <year> / <NNN min.>" meta line, and the synopsis
   *  prose. Some films carry only the prose. The structured head is emitted EITHER
   *  as those separate lines OR (newer layout) run together in one `<p>`:
   *  "reżyseria: X występują: Y / country / genre / year / N min." — [[splitHead]]
   *  normalises the collapsed form back to the per-marker lines so one parser
   *  handles both. */
  private[cinemas] def parseDetail(html: String): FilmDetail = {
    val doc   = Jsoup.parse(html)
    val lines = Option(doc.selectFirst("#artist-view-description")).toSeq
      .flatMap(_.select("p").asScala)
      .flatMap(_.html.split("(?i)<br\\s*/?>").iterator.map(s => Jsoup.parse(s).text.trim))
      .filter(_.nonEmpty)
      .flatMap(splitHead)
    def after(marker: String): Seq[String] =
      lines.find(_.toLowerCase.startsWith(marker)).toSeq
        .flatMap(_.replaceFirst("(?i)^" + marker + ":?\\s*", "").split(",").map(_.trim).filter(_.nonEmpty))
    val metaParts = lines.find(_.startsWith("/")).toSeq
      .flatMap(_.stripPrefix("/").split("/").map(_.trim).filter(_.nonEmpty))
    val year      = metaParts.flatMap(YearTokenPat.findFirstIn).headOption.map(_.toInt)
    val runtime   = metaParts.flatMap(p => RuntimeMinPat.findFirstMatchIn(p).map(_.group(1).toInt)).headOption
    val countries = metaParts.headOption.toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val genres    = metaParts.drop(1)
      .filterNot(p => YearTokenPat.findFirstIn(p).isDefined || RuntimeMinPat.findFirstMatchIn(p).isDefined)
    // Synopsis = the prose lines: long, and not one of the marker/meta/title lines.
    val synopsis  = lines.filter(l => l.length > 60 && !l.startsWith("/") &&
                      !l.toLowerCase.startsWith("reżyseria") && !l.toLowerCase.startsWith("występują"))
      .mkString("\n").trim
    val poster    = Option(doc.selectFirst("a.artist-poster img[src]")).map(_.attr("src").trim)
      .filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else BaseUrl + u)
    FilmDetail(
      synopsis       = Some(synopsis).filter(_.length > 20),
      cast           = after("występują"),
      director       = after("reżyseria"),
      runtimeMinutes = runtime,
      releaseYear    = year,
      countries      = countries,
      genres         = genres,
      posterUrl      = poster
    )
  }
}
