package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import services.movies.TitleNormalizer
import tools.{CachingDetailFetch, HttpFetch}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Client for cinemas on the CURRENT bilety24.pl platform, whose venue pages now
 * live on the main domain at `www.bilety24.pl/kino/organizator/<slug>-<id>`
 * (the per-venue `*.bilety24.pl` subdomains the legacy [[Bilety24Client]] reads
 * are being decommissioned — Kino Kosmos, Kino Światowid and Kino Elektronik
 * had their subdomains go to a dead host, which showed as red `ConnectException`
 * bars on /uptime).
 *
 * The organizer page is server-rendered and lists every screening as an
 * anchor whose `title` attribute carries the whole slot:
 *   `<a href="/kino/<id>-<slug>-<eventid>?id=N"
 *       title="Film: <Title> - YYYY-MM-DD HH:MM - <City>">`
 * One fetch yields the full programme. We read the title attribute (the same
 * `Film: … - date time - city` encoding the legacy client read off its buy
 * buttons), keep each slot's `/kino/<slug>` event-page href both as the booking
 * link and as the film's detail `filmUrl`, and group by title.
 *
 * The listing carries no film identity beyond the title, so we fetch each film's
 * event page for DISPLAY enrichment ([[fetchFilmDetail]]) via [[DetailEnricher]].
 * Those pages turn out to expose ONLY a synopsis (`div.description`) and an
 * `og:image` poster — there is NO structured `Reżyseria` / `Rok` / `Obsada`
 * block on bilety24 (verified against real Backrooms / Toy Story 5 pages: the
 * `h1.header-b24` block is followed by the synopsis and nothing else; the
 * country/year/runtime strings elsewhere on the page belong to the "Inne
 * wydarzenia w pobliżu" recommendation cards, not the main film). So the detail
 * supplies no TMDB-identity hint and `defersTmdbResolution` is FALSE — the row
 * resolves immediately from the bare title and the synopsis/poster merge in
 * asynchronously when the `EnrichDetails` task lands.
 *
 * One instance per venue, captured by its `organizerUrl` + `cinema`, so adding
 * a bilety24-hosted cinema is a catalog line, not a new client (OCP).
 */
class Bilety24OrganizerClient(http: HttpFetch, organizerUrl: String, override val cinema: Cinema)
    extends CinemaScraper with DetailEnricher {

  // Event pages are static across passes for a live film, so cache them.
  private val detailHttp = new CachingDetailFetch(http)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(organizerUrl)
  override def sourceUrl: Option[String] = Some(organizerUrl)

  override val detailGroup: String = "bilety24-organizer"

  // The detail is purely display enrichment (synopsis + poster); the listing
  // already carries the only identity the page has (the title), so the row
  // resolves from the listing and the detail merges in asynchronously.
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail off the `/kino/<slug>` event page — synopsis and
   *  poster only (bilety24 exposes no structured year/director/cast). None on a
   *  fetch failure so the task stays stale and retries rather than recording an
   *  empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map(html => Bilety24OrganizerClient.parseDetail(Jsoup.parse(html)))

  def fetch(): Seq[CinemaMovie] =
    Bilety24OrganizerClient.parse(http.get(organizerUrl), cinema)
}

object Bilety24OrganizerClient {

  val BaseUrl = "https://www.bilety24.pl"

  // title="Film: <Title> - 2026-06-19 18:50 - Katowice" — the title may itself
  // contain " - ", so the non-greedy capture stops at the first " - <ISO date>".
  private val SlotPat = """(?s)Film:\s*(.+?)\s*-\s*(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2})\s*-""".r
  private val Fmt     = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  private case class RawSlot(title: String, dateTime: LocalDateTime, eventUrl: Option[String],
                            format: List[String])


  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("a[title]").asScala.toSeq.flatMap { a =>
      SlotPat.findFirstMatchIn(a.attr("title")).flatMap { m =>
        Try(LocalDateTime.parse(s"${m.group(2)} ${m.group(3)}", Fmt)).toOption.map { dt =>
          // extractFormatTags (not stripFormatTags): the format/version word a
          // portal buries in the title — "Supergirl_dubbing", "…_3D" on Forum
          // Bolesławiec — is peeled so the variants merge into one film AND
          // surfaced as a `Showtime.format` badge instead of being discarded.
          val (clean, format) = ScraperParse.extractFormatTags(m.group(1))
          // The `Film:` anchor's href is the `/kino/<slug>` event page — kept
          // both as the slot's booking link and as the film's detail `filmUrl`.
          RawSlot(clean, dt, Option(a.attr("abs:href")).filter(_.nonEmpty), format)
        }
      }
    }

    SlotsToMovies.fold(
      slots.filter(_.title.nonEmpty),
      titleOf    = s => TitleNormalizer.cinemaClean(cinema.slug, s.title),
      showtimeOf = s => Showtime(s.dateTime, s.eventUrl, format = s.format)
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.eventUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  // ── Per-film event page (deferred display enrichment) ───────────────────────

  /** Parse a `/kino/<slug>` event page. The main film block is `h1.header-b24`
    * followed by `div.description` (the synopsis) and an `og:image` poster; the
    * page carries no structured year/director/cast. The description block also
    * holds a "czytaj więcej" toggle and a boilerplate "Bezpieczne zakupy…"
    * refund-policy paragraph (wrapped in `<em>`/`<p>`), so we drop the anchors
    * and those trailing blocks and clean the prose the same way
    * [[Bilety24Client]] does. */
  private[cinemas] def parseDetail(document: Document): FilmDetail = {
    val synopsis = Option(document.selectFirst("div.description"))
      .map(ScraperParse.cleanSynopsis(_, "a", "p", "em")).filter(_.length > 20)
    val poster = Option(document.selectFirst("meta[property=og:image]"))
      .map(_.attr("content")).filter(_.nonEmpty)
    FilmDetail(synopsis = synopsis, posterUrl = poster)
  }
}
