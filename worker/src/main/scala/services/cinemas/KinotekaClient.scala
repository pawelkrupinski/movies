package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{CachingDetailFetch, HttpFetch}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kinoteka (Warszawa, PKiN). The `/repertuar/` page is per-day; its date nav
 * links to `?date=YYYY-MM-DD`. Each film article carries genres + a row of
 * screening anchors (each with an absolute `data-day`, time and buy link). The
 * `/film/<slug>/` detail page adds runtime / director / countries / year /
 * original title / synopsis. Dates come from the page's own nav, so the replay
 * is deterministic.
 *
 * Chunked scrape: `planChunks` enumerates the date nav, each `fetchChunk(date)`
 * pulls + parses one day page independently (its own queued task, so a slow day
 * can't pin the whole ~2-week scrape), and `reduceChunks` merges the per-day
 * films by `/film/<slug>/`. The synchronous `fetch()` (the harness path) composes
 * these to exactly the old whole-scrape output.
 */
class KinotekaClient(http: HttpFetch) extends ChunkedCinemaScraper with DetailEnricher {

  // Static detail pages cached across passes (CachingDetailFetch); the listing
  // and day pages keep the live `http` since their showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = Kinoteka

  private val BaseUrl    = "https://kinoteka.pl"
  private val ListingUrl = s"$BaseUrl/repertuar/"
  private val DatePat    = """[?&]date=(\d{4}-\d{2}-\d{2})""".r
  private val SlugPat    = """/film/([^/"?#]+)""".r

  private case class RawSlot(slug: String, title: String, genres: Seq[String], dateTime: LocalDateTime,
                             booking: Option[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  /** The date nav off the listing page = one chunk per day. A failed fetch here
   *  fails the whole scrape (recorded as a normal outcome), as before. */
  def planChunks(): Seq[String] =
    DatePat.findAllMatchIn(http.get(ListingUrl)).map(_.group(1)).toSeq.distinct

  /** One day page → that day's films (slots grouped by slug). A throw reschedules
   *  just this day's chunk task. */
  def fetchChunk(date: String): Seq[CinemaMovie] =
    moviesFrom(parsePage(http.get(s"$ListingUrl?date=$date")))

  /** Merge the per-day films by `/film/<slug>/`, unioning showtimes — the same
   *  cross-day fold the old whole-scrape did (deterministic by film URL; poster is
   *  the first non-empty across days). */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    chunks.toSeq.sortBy(_._1).flatMap(_._2)
      .groupBy(_.filmUrl).toSeq.sortBy(_._1.map(_.toString).getOrElse(""))
      .map { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
        group.head.copy(showtimes = showtimes, posterUrl = group.flatMap(_.posterUrl).headOption)
      }

  /** This day's slots → one film per slug, with that day's showtimes. */
  private def moviesFrom(slots: Seq[RawSlot]): Seq[CinemaMovie] =
    slots.groupBy(_.slug).toSeq.flatMap { case (slug, group) =>
      val primary   = group.head
      val showtimes = group.map(s => Showtime(s.dateTime, s.booking, None, Nil))
                       .distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title, genres = primary.genres),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/film/$slug/") else None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes,
        trailerUrl = None
      ))
    }

  override val detailGroup: String = "kinoteka"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://kinoteka.pl/film/<slug>/`). None on fetch
   *  failure so the task stays stale and is retried rather than recording an
   *  empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val detail = KinotekaClient.parseDetail(html)
      FilmDetail(
        synopsis       = detail.synopsis,
        cast           = detail.cast,
        director       = detail.director,
        runtimeMinutes = detail.runtime,
        releaseYear    = detail.year,
        originalTitle  = detail.originalTitle,
        countries      = detail.countries,
        genres         = Seq.empty, // genres come from the listing page, not the detail page
        posterUrl      = detail.poster,
        trailerUrl     = detail.trailer
      )
    }

  private def parsePage(html: String): Seq[RawSlot] =
    Jsoup.parse(html).select("article.e-movie").asScala.toSeq.flatMap { art =>
      val link = Option(art.selectFirst("h3.e-movie__heading a[href]"))
      val slug = link.flatMap(a => SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1)))
      val title = link.map(_.text.trim).filter(_.nonEmpty)
      (slug, title) match {
        case (Some(s), Some(t)) =>
          val genres = Option(art.selectFirst("p.e-movie__category")).map(_.text.trim).filter(_.nonEmpty)
                         .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)).map(tools.TextNormalization.titleCaseIfAllLower)
          art.select("a[data-hour][data-day]").asScala.toSeq.flatMap { a =>
            val day  = a.attr("data-day"); val hour = a.attr("data-hour")
            KinotekaClient.parseDateTime(day, hour).map { dt =>
              val booking = Seq("data-buy-link", "href").map(a.attr).find(v => v.nonEmpty && v != "#")
              val poster  = Option(a.attr("data-poster-link")).filter(_.nonEmpty)
              RawSlot(s, t, genres, dt, booking, poster)
            }
          }
        case _ => Seq.empty
      }
    }
}

object KinotekaClient {

  def parseDateTime(day: String, hour: String): Option[LocalDateTime] =
    """(\d{1,2}):(\d{2})""".r.findFirstMatchIn(hour).flatMap(m =>
      Try(LocalDateTime.parse(s"${day}T${pad(m.group(1))}:${m.group(2)}:00")).toOption)

  private def pad(s: String): String = if (s.length == 1) s"0$s" else s

  final case class Detail(runtime: Option[Int], year: Option[Int], originalTitle: Option[String],
                          countries: Seq[String], director: Seq[String], cast: Seq[String],
                          synopsis: Option[String], poster: Option[String], trailer: Option[String])
  object Detail { val empty: Detail = Detail(None, None, None, Seq.empty, Seq.empty, Seq.empty, None, None, None) }

  private def dd(document: org.jsoup.nodes.Document, label: String): Option[String] =
    ScraperParse.ddField(document, label, "dl.p-movie-details__general-info dt")

  def parseDetail(html: String): Detail = {
    val document = Jsoup.parse(html)
    Detail(
      // "Czas trwania filmu" is the film; the page also carries a "Czas trwania
      // reklam" (ad/trailer block) row — match the film one so we never read the
      // ad length as the runtime regardless of which row comes first.
      runtime       = dd(document, "czas trwania filmu").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year          = dd(document, "data premiery").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      originalTitle = dd(document, "oryginalny tytuł").filter(_.nonEmpty),
      countries     = dd(document, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      director      = dd(document, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      cast          = dd(document, "obsada").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      // Event pages append an agenda ("Harmonogram wydarzenia: godz. 18:00 –
      // …") plus a partner/sponsor list as trailing <p>s of the body. When such
      // an agenda heading is present, keep only the prose paragraphs before it.
      synopsis      = Option(document.selectFirst("div.mce-content-body")).map { body =>
                        val ps = body.select("p").asScala.toSeq
                        if (ps.exists(_.text.trim.toLowerCase.startsWith("harmonogram")))
                          ps.map(_.text.trim).takeWhile(!_.toLowerCase.startsWith("harmonogram")).filter(_.nonEmpty).mkString("\n\n")
                        else ScraperParse.blockText(body).trim
                      }.map(_.trim).filter(_.length > 20)
                        .orElse(Option(document.selectFirst("meta[property=og:description]")).map(_.attr("content").trim)).filter(_.length > 20),
      // The hero poster is a `<picture class="p-movie-details__hero-poster">`
      // (not a `<div>`) wrapping the real film `<img>`; match by class on any
      // element so we don't fall through to the generic site og:image logo.
      poster        = Option(document.selectFirst(".p-movie-details__hero-poster img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                        .orElse(Option(document.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty)),
      // The trailer is a YouTube `/embed/` iframe in the content figure; skip
      // the GTM/analytics iframes by taking the first src that canonicalises.
      trailer       = document.select("iframe[src]").asScala.iterator.map(_.attr("src")).filter(_.nonEmpty)
                        .flatMap(ScraperParse.canonicalTrailer).nextOption()
    )
  }
}
