package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{CachingDetailFetch, HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Amondo (Warszawa). The `/repertuar/` page groups screenings into day
 * panes (`div#schedule-YYYY-MM-DD`); each film block carries its title, genre,
 * per-screening room+time, poster and booking link. The `/movies/<slug>` detail
 * page adds director, production countries, year and the full synopsis.
 */
class AmondoClient(http: HttpFetch, deferDetail: Boolean = false) extends CinemaScraper with DetailEnricher {

  // Static detail pages cached across passes; the repertoire page keeps the
  // live `http` since its showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = KinoAmondo

  private val BaseUrl    = "https://kinoamondo.pl"
  private val RepertoireUrl = s"$BaseUrl/repertuar/"
  private val SlugPat    = """/movies/([^/]+)/""".r

  private case class RawSlot(slug: String, title: String, genres: Seq[String], dateTime: LocalDateTime,
                             room: Option[String], booking: Option[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  // When deferDetail is on, fetch() returns BARE movies (showtimes + poster +
  // the per-film detail-page URL) and the detail is filled in later by an
  // EnrichDetails task via `fetchFilmDetail` — so a scrape pass doesn't block on
  // N detail-page round-trips. When off (default), it enriches inline as before.
  def fetch(): Seq[CinemaMovie] = {
    val bare = fetchBare()
    if (deferDetail) bare else enrichInline(bare)
  }

  private def fetchBare(): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(http.get(RepertoireUrl))

    val slots = doc.select("div[id^=schedule-]").asScala.toSeq.flatMap { pane =>
      val date = pane.id.stripPrefix("schedule-")
      pane.select("div.movie-tabs").asScala.toSeq.flatMap(block => blockSlots(block, date))
    }

    val bySlug = slots.groupBy(_.slug)
    bySlug.toSeq.flatMap { case (slug, group) =>
      val primary   = group.head
      val showtimes = group.distinctBy(s => (s.dateTime, s.room)).sortBy(_.dateTime)
                        .map(s => Showtime(s.dateTime, s.booking, s.room, Nil))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title, genres = primary.genres),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/movies/$slug/") else None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }
  }

  // Inline path: fetch each film's detail in parallel through the same
  // `fetchFilmDetail` the deferred path uses, then merge non-destructively. One
  // detail code path for both modes.
  private def enrichInline(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val urls = movies.flatMap(_.filmUrl).distinct
    if (urls.isEmpty) movies
    else {
      val metas = ParallelDetailFetch("amondo-details", urls, 1.minute)(u => fetchFilmDetail(u))
      movies.map(m => m.filmUrl.flatMap(metas.get).flatten.map(_.applyTo(m)).getOrElse(m))
    }
  }

  override val detailGroup: String = "amondo"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://kinoamondo.pl/movies/<slug>/`). None on fetch
   *  failure so the task stays stale and is retried rather than recording an
   *  empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val d = AmondoClient.parseDetail(html)
      FilmDetail(
        synopsis       = d.synopsis,
        cast           = Seq.empty,
        director       = d.director,
        runtimeMinutes = d.runtimeMinutes,
        releaseYear    = d.year,
        countries      = d.countries
      )
    }

  private def blockSlots(block: Element, date: String): Seq[RawSlot] = {
    val title  = Option(block.selectFirst("h3.no-underline")).map(_.text.trim).filter(_.nonEmpty)
    val slug   = Option(block.selectFirst("a[href*=/movies/]")).map(_.attr("href"))
                   .flatMap(h => SlugPat.findFirstMatchIn(h).map(_.group(1)))
    val genres = Option(block.selectFirst("span.title")).map(_.text.trim).filter(_.nonEmpty)
                   .toSeq.flatMap(_.split("[,/]").map(_.trim).filter(_.nonEmpty)).map(tools.TextNormalization.titleCaseIfAllLower)
    val poster = Option(block.selectFirst("img[src]")).map(_.attr("src")).filter(_.nonEmpty).map(abs)
    title.toSeq.flatMap { t =>
      block.select("div.time-wrap span.time").asScala.toSeq.flatMap { span =>
        val parts = span.text.split(",").map(_.trim).filter(_.nonEmpty)
        val time  = parts.lastOption.getOrElse("")
        val room  = if (parts.length > 1) Some(parts.init.mkString(", ")) else None
        AmondoClient.parseDateTime(date, time).map { dt =>
          val booking = Option(block.selectFirst("a.gooutButton[href]")).map(_.attr("href")).filter(_.nonEmpty)
          RawSlot(slug.getOrElse(t), t, genres, dt, room, booking, poster)
        }
      }
    }
  }

  private def abs(u: String): String =
    if (u.startsWith("http")) u
    else if (u.startsWith("//")) s"https:$u"
    else s"$BaseUrl/${u.stripPrefix("/")}"
}

object AmondoClient {

  private val TimePat = """(\d{1,2}):(\d{2})""".r

  def parseDateTime(date: String, time: String): Option[LocalDateTime] =
    TimePat.findFirstMatchIn(time).flatMap(m => Try(LocalDateTime.parse(s"${date}T${pad(m.group(1))}:${m.group(2)}:00")).toOption)

  private def pad(s: String): String = if (s.length == 1) s"0$s" else s

  final case class Detail(runtimeMinutes: Option[Int], year: Option[Int], countries: Seq[String],
                          director: Seq[String], synopsis: Option[String])
  object Detail { val empty: Detail = Detail(None, None, Seq.empty, Seq.empty, None) }

  private def infoLi(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    doc.select("ul.movie-info li").asScala.find(_.text.toLowerCase.contains(label))
      .map(_.text.replaceFirst(s"(?i)^[^A-Za-zÀ-ž0-9]*$label[:\\s]*", "").trim).filter(_.nonEmpty)

  def parseDetail(html: String): Detail = {
    val doc  = Jsoup.parse(html)
    val prod = infoLi(doc, "produkcja")
    val year = prod.flatMap(s => """\b((?:19|20)\d{2})\b""".r.findFirstMatchIn(s).map(_.group(1).toInt))
    val countries = prod.map(s => s.replaceAll("""\b(?:19|20)\d{2}\b.*$""", "").trim.stripSuffix(","))
                      .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    Detail(
      runtimeMinutes = infoLi(doc, "czas trwania").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year           = year,
      countries      = countries,
      director       = infoLi(doc, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      synopsis       = Option(doc.selectFirst("div.filmPosterSection__plot")).map(_.text.trim).filter(_.length > 20)
    )
  }
}
