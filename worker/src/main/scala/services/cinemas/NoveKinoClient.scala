package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cinemas in the Nove Kino chain (e.g. Atlantic, Warszawa). Each cinema's
 * `repertuar.php` shows one day; the day picker links to `?data=YYYY-MM-DD` for
 * the rest. Every showtime anchor carries its own absolute `data-day` + time +
 * buy link, so the schedule is read straight off the listing pages; the
 * `film.php?id=` page adds director / year / countries / genres / synopsis
 * (runtime isn't published anywhere — TMDB supplies it). Parameterised by the
 * cinema's URL slug so one client serves any Nove Kino venue.
 */
class NoveKinoClient(http: HttpFetch, slug: String, override val cinema: Cinema) extends CinemaScraper {

  private val BaseUrl    = "https://www.novekino.pl"
  private val CinemaUrl  = s"$BaseUrl/kina/$slug"
  private val DatePat    = """data=(\d{4}-\d{2}-\d{2})""".r
  private val FilmIdPat  = """film\.php\?id=(\d+)""".r

  private case class RawSlot(filmId: String, title: String, dateTime: LocalDateTime, booking: Option[String],
                             poster: Option[String], countries: Seq[String], genres: Seq[String], format: List[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val today = http.get(s"$CinemaUrl/repertuar.php")
    val dates = DatePat.findAllMatchIn(today).map(_.group(1)).toSeq.distinct
    val dayPages = ParallelDetailFetch.keyed("nove-kino-days", dates, 1.minute, maxConcurrent = 1)(d => s"$CinemaUrl/repertuar.php?data=$d") { url =>
      Try(http.get(url)).toOption
    }
    val htmls = today +: dates.flatMap(d => dayPages.getOrElse(d, None))

    val slots = htmls.flatMap(parsePage)
    // The same film is listed once per presentation variant ("- napisy" /
    // "- dubbing"); group by the cleaned title so it's one row, the variant
    // captured as the showtime format.
    val byTitle = slots.groupBy(_.title)

    val details = ParallelDetailFetch.keyed("nove-kino-details", byTitle.values.map(_.head.filmId).toSeq.distinct, 1.minute)(id => s"$CinemaUrl/film.php?id=$id") { url =>
      Try(http.get(url)).toOption.map(NoveKinoClient.parseDetail).getOrElse(NoveKinoClient.Detail.empty)
    }

    byTitle.toSeq.flatMap { case (_, group) =>
      val primary    = group.head
      val filmId     = primary.filmId
      val showtimes  = group.map(s => Showtime(s.dateTime, s.booking, None, s.format))
                         .distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val d = details.getOrElse(filmId, NoveKinoClient.Detail.empty)
        Some(CinemaMovie(
          movie     = Movie(
            title          = primary.title,
            runtimeMinutes = None,
            releaseYear    = d.year,
            countries      = if (primary.countries.nonEmpty) primary.countries else d.countries,
            genres         = if (primary.genres.nonEmpty) primary.genres else d.genres
          ),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = Some(s"$CinemaUrl/film.php?id=$filmId"),
          synopsis  = d.synopsis,
          cast      = d.cast,
          director  = d.director,
          showtimes = showtimes,
          trailerUrl = d.trailer
        ))
      }
    }
  }

  private def parsePage(html: String): Seq[RawSlot] =
    Jsoup.parse(html).select("tr.repertoire-movie-tr").asScala.toSeq.flatMap { row =>
      val link = Option(row.selectFirst("div.repertoire-movie-title a"))
      val id   = link.flatMap(a => FilmIdPat.findFirstMatchIn(a.attr("href")).map(_.group(1)))
      val parsed = link.map(_.text.trim).filter(_.nonEmpty).map(NoveKinoClient.parseTitle)
      (id, parsed) match {
        case (Some(filmId), Some((t, fmt))) =>
          val poster = Option(row.selectFirst("div.repertoire-movie-poster img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                         .map(u => if (u.startsWith("http")) u else BaseUrl + u)
          val desc   = Option(row.selectFirst("div.repertoire-movie-description")).map(_.text).getOrElse("")
          val countries = NoveKinoClient.after(desc, "produkcja").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
          val genres    = NoveKinoClient.after(desc, "gatunek").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
                            .map(tools.TextNormalization.titleCaseIfAllLower)
          row.select("a.repertoire-movie-time[data-day][data-hour]").asScala.toSeq.flatMap { a =>
            val booking = Option(a.attr("data-buy-link")).filter(_.nonEmpty)
            NoveKinoClient.parseDateTime(a.attr("data-day"), a.attr("data-hour"))
              .map(dt => RawSlot(filmId, t, dt, booking, poster, countries, genres, fmt))
          }
        case _ => Seq.empty
      }
    }
}

object NoveKinoClient {

  private val FormatWord = Map(
    "napisy" -> "NAP", "nap" -> "NAP", "dubbing" -> "DUB", "dub" -> "DUB", "dubb" -> "DUB",
    "lektor" -> "LEK", "2d" -> "2D", "3d" -> "3D", "imax" -> "IMAX", "4dx" -> "4DX"
  )

  /** Split a Nove Kino title into its clean form + format tokens. The site
   *  appends the presentation as a suffix ("Zawodowcy - napisy"); strip it only
   *  when the trailing segment is entirely format words, so real dash-bearing
   *  titles ("Mission - Impossible") stay intact. */
  def parseTitle(raw: String): (String, List[String]) = {
    val i = raw.lastIndexOf(" - ")
    if (i <= 0) (raw, Nil)
    else {
      val tail = raw.substring(i + 3).trim.toLowerCase.split("\\s+").toList.filter(_.nonEmpty)
      if (tail.nonEmpty && tail.forall(FormatWord.contains)) (raw.substring(0, i).trim, tail.flatMap(FormatWord.get).distinct)
      else (raw, Nil)
    }
  }

  def parseDateTime(day: String, hour: String): Option[LocalDateTime] =
    """(\d{1,2}):(\d{2})""".r.findFirstMatchIn(hour).flatMap(m =>
      Try(LocalDateTime.parse(s"${day}T${pad(m.group(1))}:${m.group(2)}:00")).toOption)

  private def pad(s: String): String = if (s.length == 1) s"0$s" else s

  /** Pull the text after a `Label:` token out of the description blob. */
  def after(desc: String, label: String): Option[String] =
    s"(?i)$label:\\s*([^\\n]+?)(?:\\s{2,}|gatunek:|produkcja:|$$)".r.findFirstMatchIn(desc).map(_.group(1).trim).filter(_.nonEmpty)

  final case class Detail(year: Option[Int], countries: Seq[String], genres: Seq[String],
                          director: Seq[String], cast: Seq[String], synopsis: Option[String], trailer: Option[String])
  object Detail { val empty: Detail = Detail(None, Seq.empty, Seq.empty, Seq.empty, Seq.empty, None, None) }

  private def dd(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    ScraperParse.ddField(doc, label)

  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    Detail(
      year      = dd(doc, "rok produkcji").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries = dd(doc, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      genres    = dd(doc, "gatunek").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)).map(tools.TextNormalization.titleCaseIfAllLower),
      director  = dd(doc, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      cast      = dd(doc, "obsada").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      synopsis  = Option(doc.selectFirst("section.text_panel p")).map(_.text.trim).filter(_.length > 20),
      // The film page embeds a single YouTube `/embed/` iframe in its slider.
      trailer   = doc.select("iframe[src]").asScala.iterator.map(_.attr("src")).filter(_.nonEmpty)
                    .flatMap(ScraperParse.canonicalTrailer).nextOption()
    )
  }
}
