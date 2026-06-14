package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kultura (Warszawa, SFP). The page loads its schedule via AJAX: the main
 * page embeds a per-load nonce (`u_time`) and a `rep_posters(nonce, date)` call
 * per day; each day's `rep_posters.php` response lists screenings as
 * `rep_movie(...)` onclicks plus a poster image per film. We read the nonce +
 * date list off the main page (so the fixture replay is deterministic) and pull
 * showtimes from each day's response. Film metadata is left to TMDB enrichment.
 */
class KinoKulturaClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoKultura

  private val BaseUrl    = "https://www.kinokultura.pl"
  private val PostersUrl = s"$BaseUrl/_core/_include/_rep/rep_posters.php"
  private val BookingFmt = (date: String) => s"https://rezerwacja.kinokultura.pl/MSI/mvc/pl?sort=Flow&date=$date&datestart=0"

  private val NoncePat = """rep_posters\('(\d+)'""".r
  private val DatePat  = """rep_posters\('\d+',\s*'(\d{4}-\d{2}-\d{2})'""".r
  // rep_movie('nonce','filmId','hour','min','date','room',...)
  private val MoviePat = """rep_movie\('\d+',\s*'(\d+)','(\d+)','(\d+)','(\d{4}-\d{2}-\d{2})','(\d)'""".r
  private val PosterPat = """foto,(\d+),""".r

  private case class RawSlot(filmId: String, title: String, dateTime: LocalDateTime, room: Option[String],
                             booking: String, poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl, "https://rezerwacja.kinokultura.pl")
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val main  = http.get(s"$BaseUrl/")
    val nonce = NoncePat.findFirstMatchIn(main).map(_.group(1)).getOrElse("")
    val dates = DatePat.findAllMatchIn(main).map(_.group(1)).toSeq.distinct
    if (nonce.isEmpty || dates.isEmpty) return Seq.empty

    val pages = ParallelDetailFetch.keyed("kino-kultura-days", dates, 1.minute)(d => s"$PostersUrl?ajax=1&u_time=$nonce&rep_date=$d") { url =>
      Try(http.get(url)).toOption
    }
    val slots = dates.flatMap(d => pages.getOrElse(d, None).toSeq.flatMap(parseDay))

    slots.groupBy(_.filmId).toSeq.flatMap { case (_, group) =>
      val primary    = group.head
      val showtimes  = group.distinctBy(s => (s.dateTime, s.room)).sortBy(_.dateTime)
                         .map(s => Showtime(s.dateTime, Some(s.booking), s.room, Nil))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title, runtimeMinutes = None, releaseYear = None),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }
  }

  private def parseDay(html: String): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    // film id → (title, poster) from the poster images.
    val byId = document.select("img.poster_photo").asScala.toSeq.flatMap { img =>
      val src = img.attr("src")
      PosterPat.findFirstMatchIn(src).map(_.group(1)).map { id =>
        val poster = Some(if (src.startsWith("http")) src else s"$BaseUrl/${src.stripPrefix("/")}")
        id -> (img.attr("title").trim, poster)
      }
    }.toMap

    MoviePat.findAllMatchIn(html).toSeq.flatMap { m =>
      val filmId = m.group(1)
      val date   = m.group(4)
      byId.get(filmId).filter(_._1.nonEmpty).flatMap { case (title, poster) =>
        Try(LocalDateTime.parse(s"${date}T${pad(m.group(2))}:${pad(m.group(3))}:00")).toOption.map { dt =>
          val room = m.group(5) match { case "0" => Some("Kultura"); case "1" => Some("Rejs"); case _ => None }
          RawSlot(filmId, title, dt, room, BookingFmt(date), poster)
        }
      }
    }
  }

  private def pad(s: String): String = if (s.length == 1) s"0$s" else s
}
