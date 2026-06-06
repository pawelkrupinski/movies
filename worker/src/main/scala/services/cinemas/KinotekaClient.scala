package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kinoteka (Warszawa, PKiN). The `/repertuar/` page is per-day; its date nav
 * links to `?date=YYYY-MM-DD`. Each film article carries genres + a row of
 * screening anchors (each with an absolute `data-day`, time and buy link). The
 * `/film/<slug>/` detail page adds runtime / director / countries / year /
 * original title / synopsis. Dates come from the page's own nav, so the replay
 * is deterministic.
 */
class KinotekaClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = Kinoteka

  private val BaseUrl    = "https://kinoteka.pl"
  private val ListingUrl = s"$BaseUrl/repertuar/"
  private val DatePat    = """[?&]date=(\d{4}-\d{2}-\d{2})""".r
  private val SlugPat    = """/film/([^/"?#]+)""".r

  private case class RawSlot(slug: String, title: String, genres: Seq[String], dateTime: LocalDateTime,
                             booking: Option[String], poster: Option[String])

  def fetch(): Seq[CinemaMovie] = {
    val base  = http.get(ListingUrl)
    val dates = DatePat.findAllMatchIn(base).map(_.group(1)).toSeq.distinct
    val dayPages = ParallelDetailFetch.keyed("kinoteka-days", dates, 1.minute)(d => s"$ListingUrl?date=$d") { url =>
      Try(http.get(url)).toOption
    }
    val slots = dates.flatMap(d => dayPages.getOrElse(d, None).toSeq.flatMap(parsePage))

    val bySlug  = slots.groupBy(_.slug)
    val details = ParallelDetailFetch.keyed("kinoteka-details", bySlug.keys.toSeq.filter(_.nonEmpty), 1.minute)(s => s"$BaseUrl/film/$s/") { url =>
      Try(http.get(url)).toOption.map(KinotekaClient.parseDetail).getOrElse(KinotekaClient.Detail.empty)
    }

    bySlug.toSeq.flatMap { case (slug, group) =>
      val primary    = group.head
      val showtimes  = group.map(s => Showtime(s.dateTime, s.booking, None, Nil))
                         .distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val d = details.getOrElse(slug, KinotekaClient.Detail.empty)
        Some(CinemaMovie(
          movie     = Movie(
            title          = primary.title,
            runtimeMinutes = d.runtime,
            releaseYear    = d.year,
            originalTitle  = d.originalTitle,
            countries      = d.countries,
            genres         = primary.genres
          ),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption.orElse(d.poster),
          filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/film/$slug/") else None,
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

  private def dd(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    ScraperParse.ddField(doc, label, "dl.p-movie-details__general-info dt")

  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    Detail(
      runtime       = dd(doc, "czas trwania").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year          = dd(doc, "data premiery").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      originalTitle = dd(doc, "oryginalny tytuł").filter(_.nonEmpty),
      countries     = dd(doc, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      director      = dd(doc, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      cast          = dd(doc, "obsada").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      synopsis      = Option(doc.selectFirst("div.mce-content-body")).map(_.text.trim)
                        .orElse(Option(doc.selectFirst("meta[property=og:description]")).map(_.attr("content").trim)).filter(_.length > 20),
      poster        = Option(doc.selectFirst("div.p-movie-details__hero-poster img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                        .orElse(Option(doc.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty)),
      // The trailer is a YouTube `/embed/` iframe in the content figure; skip
      // the GTM/analytics iframes by taking the first src that canonicalises.
      trailer       = doc.select("iframe[src]").asScala.iterator.map(_.attr("src")).filter(_.nonEmpty)
                        .flatMap(ScraperParse.canonicalTrailer).nextOption()
    )
  }
}
