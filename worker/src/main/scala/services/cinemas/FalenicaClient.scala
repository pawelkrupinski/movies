package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KINOkawiarnia Stacja Falenica (Warszawa). The `/repertuar/` page lists each
 * film (title, runtime + director, poster) linking to `/filmy/<slug>/`, whose
 * "Dostępne terminy" table holds the screenings (absolute DD.MM.YYYY date +
 * time + a systembiletowy booking link) and the full synopsis.
 */
class FalenicaClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = StacjaFalenica

  private val BaseUrl    = "https://stacjafalenica.pl"
  private val ListingUrl = s"$BaseUrl/repertuar/"
  private val SlugPat    = """/filmy/([^/"]+)/""".r
  private val DateFmt    = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  private case class Film(slug: String, title: String, runtime: Option[Int], director: Seq[String], poster: Option[String])

  def fetch(): Seq[CinemaMovie] = {
    val films = Jsoup.parse(http.get(ListingUrl)).select("article.filmy").asScala.toSeq.flatMap(parseListItem)
      .filterNot(_.slug.contains("__trashed")).distinctBy(_.slug)

    val pages = ParallelDetailFetch.keyed("falenica-details", films.map(_.slug), 1.minute)(s => s"$BaseUrl/filmy/$s/") { url =>
      Try(http.get(url)).toOption.map(Jsoup.parse)
    }

    films.flatMap { f =>
      val detail    = pages.getOrElse(f.slug, None)
      val showtimes = detail.toSeq.flatMap(parseShowtimes).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = f.title, runtimeMinutes = f.runtime, releaseYear = None),
        cinema    = cinema,
        posterUrl = f.poster,
        filmUrl   = Some(s"$BaseUrl/filmy/${f.slug}/"),
        synopsis  = detail.flatMap(d => Option(d.selectFirst("div.section.tresc"))).map(_.text.trim).filter(_.length > 20),
        cast      = Seq.empty,
        director  = f.director,
        showtimes = showtimes,
        // The detail page's WordPress `[video]` block holds the YouTube
        // trailer as `<source type="video/youtube" src="…watch?v=…">`.
        trailerUrl = detail.toSeq.flatMap(d => d.select("video source[src], iframe[src]").asScala)
                       .map(_.attr("src")).filter(_.nonEmpty).flatMap(ScraperParse.canonicalTrailer).headOption
      ))
    }
  }

  private def parseListItem(art: Element): Option[Film] =
    Option(art.selectFirst("h2.repe_title a")).flatMap { a =>
      val title = a.text.trim
      SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1)).filter(_ => title.nonEmpty).map { slug =>
        val czas     = Option(art.selectFirst("div.repe_czas")).map(_.text.trim).getOrElse("")
        val runtime  = """(\d+)\s*min""".r.findFirstMatchIn(czas).map(_.group(1).toInt)
        val director = """(?i)reż\.\s*(.+)$""".r.findFirstMatchIn(czas).map(_.group(1).trim)
                         .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
        val poster   = Option(art.selectFirst("div.repe_outer")).map(_.attr("style"))
                         .flatMap(ScraperParse.cssUrl)
                         .map(u => if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}")
        Film(slug, title, runtime, director, poster)
      }
    }

  private def parseShowtimes(doc: org.jsoup.nodes.Document): Seq[Showtime] =
    doc.select("div.terminy_list > div.row").asScala.toSeq.flatMap { row =>
      val dets = row.select("div.term_det").asScala.toSeq.map(_.text.trim)
      val date = dets.flatMap(t => Try(java.time.LocalDate.parse(t, DateFmt)).toOption).headOption
      val time = dets.flatMap(ScraperParse.parseHHmm).headOption
      val booking = Option(row.selectFirst("a.green_but[href]")).map(_.attr("href")).filter(_.nonEmpty)
      for { d <- date; t <- time } yield Showtime(d.atTime(t), booking, None, Nil)
    }
}
