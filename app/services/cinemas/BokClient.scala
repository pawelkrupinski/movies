package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cinemas run by Białołęcki Ośrodek Kultury — Kino na Boku and Kino Głębocka 66
 * — share an identical site under `bok.waw.pl/<prefix>`. The listing page links
 * to each film's detail page (`/<prefix>/<slug>`), which carries the film's
 * whole schedule as `movieshow-list` blocks (date `DD.MM`, time, biletyna
 * booking link) plus director / countries / runtime / synopsis. Parameterised
 * by the slug prefix + cinema so one client serves both; `today` supplies the
 * year the listing omits.
 */
class BokClient(http: HttpFetch, prefix: String, override val cinema: Cinema,
                today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper {

  private val BaseUrl = "https://bok.waw.pl"
  private val SlugPat = s"""href="/$prefix/([a-z0-9-]+)"""".r

  def fetch(): Seq[CinemaMovie] = {
    val listing = http.get(s"$BaseUrl/$prefix")
    val slugs   = SlugPat.findAllMatchIn(listing).map(_.group(1)).toSeq.distinct

    val pages = slugs.map(s => s -> http.getAsync(s"$BaseUrl/$prefix/$s"))
    pages.flatMap { case (slug, f) =>
      Try(f.join()).toOption.flatMap(html => parseFilm(html, slug))
    }
  }

  private def parseFilm(html: String, slug: String): Option[CinemaMovie] = {
    val doc       = Jsoup.parse(html)
    // Titles carry a trailing promo tag ("Drzewo magii | PREMIERA"); drop it so
    // the film merges with — and enriches off — its clean title.
    val title     = Option(doc.selectFirst("h2")).map(_.text.trim.split("\\s+\\|\\s+").head.trim).filter(_.nonEmpty)
    val showtimes = doc.select("div.movieshow-list.p-relative").asScala.toSeq.flatMap(showtimeOf)
                       .distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)

    title.filter(_ => showtimes.nonEmpty).map { t =>
      val director = metaRow(doc, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      val countries = metaRow(doc, "produkcja").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      val runtime  = metaRow(doc, "czas trwania").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt))
      CinemaMovie(
        movie     = Movie(title = t, runtimeMinutes = runtime, releaseYear = None, countries = countries),
        cinema    = cinema,
        posterUrl = Option(doc.selectFirst("div.item-image-thumb img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                      .orElse(Option(doc.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty)),
        filmUrl   = Some(s"$BaseUrl/$prefix/$slug"),
        synopsis  = Option(doc.selectFirst("meta[name=description]")).map(_.attr("content").trim).filter(_.length > 20),
        cast      = Seq.empty,
        director  = director,
        showtimes = showtimes
      )
    }
  }

  private def showtimeOf(block: Element): Option[Showtime] = {
    val date = Option(block.selectFirst("div.fs-24.fw-black")).map(_.text.trim)
    val time = Option(block.selectFirst("div.fs-16.fw-black")).map(_.text.trim)
    for {
      d  <- date.flatMap(BokClient.parseDate(_, today))
      t  <- time.flatMap(ScraperParse.parseHHmm)
    } yield {
      val booking = Option(block.selectFirst("a[href*=biletyna]")).map(_.attr("href")).filter(_.nonEmpty)
      Showtime(d.atTime(t), booking, None, Nil)
    }
  }

  private def metaRow(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    doc.select("div.meta-row").asScala.find(_.text.toLowerCase.contains(label))
      .flatMap(r => Option(r.selectFirst("div.body"))).map(_.text.trim).filter(_.nonEmpty)
}

object BokClient {
  private val DatePat = """(\d{1,2})\.(\d{1,2})""".r

  /** "05.06" (DD.MM, no year) → a date; year from `today`, rolling forward when
   *  the month is already behind us. */
  def parseDate(raw: String, today: LocalDate): Option[LocalDate] =
    DatePat.findFirstMatchIn(raw).flatMap { m =>
      val mon  = m.group(2).toInt
      val year = if (mon < today.getMonthValue) today.getYear + 1 else today.getYear
      Try(LocalDate.of(year, mon, m.group(1).toInt)).toOption
    }
}
