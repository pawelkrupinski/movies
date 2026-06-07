package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Służewski Dom Kultury cinema (Warszawa). The Joomla "kino" category page is
 * paged via `?start=N`; each item carries the screening's title, absolute
 * date+time and poster, linking to an event page with the auditorium, biletyna
 * booking link, director / countries / year / runtime and synopsis.
 */
class SdkClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = SluzewskiDomKultury

  private val BaseUrl    = "https://sdk.waw.pl"
  private val ListingUrl = s"$BaseUrl/wydarzenia/rodzaj-wydarzenia/kino"
  private val PageSize   = 11
  private val DateTimePat = """(\d{2}\.\d{2}\.\d{4})\s*/\s*\S+\s*/\s*(\d{1,2})\.(\d{2})""".r

  private case class Item(title: String, dateTime: LocalDateTime, detailUrl: String, poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val items = listingPages().flatMap(parseListPage)

    val details = ParallelDetailFetch("sdk-details", items.map(_.detailUrl).distinct, 1.minute) { url =>
      Try(http.get(url)).toOption.map(Jsoup.parse)
    }

    items.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val primary = group.head
      val d       = details.getOrElse(primary.detailUrl, None)
      val showtimes = group.map { it =>
        val pd = details.getOrElse(it.detailUrl, None)
        Showtime(it.dateTime, pd.flatMap(booking), pd.flatMap(room), Nil)
      }.distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = title, runtimeMinutes = d.flatMap(metaLi(_, "czas trwania").flatMap(intIn)),
                          releaseYear = d.flatMap(metaLi(_, "rok produkcji").flatMap(yearIn)),
                          countries = d.toSeq.flatMap(metaLi(_, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)))),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = Some(primary.detailUrl),
        synopsis  = d.flatMap(x => Option(x.selectFirst("div.item-fulltext")).orElse(Option(x.selectFirst("div.com-content-article__body"))))
                      .map(_.text.trim).filter(_.length > 20),
        cast      = Seq.empty,
        director  = d.toSeq.flatMap(metaLi(_, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))),
        showtimes = showtimes
      ))
    }
  }

  private def listingPages(): Seq[String] = {
    val buf = scala.collection.mutable.ListBuffer[String]()
    var start = 0
    var more  = true
    while (more && start <= 200) {
      val html  = Try(http.get(s"$ListingUrl?start=$start")).getOrElse("")
      val items = parseListPage(html)
      if (items.isEmpty) more = false
      else { buf += html; more = items.size >= PageSize; start += PageSize }
    }
    buf.toSeq
  }

  private def parseListPage(html: String): Seq[Item] =
    Jsoup.parse(html).select("div.item-content").asScala.toSeq.flatMap { el =>
      val link = Option(el.selectFirst("a[href*=/wydarzenia/rodzaj-wydarzenia/kino/]"))
      link.flatMap { a =>
        val title = a.attr("title").trim.replaceFirst("(?i)\\s*/\\s*(kino|film|z cyklu).*$", "").replaceAll("[„”\"“]", "").trim
        val dt    = DateTimePat.findFirstMatchIn(el.text).flatMap { m =>
          Try(java.time.LocalDate.parse(m.group(1), java.time.format.DateTimeFormatter.ofPattern("dd.MM.yyyy"))
            .atTime(m.group(2).toInt, m.group(3).toInt)).toOption
        }
        val poster = Option(el.selectFirst("figure.item-image img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                       .map(u => if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}")
        for { t <- Option(title).filter(_.nonEmpty); d <- dt } yield {
          val href = a.attr("href")
          Item(t, d, if (href.startsWith("http")) href else s"$BaseUrl/${href.stripPrefix("/")}", poster)
        }
      }
    }

  private def booking(doc: org.jsoup.nodes.Document): Option[String] =
    Option(doc.selectFirst("a[href*=biletyna]")).map(_.attr("href")).filter(_.nonEmpty)

  private def room(doc: org.jsoup.nodes.Document): Option[String] =
    doc.select("dl.event-details dd").asScala.find(_.selectFirst("span.text-muted") != null)
      .flatMap(d => Option(d.selectFirst("span.text-muted"))).map(_.text.trim).filter(_.nonEmpty)

  private def metaLi(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    doc.select("li").asScala.find(_.text.toLowerCase.startsWith(label))
      .map(_.text.replaceFirst(s"(?i)^$label[:\\s]*", "").trim).filter(_.nonEmpty)

  private def intIn(s: String): Option[Int]  = """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)
  private def yearIn(s: String): Option[Int] = """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)
}
