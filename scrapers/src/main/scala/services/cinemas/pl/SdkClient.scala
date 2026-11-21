package services.cinemas.pl

import java.time.LocalDateTime
import java.util.Locale

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.*
import org.jsoup.Jsoup
import services.cinemas.common.ScraperParse
import services.cinemas.common.SlotsToMovies
import services.cinemas.common.SynopsisProse
import services.cinemas.common.{SingleSiteScraper, ListingPages}
import tools.{HttpFetch, HttpRead}

/**
 * Służewski Dom Kultury cinema (Warszawa). The Joomla "kino" category page is
 * paged via `?start=N`; each item carries the screening's title, absolute
 * date+time and poster, linking to an event page with the auditorium, biletyna
 * booking link, director / countries / year / runtime and synopsis.
 */
class SdkClient(http: HttpFetch) extends SingleSiteScraper {

  val cinema: Cinema = SluzewskiDomKultury

  private val BaseUrl    = "https://sdk.waw.pl"
  private val ListingUrl = s"$BaseUrl/wydarzenia/rodzaj-wydarzenia/kino"
  private val PageSize   = 11
  private val DateTimePat = raw"""(\d{2}\.\d{2}\.\d{4})\s*/\s*\S+\s*/\s*${ScraperParse.ClockPartsDotted}""".r

  private case class Item(title: String, dateTime: LocalDateTime, detailUrl: String, poster: Option[String])

  protected def siteUrl: String = BaseUrl

  def fetch(): Seq[CinemaMovie] = {
    val items = listingPages().flatMap(parseListPage)

    val details = ListingPages.readMore("sdk-details", items.map(_.detailUrl).distinct, identity[String], timeout = 1.minute) { url =>
      Option(HttpRead.page(http, url)).map(Jsoup.parse)
    }.toMap

    def showtimeOf(item: Item) = {
      val page = details.getOrElse(item.detailUrl, None)
      Showtime(item.dateTime, page.flatMap(booking), page.flatMap(room), Nil)
    }
    SlotsToMovies.fold(items, _.title, showtimeOf, order = SlotsToMovies.MovieOrder.asGrouped) { (title, group, showtimes) =>
      val primary = group.head
      val d       = details.getOrElse(primary.detailUrl, None)
      CinemaMovie(
        movie     = Movie(title = title, runtimeMinutes = d.flatMap(metaLi(_, "czas trwania").flatMap(intIn)),
                          releaseYear = d.flatMap(metaLi(_, "rok produkcji").flatMap(yearIn)),
                          countries = d.toSeq.flatMap(metaLi(_, "kraj produkcji").toSeq.flatMap(ScraperParse.commaItems))),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = Some(primary.detailUrl),
        synopsis  = d.flatMap(x => Option(x.selectFirst("div.item-fulltext")).orElse(Option(x.selectFirst("div.com-content-article__body"))))
                      .map(SynopsisProse.blockText(_).trim).filter(_.length > 20),
        director  = d.toSeq.flatMap(metaLi(_, "reżyseria").toSeq.flatMap(ScraperParse.commaItems)),
        showtimes = showtimes
      )
    }
  }

  private def listingPages(): Seq[String] = {
    val buf = scala.collection.mutable.ListBuffer[String]()
    var start = 0
    var more  = true
    val attempts = Seq.newBuilder[Try[String]]
    while (more && start <= 200) {
      val page  = Try(HttpRead.page(http, s"$ListingUrl?start=$start"))
      attempts += page
      val html  = page.getOrElse("")
      val items = parseListPage(html)
      if (items.isEmpty) more = false
      else { buf += html; more = items.size >= PageSize; start += PageSize }
    }
    // A later page failing ends the walk with what was read; the FIRST failing means
    // the listing is down, which must fail the scrape rather than read as empty.
    ListingPages.requireAnyReached(attempts.result())
    buf.toSeq
  }

  private def parseListPage(html: String): Seq[Item] =
    for {
      element <- Jsoup.parse(html).select("div.item-content").asScala.toSeq
      a       <- Option(element.selectFirst("a[href*=/wydarzenia/rodzaj-wydarzenia/kino/]"))
      title   <- Some(a.attr("title").trim.replaceFirst("(?i)\\s*/\\s*(kino|film|z cyklu).*$", "").replaceAll("[„”\"“]", "").trim)
                   .filter(_.nonEmpty)
      m       <- DateTimePat.findFirstMatchIn(element.text)
      day     <- ScraperParse.parseDate(m.group(1))
      time    <- ScraperParse.clockAt(m, 2)
    } yield {
      val poster = Option(element.selectFirst("figure.item-image img[src]")).flatMap(ScraperParse.largestImageSource)
        .map(ScraperParse.onSite(BaseUrl))
      Item(title, day.atTime(time), ScraperParse.onSite(BaseUrl)(a.attr("href")), poster)
    }

  private def booking(document: org.jsoup.nodes.Document): Option[String] =
    Option(document.selectFirst("a[href*=biletyna]")).map(_.attr("href")).filter(_.nonEmpty)

  private def room(document: org.jsoup.nodes.Document): Option[String] =
    document.select("dl.event-details dd").asScala.find(_.selectFirst("span.text-muted") != null)
      .flatMap(d => Option(d.selectFirst("span.text-muted"))).map(_.text.trim).filter(_.nonEmpty)

  private def metaLi(document: org.jsoup.nodes.Document, label: String): Option[String] =
    document.select("li").asScala.find(_.text.toLowerCase(Locale.ROOT).startsWith(label))
      .map(_.text.replaceFirst(s"(?i)^$label[:\\s]*", "").trim).filter(_.nonEmpty)

  private def intIn(s: String): Option[Int]  = """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)
  private def yearIn(s: String): Option[Int] = """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)
}
