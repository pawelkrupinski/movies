package services.cinemas.pl

import services.cinemas.common.ScraperParse
import tools.{HttpFetch, HttpRead}
import models._
import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import services.cinemas.common.{SingleSiteScraper, OnlyMovieEventsFilter, ScrapedSlot, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Kino Amok (Gliwice) — studyjne cinema run by CSW Górnośląskie Centrum Kultury.
 * The repertoire is a server-rendered WordPress page at `/repertuar/`, listing
 * all days for the coming week.
 *
 * Layout:
 *  - Each day is introduced by `div.ribbon.rep-ribbon` containing an `<h3>`
 *    with the Polish date ("7 czerwca") and a weekday name.
 *  - Within that day's `div.post-list`, each screening is a `div.post` with:
 *      - `div.margin-b` → start time (`HH:MM`).
 *      - sibling `div` → room name ("Duża sala" / "Mała sala" / …).
 *      - `div.rep-title a strong` → film title.
 *      - `a.more-link[href=/repertuar/kup-bilet?id=N]` → booking URL.
 *        (Some screenings have no ticket button when sold out or not yet bookable.)
 *
 * Showtimes are scraped directly from the listing; no per-film detail fetch is
 * needed for our surface (title + time + room + booking URL).
 *
 * The year is inferred from `today`: if the "D miesiąca" date would be more
 * than 60 days in the past, it belongs to next year (handles the
 * December → January page-turn).
 */
class KinoAmokClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            => LocalDate
) extends SingleSiteScraper with OnlyMovieEventsFilter {

  import KinoAmokClient._

  protected def siteUrl: String = BaseUrl

  // The repertoire also sells the venue's own evenings — "Siesta – Trylogia Afrykańska | Spotkanie autorskie z Marcinem
  // Kydryńskim i koncert muzyki na żywo" — which the identity resolver then matched to the 1987 film "Sjesta".
  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val html = HttpRead.page(http, RepertoireUrl)
    val document  = Jsoup.parse(html)
    val slots = parseDocument(document, today)

    SlotsToMovies.basic(slots, cinema, distinctBy = SlotsToMovies.byTimeAndRoom)
  }
}

object KinoAmokClient {

  val BaseUrl        = "https://amok.gliwice.pl"
  val RepertoireUrl  = s"$BaseUrl/repertuar/"

  private[cinemas] def parseDocument(document: Document, today: LocalDate): Seq[ScrapedSlot] = {
    // Split the page by ribbon divs (each day section).  We use Jsoup's
    // own select rather than splitting raw HTML.
    val container = document.selectFirst("div.repertoire_list_f")
    if (container == null) return Seq.empty

    ScraperParse.underDayHeaders(container.children.asScala) {
      // Date header — extract "DD miesiąca"
      case ribbon if ribbon.hasClass("ribbon") && ribbon.hasClass("rep-ribbon") =>
        parsePolishDate(Option(ribbon.selectFirst("h3")).map(_.text.trim).getOrElse(""), today)
    } { (child, date) =>
      // Film list for the current date
      if (!child.hasClass("post-list")) Nil
      else child.select("div.post").asScala.flatMap(parsePost(_, date, BaseUrl))
    }
  }

  /** Parse a single screening `div.post` into a `ScrapedSlot`. */
  private def parsePost(post: org.jsoup.nodes.Element, date: LocalDate, baseUrl: String): Option[ScrapedSlot] = {
    val timeText = Option(post.selectFirst("div.margin-b")).map(_.text.trim).getOrElse("")
    val time     = ScraperParse.parseHHmm(timeText)

    // The room sits in the sibling column to the time — the first bare `<div>`
    // after the time cell that has text.
    val room = post.select("div.col-sm-3 div.row div.col-xs-9").asScala.toSeq
      .flatMap(d => Option(d.selectFirst("div")).map(_.text.trim).filter(_.nonEmpty))
      .headOption

    val title = Option(post.selectFirst("div.rep-title a strong")).map(_.text.trim).filter(_.nonEmpty)

    val booking = Option(post.selectFirst("a.more-link[href]"))
      .map(_.attr("href")).filter(_.contains("kup-bilet"))
      .map(href => if (href.startsWith("http")) href else baseUrl + href)

    for {
      t    <- title
      lt   <- time
    } yield ScrapedSlot(t, LocalDateTime.of(date, lt), booking, room = room)
  }

  /** "7 czerwca" / "13 stycznia" → LocalDate.
   *  If the resulting date would be more than 60 days in the past, assume next year. */
  private[cinemas] def parsePolishDate(text: String, today: LocalDate): Option[LocalDate] =
    ScraperParse.parseDayMonth(text).flatMap(ScraperParse.upcomingDate(_, today))
}
