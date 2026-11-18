package services.cinemas.us

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element, TextNode}
import services.cinemas.common.{CinemaScraper, OnlyMovieEventsFilter, ScraperParse}
import tools.{HttpFetch, HttpRead, ReadOutcome}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * A house of Mitchell Theatres (mitchelltheatres.com), the Kansas / Oklahoma / Texas / New
 * Mexico circuit whose every venue runs its own site on one template — `www.southgate6.com`,
 * `www.sequoyah9.com`, … — each selling through its own `tickets.<site>` host.
 *
 * Here because flicks.us files one house's programme under another's name: since at least
 * 2026-11-16 its page for Southgate Cinema 6 (Liberal, KS) lists Sequoyah Cinema 9's
 * (Garden City, KS) sessions, every showtime booked on `tickets.sequoyah9.com`, while
 * southgate6.com sells its own on `tickets.southgate6.com` (DuplicateVenueListing, us).
 *
 * The site's home page (`/index`) is server-rendered and carries the whole programme on sale:
 * one block per film, every day it is booked, every time a link to its own session —
 *
 * {{{
 * <div data-title="Ebenezer" class="veeziMovie GQ165594" data-opening="2026-11-13">
 *   <img class="moviePoster" src="https://indy-systems.imgix.net/…">
 *   <div class="reveal-modal"> … <strong>Plot Summary: </strong>An elderly miser …<br>
 *     <a href="https://www.youtube.com/watch?v=…">Watch the Trailer …</a> </div>
 *   <table class="showtimes">
 *     <tr class="movieTitleRow"><td class="movietitle">Ebenezer
 *       <img class="ratings" title="MPAA Rating: PG-13"> 1h 55m</td></tr>
 *     <tr class="st" data-date="2026-11-21"><td class="dates">…</td><td class="times">
 *       <span class="showtime aud5 matin"><a href="https://tickets.southgate6.com/checkout/showing/4327522/">1:25pm</a></span> …
 * }}}
 *
 * Its "coming soon" page lists release dates only, no sessions, so the home page is the whole
 * published horizon. A day's times are the house's own wall clock; days before `today` are
 * dropped, today's earlier showings kept as every client keeps them.
 *
 * @param siteUrl the house's origin, no trailing slash (`https://www.southgate6.com`).
 */
class MitchellTheatresClient(
  http:    HttpFetch,
  siteUrl: String,
  override val cinema: Cinema,
  today:   => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import MitchellTheatresClient._

  private val pageUrl = s"$siteUrl/index"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(siteUrl)
  override def sourceUrl: Option[String] = Some(pageUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    films(HttpRead.html(http, pageUrl, Marker)(body => ReadOutcome.Answered(Jsoup.parse(body, pageUrl))).required,
      cinema, from = today)
}

object MitchellTheatresClient {

  /** The programme's container on every house's home page; a page without it is not one
   *  (or the template changed). */
  val Marker: HttpRead.PageMarker = HttpRead.PageMarker("showtimespage")

  private val ShowtimeLabel = raw"""(?i)^${ScraperParse.ClockParts}\s*([ap])\.?m\.?$$""".r

  /** "7:05pm" / "1:25pm" / "12:10am" → the time of day; anything else is not a showtime. */
  def timeOf(label: String): Option[java.time.LocalTime] =
    ShowtimeLabel.findFirstMatchIn(label.trim).flatMap(ScraperParse.meridiemClockAt(_, 1, 3))

  /** The home page's film blocks → one listing per film with a showing on `from` or later. */
  def films(page: Document, cinema: Cinema, from: LocalDate): Seq[CinemaMovie] =
    page.select("div.showtimespage div.veeziMovie[data-title]").asScala.toSeq.flatMap { block =>
      val showtimes = showtimesOf(block).filterNot(_.dateTime.toLocalDate.isBefore(from))
      Option.when(showtimes.nonEmpty)(CinemaMovie(
        movie     = Movie(title = block.attr("data-title").trim, runtimeMinutes = runtimeOf(block)),
        cinema    = cinema,
        posterUrl = Option(block.selectFirst("img.moviePoster")).flatMap(ScraperParse.imageUrl(_)),
        filmUrl   = None,
        synopsis  = labelled(block, "Plot Summary:"),
        cast      = Nil,
        director  = Nil,
        showtimes = showtimes,
        trailerUrl = Option(block.selectFirst(".reveal-modal a[href*=youtube.com/watch]")).map(_.absUrl("href")).filter(_.nonEmpty),
        ageRating = Option(block.selectFirst("td.movietitle img.ratings")).map(_.attr("title").stripPrefix("MPAA Rating:").trim)
          .filter(_.nonEmpty)
      )).filter(_.movie.title.nonEmpty)
    }

  private def showtimesOf(block: Element): Seq[Showtime] =
    block.select("tr.st[data-date]").asScala.toSeq.flatMap { row =>
      scala.util.Try(LocalDate.parse(row.attr("data-date"))).toOption.toSeq.flatMap { day =>
        row.select("td.times span.showtime a").asScala.toSeq.flatMap { link =>
          timeOf(link.text).map(time => Showtime(
            dateTime   = LocalDateTime.of(day, time),
            bookingUrl = Option(link.absUrl("href")).filter(_.nonEmpty),
            room       = None,
            format     = Nil
          ))
        }
      }
    }.distinct.sortBy(_.dateTime)

  /** "1h 55m" off the title row. */
  private def runtimeOf(block: Element): Option[Int] =
    Option(block.selectFirst("td.movietitle span")).flatMap(span => ScraperParse.hoursMinutesRuntime(span.text))

  /** The text after a `<strong>label</strong>` in the film's info panel, up to its `<br>`. */
  private def labelled(block: Element, label: String): Option[String] =
    block.select(".reveal-modal strong").asScala.find(_.text.trim == label)
      .flatMap(strong => Option(strong.nextSibling)).collect { case t: TextNode => t.text.trim }.filter(_.nonEmpty)
}
