package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Client for cinemas on the CURRENT bilety24.pl platform, whose venue pages now
 * live on the main domain at `www.bilety24.pl/kino/organizator/<slug>-<id>`
 * (the per-venue `*.bilety24.pl` subdomains the legacy [[Bilety24Client]] reads
 * are being decommissioned — Kino Kosmos, Kino Światowid and Kino Elektronik
 * had their subdomains go to a dead host, which showed as red `ConnectException`
 * bars on /uptime).
 *
 * The organizer page is server-rendered and lists every screening as an
 * anchor whose `title` attribute carries the whole slot:
 *   `<a href="/kino/<id>-<slug>-<eventid>?id=N"
 *       title="Film: <Title> - YYYY-MM-DD HH:MM - <City>">`
 * One fetch yields the full programme — no per-event pages. We read the title
 * attribute (the same `Film: … - date time - city` encoding the legacy client
 * read off its buy buttons), pair each slot's booking link, and group by title.
 *
 * One instance per venue, captured by its `organizerUrl` + `cinema`, so adding
 * a bilety24-hosted cinema is a catalog line, not a new client (OCP).
 */
class Bilety24OrganizerClient(http: HttpFetch, organizerUrl: String, override val cinema: Cinema)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(organizerUrl)

  def fetch(): Seq[CinemaMovie] =
    Bilety24OrganizerClient.parse(http.get(organizerUrl), cinema)
}

object Bilety24OrganizerClient {

  val BaseUrl = "https://www.bilety24.pl"

  // title="Film: <Title> - 2026-06-19 18:50 - Katowice" — the title may itself
  // contain " - ", so the non-greedy capture stops at the first " - <ISO date>".
  private val SlotPat = """(?s)Film:\s*(.+?)\s*-\s*(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2})\s*-""".r
  private val Fmt     = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String])


  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, BaseUrl)

    val slots = doc.select("a[title]").asScala.toSeq.flatMap { a =>
      SlotPat.findFirstMatchIn(a.attr("title")).flatMap { m =>
        Try(LocalDateTime.parse(s"${m.group(2)} ${m.group(3)}", Fmt)).toOption.map { dt =>
          RawSlot(ScraperParse.stripFormatTags(m.group(1)), dt, Option(a.attr("abs:href")).filter(_.nonEmpty))
        }
      }
    }

    slots.filter(_.title.nonEmpty).groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}
