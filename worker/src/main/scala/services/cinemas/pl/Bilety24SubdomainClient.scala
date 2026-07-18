package services.cinemas.pl

import tools.{HttpFetch, ParallelDetailFetch}
import models._
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, ZoneId}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Client for the LEGACY per-venue bilety24.pl SUBDOMAINS (e.g.
 * `kulturalne-oborniki.bilety24.pl`) that are still alive but whose
 * `/repertuar/` page only renders ONE day at a time: a WordPress
 * `b24-ajax-list` widget lazy-loads each day behind a `?b24_day=YYYY-MM-DD`
 * query param (the day links are plain `<a href="?b24_day=…">`, server-rendered
 * — no JavaScript needed, the param works on a direct GET).
 *
 * This is the counterpart to [[Bilety24OrganizerClient]], which reads the
 * CURRENT main-domain `www.bilety24.pl/kino/organizator/<slug>-<id>` pages that
 * render a venue's whole programme in one shot. The subdomain pages can't be
 * read that way (the base `/repertuar/` only shows the nearest day), so here we
 * walk `today … today+daysAhead`, fetch each `?b24_day=` page, and union the
 * results. The per-day HTML carries the SAME
 * `title="Film: <Title> - <YYYY-MM-DD HH:MM> - <city>"` anchor encoding, so we
 * reuse [[Bilety24OrganizerClient.parse]] and merge showtimes by title.
 *
 * One instance per venue (its `repertuarUrl` + `cinema`), so adding another
 * surviving-subdomain venue is a catalog line, not a new client (OCP).
 */
class Bilety24SubdomainClient(
  http:         HttpFetch,
  repertuarUrl: String,
  override val cinema: Cinema,
  daysAhead:    Int       = 9,
  today:        LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(repertuarUrl)
  override def sourceUrl: Option[String] = Some(repertuarUrl)

  def fetch(): Seq[CinemaMovie] = {
    val dates = (0 to daysAhead).map(today.plusDays(_))
    val sep   = if (repertuarUrl.contains("?")) "&" else "?"

    // One `?b24_day=` page per date, fetched tolerantly in parallel: a failed or
    // unparseable day yields no films rather than killing the batch.
    val byDate = ParallelDetailFetch.keyed(
      "bilety24-subdomain-day", dates, 1.minute, maxConcurrent = 2
    )(d => s"$repertuarUrl${sep}b24_day=$d") { url =>
      Try(http.get(url)).toOption.toSeq.flatMap(html => Bilety24OrganizerClient.parse(html, cinema))
    }

    // Merge across days: the same film recurs on several dates, so union each
    // title's showtimes (deduped, sorted) into a single row.
    dates.flatMap(d => byDate.getOrElse(d, Seq.empty))
      .groupBy(_.movie.title).toSeq
      .flatMap { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes)
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        group.headOption.filter(_ => showtimes.nonEmpty).map(_.copy(showtimes = showtimes))
      }
      .sortBy(_.movie.title)
  }
}
