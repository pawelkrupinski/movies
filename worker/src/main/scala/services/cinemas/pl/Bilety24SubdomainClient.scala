package services.cinemas.pl

import tools.HttpFetch
import models._
import services.cinemas.common.{CinemaScraper, ScrapeHorizon}
import services.movies.TitleNormalizer

import java.time.{LocalDate, ZoneId}
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
 * walk forward from today for as long as the programme lasts, fetch each
 * `?b24_day=` page, and union the results. These venues screen on scattered days
 * — Kino Astra ran four dates across a month when measured on 2026-08-05, the
 * last of them 25 days out and three weeks past the fixed window this used to
 * ask for — so the walk has to follow the programme, not a guess at its length.
 * See [[ScrapeHorizon.liveDays]]. The per-day HTML carries the SAME
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
  today:        LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")),
  titles:       TitleNormalizer
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(repertuarUrl)
  override def sourceUrl: Option[String] = Some(repertuarUrl)

  def fetch(): Seq[CinemaMovie] = {
    val sep = if (repertuarUrl.contains("?")) "&" else "?"

    // One `?b24_day=` page per date, read tolerantly: a failed or unparseable day
    // yields no films rather than killing the walk. Each day is fetched once —
    // the walk parses a day to learn whether the programme goes on, so the
    // results are kept rather than asked for twice.
    val byDate = scala.collection.mutable.LinkedHashMap.empty[LocalDate, Seq[CinemaMovie]]
    ScrapeHorizon.liveDays(today) { d =>
      byDate.getOrElseUpdate(d,
        Try(http.get(s"$repertuarUrl${sep}b24_day=$d")).toOption.toSeq
          .flatMap(html => Bilety24OrganizerClient.parse(html, cinema, titles))).nonEmpty
    }

    // Merge across days: the same film recurs on several dates, so union each
    // title's showtimes (deduped, sorted) into a single row.
    byDate.values.toSeq.flatten
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
