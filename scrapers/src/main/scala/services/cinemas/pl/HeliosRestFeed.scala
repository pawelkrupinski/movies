package services.cinemas.pl

import java.time.LocalDate

import scala.concurrent.duration.*
import scala.util.Try

import services.cinemas.common.{ListingPages, ScrapeHorizon}
import services.cinemas.pl.HeliosRest.*
import tools.{HttpFetch, HttpRead}

/** One Helios venue's REST read ([[HeliosRest]]): its screenings and event screenings over the scrape horizon, then
 *  the film bodies and screen names they name. `detailFetch` carries the per-film and per-screen bodies (the
 *  composition root shares one caching fetch across every Helios location); the volatile listings stay on `http`. */
final class HeliosRestFeed(http: HttpFetch, detailFetch: HttpFetch, sourceId: String) {

  def read(from: LocalDate): RestData = {
    val windows         = HeliosRestFeed.windows(from)
    val regular         = window(windows.map(w => s"${HeliosRestFeed.ApiBase}/cinema/$sourceId/screening?$w"))(parseApiScreenings)
    // `/event` honours the SAME date windows as `/screening`. Without a window the
    // endpoint returns the cinema's ENTIRE event history — ~4400 events, 9 MB,
    // ~99% of them in the past — and the 18-20s spent hauling-then-discarding
    // that payload was the single dominant cost of a Helios scrape (the whole
    // fetch dropped from ~20s to ~1s once windowed). What made it expensive was
    // the PAST, which `dateTimeFrom` excludes, so reaching forward is cheap. We
    // only ever use events whose screenings fall in the windows `/screening`
    // already covers, so the filter loses nothing.
    val eventScreenings = window(windows.map(w => s"${HeliosRestFeed.ApiBase}/cinema/$sourceId/event?$w"))(parseEventScreenings)
    // `/screening` lists regular film screenings; event screenings (anime, concerts,
    // sports broadcasts) are excluded from it but carry the same id→screenId mapping
    // under `/event`. Merge both so room/format enrichment covers events too. The two
    // sets are disjoint; on the (unexpected) collision the regular screening wins.
    val screeningsById  = eventScreenings ++ regular
    RestData(screeningsById, movieDetails(regular, eventScreenings), screenNames(screeningsById))
  }

  // Synchronous `get` (NOT getAsync): the two fetches are already sequential (each
  // `.join()`s immediately), so getAsync bought no concurrency — it just bounced a
  // BLOCKING GET onto the shared ForkJoinPool.commonPool, where ~30 slow Helios
  // venues pegged the pool and floored the worker's CPU credit (2026-07-05). Plain
  // `get` keeps the blocking on the bounded scrape worker (isolated), and the
  // per-host 4s timeout + circuit breaker bound how long each one holds it.
  // A window that fails yields nothing rather than killing the scrape — the near
  // window is what most of the programme lives in, and losing the tail to a blip
  // must not cost us that — but it is reported as a failed page, so the listing is
  // incomplete and nothing is pruned on the strength of it (ListingPages.reportFailed).
  private def window(urls: Seq[String])(parse: String => Map[String, ApiScreening]): Map[String, ApiScreening] = {
    val reads = urls.map(u => Try(parse(HttpRead.page(http, u))))
    ListingPages.reportFailed(reads)
    reads.flatMap(_.toOption).foldLeft(Map.empty[String, ApiScreening])(_ ++ _)
  }

  // A movie body is part of the listing: a REST-only film (a Latin-titled UA screening NUXT
  // lists in Cyrillic) is emitted only from its body, so one that fails can drop a film and
  // leaves the listing incomplete. The film an event names only describes the event the
  // repertoire page already lists: one that fails costs its details, never a row, and leaves
  // the listing complete.
  private def movieDetails(regular: Map[String, ApiScreening], events: Map[String, ApiScreening]): Map[String, ApiMovieInfo] = {
    val filmIds         = filmIdsOf(regular)
    val movieBodies     = ListingPages.readMore("helios-movies", filmIds, movieUrl, timeout = 1.minute)(HttpRead.page(detailFetch, _)).toMap
    val eventFilmBodies = enrichment("helios-event-films", filmIdsOf(events).filterNot(filmIds.contains), movieUrl)
    (movieBodies ++ eventFilmBodies).flatMap { case (id, body) => parseApiMovieBody(body).map(id -> _) }
  }

  // A screen body only names a showtime's room: one that fails drops nothing, and must not keep the venue from pruning.
  private def screenNames(screeningsById: Map[String, ApiScreening]): Map[String, String] =
    enrichment("helios-screens", screeningsById.values.map(_.screenId).toSeq.distinct,
      id => s"${HeliosRestFeed.ApiBase}/cinema/$sourceId/screen/$id")
      .flatMap { case (id, body) => parseScreenName(body).map(id -> _) }

  private def enrichment(label: String, ids: Seq[String], url: String => String): Map[String, String] =
    ListingPages.readEnrichment(label, ids, url, timeout = 1.minute)(HttpRead.page(detailFetch, _))
      .flatMap { case (id, attempt) => attempt.toOption.map(id -> _) }.toMap

  private def filmIdsOf(screenings: Map[String, ApiScreening]): Seq[String] =
    screenings.values.map(_.movieId).filter(_.nonEmpty).toSeq.distinct

  private def movieUrl(id: String): String = s"${HeliosRestFeed.ApiBase}/movie/$id"
}

object HeliosRestFeed {
  val ApiBase = "https://restapi.helios.pl/api"

  // Two windows, not one. The near week is dense (Poznań, measured 2026-08-05:
  // 247 screenings) and the tail is sparse but real (another 81, which a
  // today+6 window never saw at all) — so the first request stays exactly as
  // it was and a second sweeps everything after it, out to the horizon.
  // Splitting keeps the near-term response small, and leaves the recorded
  // fixtures for the near window matching byte-for-byte.
  private def windows(from: LocalDate): Seq[String] = {
    val nearEnd = from.plusDays(6)
    val farEnd  = from.plusDays(ScrapeHorizon.MaxDays.toLong)
    Seq(
      s"dateTimeFrom=${from}T00:00:00&dateTimeTo=${nearEnd}T23:59:59",
      s"dateTimeFrom=${nearEnd.plusDays(1)}T00:00:00&dateTimeTo=${farEnd}T23:59:59"
    )
  }
}
