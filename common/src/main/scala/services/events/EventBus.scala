package services.events

import play.api.Logging
import services.tasks.TaskType

import java.util.concurrent.CopyOnWriteArrayList

/**
 * Domain-level events published by core services. Listeners subscribe by
 * passing a `PartialFunction[DomainEvent, Unit]` to `EventBus.subscribe` — the
 * bus uses `applyOrElse`, so handlers only need to match the cases they care
 * about (no explicit `case _ => ()` fallback). Handlers run synchronously on
 * the publisher's thread; listeners that do real work (network, disk) should
 * hand off to their own executor.
 */
sealed trait DomainEvent

/** A venue detail page was read into venue_pages — its detail, or that it is gone — for the enricher
 *  group `detailGroup` (one page serves every venue of a chain) and `page`, the ref the enrichment
 *  fetched (`DetailEnricher.nativeDetailRef`). Published AFTER the page is stored, so the model's
 *  index of venue_pages (`services.identity.VenuePageIndex`) reads the answer it announces: the
 *  identity model re-asks the listings that read that page. */
case class VenueDetailRead(detailGroup: String, page: String) extends DomainEvent

/** A film's per-cinema details are as complete as they're going to get, so it's
 *  ready to enrich — resolve TMDB (and, downstream of that, the ratings). The
 *  single trigger for the enrichment pipeline. It fires EITHER:
 *    - immediately when a newly-scraped film needs no deferred detail
 *      enrichment (its listing already carries everything we'll get), OR
 *    - once a deferred cinema's per-film detail page has been fetched and
 *      merged (published by `EnrichDetailsHandler`), so the director / original
 *      title / production year the detail page supplies are on the row BEFORE
 *      TMDB resolves — rather than burning a director-less attempt at scrape and
 *      then waiting for the daily sweep (the "stuck TMDB-unresolved" class).
 *
 *  A film that DOES await detail enrichment is deliberately NOT published at
 *  scrape time: `CinemaScrapeRunner` marks it `detailPending` and holds the
 *  trigger until the detail lands. Such a row is also held out of the read
 *  model until then — see `MovieRecord.readyToProject`.
 *
 *  `originalTitle` carries the cinema's English/international title when
 *  available (Multikino exposes one for ~5% of films — Cirque du Soleil,
 *  opera/concert documents, English imports). The TMDB stage uses it as a secondary
 *  search title when the Polish title doesn't resolve.
 *
 *  `director` carries the reported director name(s) (possibly comma-separated
 *  for co-directors). The TMDB stage uses it to *verify* a title-search
 *  candidate — when the candidate's credits don't include the reported
 *  director, the resolver walks the director's TMDB filmography instead. Solves
 *  the same-title-different-film mis-resolution class (Niedźwiedzica → Grizzly
 *  Falls 1999 vs the 2026 film).
 *
 *  Both optional fields default to None so cinemas without the field — and the
 *  unit specs that publish this directly — stay unchanged. */
case class MovieDetailsComplete(
  title:         String,
  year:          Option[Int],
  originalTitle: Option[String] = None,
  director:      Option[String] = None
) extends DomainEvent

object MovieDetailsComplete {
  /** The event for a cached row, with both hints read off its CINEMA slots.
   *
   *  Shared by the two publishers that announce a row whose detail has just
   *  settled (`EnrichDetailsHandler`, `DetailReaper`) so the pair can't drift
   *  apart again: they used to build this inline, taking `cinemaOriginalTitle`
   *  (cinema-only) but plain `director` (which falls back to the derived
   *  `Tmdb`/`Imdb`/`Filmweb` slots). That fed a previous resolution's own output
   *  back in as a hint for re-resolving the same row — see
   *  `MovieRecord.evidence.directors`. */
  def forRow(title: String, year: Option[Int], row: Option[models.MovieRecord]): MovieDetailsComplete =
    MovieDetailsComplete(
      title, year,
      row.flatMap(_.evidence.originalTitle),
      row.map(_.evidence.directors).filter(_.nonEmpty).map(_.mkString(", ")))
}

/** TMDB resolved a `(title, year)` to a film but TMDB had no IMDb cross-
 *  reference for it (common for very recent films and festival items, e.g.
 *  "Mortal Kombat II" 2026). `searchTitle` is the title we want to search
 *  IMDb's suggestion endpoint with — typically TMDB's `originalTitle` or
 *  English release title, whichever is more likely to match IMDb's primary.
 *
 *  `ImdbIdResolver` subscribes to this event, calls `ImdbClient.findId(...)`
 *  to recover the id, and writes it back to the cache — from where the
 *  `EnrichmentReaper` picks up the now-eligible IMDb rating on its next pass. */
case class ImdbIdMissing(title: String, year: Option[Int], searchTitle: String) extends DomainEvent

/** A queue task ran to a successful conclusion (`Done`/`Skipped` — NOT a
 *  reschedule). Published by `TaskWorker` (via an injected hook) the moment a
 *  task completes, so a consumer can chain follow-up work off it without the
 *  handler having to know what comes next (a chunked scrape's reduce, a share
 *  card's follow-up); every other task type's `TaskFinished` is simply ignored. */
case class TaskFinished(taskType: TaskType, dedupKey: String, payload: Map[String, String]) extends DomainEvent

/**
 * Publish/subscribe bus carrying `DomainEvent`s. Per CLAUDE.md DIP guidance,
 * consumers depend on this trait; `InProcessEventBus` is the production
 * (and only) implementation.
 */
trait EventBus {
  /** Register a handler. The bus uses `applyOrElse`, so the handler only
   *  needs to match the cases it cares about. */
  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit

  /** Invoke every subscriber on the caller's thread. Throwing handlers are
   *  logged and skipped — a buggy listener can't break the bus or other
   *  listeners. */
  def publish(event: DomainEvent): Unit
}

class InProcessEventBus extends EventBus with Logging {
  // CopyOnWriteArrayList: writes (subscribe) are rare and happen at startup;
  // reads (publish) are hot and want a stable snapshot without locking.
  private val listeners = new CopyOnWriteArrayList[PartialFunction[DomainEvent, Unit]]()

  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = listeners.add(handler)

  def publish(event: DomainEvent): Unit = {
    val it = listeners.iterator()
    while (it.hasNext) {
      val pf = it.next()
      try pf.applyOrElse(event, (_: DomainEvent) => ()) catch {
        case exception: Throwable =>
          logger.warn(s"Event listener failed for $event: ${exception.getMessage}")
      }
    }
  }
}
