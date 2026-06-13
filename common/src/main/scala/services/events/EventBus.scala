package services.events

import play.api.Logging

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
 *  opera/concert docs, English imports). The TMDB stage uses it as a secondary
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

/** A *new* `(cinema, title, year)` tuple was just persisted to the cache (and
 *  written through to Mongo) by `recordCinemaScrape`. Fires only on the
 *  *first* observation of that tuple on the row — repeat ticks for the same
 *  combination are suppressed (the scrape's `isNew` gate).
 *  Published from inside `MovieCache.recordCinemaScrape` so the slot's
 *  visibility and the event are atomic: any handler reading the cache for
 *  the just-published tuple sees the newly-written slot.
 *
 *  Carries `filmUrl` so a cinema-specific listener (a `DetailTaskEnqueuer`
 *  for a deferred-detail cinema) can enqueue a per-film detail-page fetch
 *  without re-reading the slot.
 *
 *  Periodic safety net: detail-page enrichers should NOT rely solely on
 *  this event — handlers can be lost across a restart between publish and
 *  consumption, so each detail-page enricher should also expose a slower
 *  periodic scan that picks up any rows the event-driven path missed. */
case class CinemaMovieAdded(
  cinema:  models.Cinema,
  title:   String,
  year:    Option[Int],
  filmUrl: Option[String]
) extends DomainEvent

/** TMDB resolved a `(title, year)` to a film and an IMDb id. The enrichment
 *  pipeline publishes this after its TMDB stage writes the row to the cache;
 *  the IMDb stage subscribes and fetches the rating asynchronously, so the
 *  TMDB lookup doesn't block on IMDb's GraphQL CDN. */
case class TmdbResolved(title: String, year: Option[Int], imdbId: String) extends DomainEvent

/** TMDB resolved a `(title, year)` to a film but TMDB had no IMDb cross-
 *  reference for it (common for very recent films and festival items, e.g.
 *  "Mortal Kombat II" 2026). `searchTitle` is the title we want to search
 *  IMDb's suggestion endpoint with — typically TMDB's `originalTitle` or
 *  English release title, whichever is more likely to match IMDb's primary.
 *
 *  `ImdbIdResolver` subscribes to this event, calls `ImdbClient.findId(...)`
 *  to recover the id, writes it back to the cache, then publishes
 *  `ImdbIdResolved` so the rating + score services can chain off the new id. */
case class ImdbIdMissing(title: String, year: Option[Int], searchTitle: String) extends DomainEvent

/** `ImdbIdResolver` recovered the IMDb id for a row that TMDB couldn't
 *  cross-reference. Listeners (rating fetchers) chain off this to drive
 *  the per-row refresh once the id is finally known. */
case class ImdbIdResolved(title: String, year: Option[Int], imdbId: String) extends DomainEvent

/** A newcomer film incubating in the `pending_movies` staging collection has
 *  reached a definitive TMDB conclusion (a hit, or `tmdbNoMatch`). The
 *  `StagingFolder` listens for this to fold the film's per-cinema staging rows
 *  into the merged `movies` collection (in a transaction) and delete them. */
case class StagingFilmEnriched(cleanTitle: String, year: Option[Int]) extends DomainEvent

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
        case ex: Throwable =>
          logger.warn(s"Event listener failed for $event: ${ex.getMessage}")
      }
    }
  }
}
