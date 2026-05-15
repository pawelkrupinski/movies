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

/** A title was observed in a cinema's refreshed schedule. May fire repeatedly
 *  for the same film as long as it remains in any cinema's listing —
 *  subscribers are responsible for dedup.
 *
 *  `originalTitle` carries the cinema's English/international title for the
 *  film when its API exposes one (Multikino does for ~5% of films — Cirque du
 *  Soleil, opera/concert docs, English-language imports). Used by the TMDB
 *  stage as a secondary search title when the Polish title doesn't resolve.
 *
 *  `director` carries the cinema-reported director name(s) (possibly comma-
 *  separated for co-directors). Used by the TMDB stage to *verify* a title-
 *  search candidate — when the candidate's credits don't include the
 *  reported director, the resolver walks the director's TMDB filmography
 *  instead. Solves the same-title-different-film class of mis-resolution
 *  (e.g. Niedźwiedzica → Grizzly Falls 1999 vs Frost Without Snow and Ice
 *  2026).
 *
 *  Both optional fields default to None so existing tests and cinemas
 *  without the field stay unchanged. */
case class MovieRecordCreated(
  title:         String,
  year:          Option[Int],
  originalTitle: Option[String] = None,
  director:      Option[String] = None
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

class EventBus extends Logging {
  // CopyOnWriteArrayList: writes (subscribe) are rare and happen at startup;
  // reads (publish) are hot and want a stable snapshot without locking.
  private val listeners = new CopyOnWriteArrayList[PartialFunction[DomainEvent, Unit]]()

  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = listeners.add(handler)

  /** Invoke every subscriber on the caller's thread. A handler that throws is
   *  logged and skipped — a buggy listener can't break the bus or other
   *  listeners. */
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
