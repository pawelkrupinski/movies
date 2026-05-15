package services.movies

import models.MovieRecord
import play.api.Logging
import services.events.{DomainEvent, ImdbIdMissing, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, Executors, TimeUnit}
import scala.util.Try

/**
 * Collapses rows that the (sanitized-title, year) docId can't bridge: same
 * film, different year reporting from different cinemas, OR same film with
 * cross-script translations (Polish + Cyrillic, etc.). Two rows that resolve
 * to the same TMDB or IMDb id are definitionally the same film — this class
 * merges them into one.
 *
 * Event-driven, not scheduled. Subscribes to `TmdbResolved` and
 * `ImdbIdMissing` — both events imply TMDB resolution completed and the row
 * carries a `tmdbId`. Each event triggers a merge attempt for the row that
 * just resolved; the merge function is idempotent (no siblings → no-op).
 *
 * Survivor pick rule (deterministic across instances):
 *   - prefer year=Some(_) over year=None;
 *   - tiebreak by longer cleanTitle (more content);
 *   - within ties, lexicographically smallest cleanTitle.
 *
 * The merge unions `cinemaTitles` and `cinemaShowings` (last-writer-wins per
 * cinema), and folds every Option-valued enrichment field with non-None
 * winning. Lifecycle is owned by `AppLoader`; the class never self-subscribes
 * (CLAUDE.md).
 */
class IdentityMerger(cache: MovieCache) extends Logging {

  // Per-group dedup so two concurrent triggers (e.g. year=Some and year=None
  // both resolving to the same tmdbId on different TMDB workers at once)
  // don't both run the merge. Identifier is "tmdb:<id>" or "imdb:<id>".
  private val inFlight = ConcurrentHashMap.newKeySet[String]()

  // Worker pool — merges run async so the publishing thread isn't blocked
  // by Mongo round-trips. Safe to run async because rating listeners use
  // `cache.putIfPresent` (Caffeine `computeIfPresent` + Mongo
  // `replaceOne(upsert=false)`), so a rating write to a key the merger just
  // deleted is a no-op rather than a resurrection.
  private val Workers = 2
  private val counter = new AtomicInteger(0)
  private val worker  = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"identity-merger-${counter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  // ── Event listeners ────────────────────────────────────────────────────────

  /** Bus listener: TMDB resolved a row to an imdbId. Schedule merge. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(title, year)
  }

  /** Bus listener: TMDB resolved a row but had no IMDb cross-reference. The
   *  row still has a `tmdbId` set, which is enough to find siblings. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, _) => schedule(title, year)
  }

  // ── Core ───────────────────────────────────────────────────────────────────

  private def schedule(title: String, year: Option[Int]): Unit =
    worker.execute(() => Try(mergeForTrigger(cache.keyOf(title, year))).recover {
      case ex => logger.warn(s"IdentityMerger failed for '$title' (${year.getOrElse("?")}): ${ex.getMessage}")
    })

  /** Public sync entry for scripts and the one-shot backfill. Same semantics
   *  as the event-driven path: idempotent, collapses every row sharing an
   *  identity with the given (title, year). */
  def mergeFor(title: String, year: Option[Int]): Unit =
    mergeForTrigger(cache.keyOf(title, year))

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  def stop(): Unit = {
    worker.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }

  /** Public sync entry for scripts/tests/backfill. Idempotent: a single call
   *  collapses every row that shares an identity with `triggerKey`. */
  private[services] def mergeForTrigger(triggerKey: CacheKey): Unit = {
    val trigger = cache.get(triggerKey).getOrElse(return)
    val groupId = identityKey(trigger).getOrElse(return)

    // Skip if another thread is already merging this group. Idempotency
    // means we don't need a strict barrier — if a sibling arrives mid-merge
    // its own event will retrigger after the current run.
    if (!inFlight.add(groupId)) return
    try {
      val group = cache.entries.filter { case (k, e) => isSibling(triggerKey, trigger, k, e) }
      if (group.size < 2) return

      val (survivorKey, merged) = pickSurvivor(group)
      cache.put(survivorKey, merged)
      group.iterator
        .filterNot(_._1 == survivorKey)
        .foreach { case (k, _) => cache.invalidate(k) }
      logger.info(s"Identity merge: ${group.size} rows → '${survivorKey.cleanTitle}' " +
                  s"(${survivorKey.year.getOrElse("?")}) [$groupId]")
    } finally inFlight.remove(groupId)
  }

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def identityKey(e: MovieRecord): Option[String] =
    e.tmdbId.map(id => s"tmdb:$id").orElse(e.imdbId.map(id => s"imdb:$id"))

  /** Sibling rule: shares a tmdbId OR imdbId with the trigger AND has a
   *  matching normalised cleanTitle. Same-normalised-title catches the
   *  common case the merger is for (year=None / year=Some divergence on
   *  the same Latin title). Different-normalised-title rows that happen to
   *  share a tmdbId/imdbId — cross-script translations (Cyrillic
   *  "МОРТАЛ КОМБАТ ІІ" vs Latin "Mortal Kombat II"), foreign-language
   *  re-titlings, dub-variant suffixes — stay as separate rows. */
  private def isSibling(
    triggerKey: CacheKey, trigger: MovieRecord,
    otherKey:   CacheKey, other:   MovieRecord
  ): Boolean = {
    val sameNormalisedTitle =
      MovieService.normalize(triggerKey.cleanTitle) ==
      MovieService.normalize(otherKey.cleanTitle)
    sameNormalisedTitle && (
      trigger.tmdbId.isDefined && other.tmdbId == trigger.tmdbId ||
      trigger.imdbId.isDefined && other.imdbId == trigger.imdbId
    )
  }

  /** Deterministic survivor: year=Some > year=None; longer cleanTitle wins
   *  the tie; finally lexicographic on cleanTitle for total ordering. */
  private def pickSurvivor(group: Seq[(CacheKey, MovieRecord)]): (CacheKey, MovieRecord) = {
    val survivor = group.maxBy { case (k, _) =>
      (k.year.isDefined, k.cleanTitle.length, k.cleanTitle)
    }
    val (survivorKey, survivorE) = survivor
    val merged = group.iterator
      .filterNot(_._1 == survivorKey)
      .map(_._2)
      .foldLeft(survivorE)((acc, loser) => mergeFields(acc, loser))
    (survivorKey, merged)
  }

  /** Survivor wins on every Option field when it has a value; the loser
   *  fills in any None. `cinemaShowings` is unioned with the survivor's per-
   *  cinema slot taking precedence (so the freshest scrape stays). */
  private def mergeFields(survivor: MovieRecord, loser: MovieRecord): MovieRecord = survivor.copy(
    imdbId            = survivor.imdbId.orElse(loser.imdbId),
    imdbRating        = survivor.imdbRating.orElse(loser.imdbRating),
    metascore         = survivor.metascore.orElse(loser.metascore),
    originalTitle     = survivor.originalTitle.orElse(loser.originalTitle),
    filmwebUrl        = survivor.filmwebUrl.orElse(loser.filmwebUrl),
    filmwebRating     = survivor.filmwebRating.orElse(loser.filmwebRating),
    rottenTomatoes    = survivor.rottenTomatoes.orElse(loser.rottenTomatoes),
    tmdbId            = survivor.tmdbId.orElse(loser.tmdbId),
    metacriticUrl     = survivor.metacriticUrl.orElse(loser.metacriticUrl),
    rottenTomatoesUrl = survivor.rottenTomatoesUrl.orElse(loser.rottenTomatoesUrl),
    cinemaTitles      = survivor.cinemaTitles ++ loser.cinemaTitles,
    cinemaShowings    = loser.cinemaShowings ++ survivor.cinemaShowings
  )

}
