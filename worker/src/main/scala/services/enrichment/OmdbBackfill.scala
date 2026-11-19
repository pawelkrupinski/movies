package services.enrichment

import services.movies.{CacheKey, MovieCache}
import services.tasks.BulkRefreshResult

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.util.control.NonFatal

/**
 * OMDb IMDb-id backfill: recovers a missing `imdbId` by title+year search. It
 * writes only that IDENTIFIER — never a rating value, and never a rating site's
 * link: [[RottenTomatoesRatings]] alone finds, verifies and writes
 * `rottenTomatoesUrl`, as [[MetascoreRatings]] does `metacriticUrl`. A second
 * writer of those links raced them and bypassed their page checks. [[ImdbRatings]]
 * then fills `imdbRating` from the recovered id on its next tick.
 *
 * FALLBACK SEMANTICS:
 *   - Acts only on a TMDB film MISSING `imdbId` (nothing to gain otherwise — skip the
 *     HTTP call). A film TMDB has no record of takes its IMDb id from the identity
 *     resolver's fallback source, on every fact it publishes (`ResolverDecision.fallback`).
 *   - Writes via `orElse` against the live cached row, so a canonical writer
 *     that filled the id in between keeps its value — OMDb never overrides.
 *   - The imdb-id search is title-match guarded (see [[OMDbClient]]) so a fuzzy
 *     OMDb hit can't bind an unrelated film; how a row is asked is [[OmdbSearch]]'s.
 *   - Only an ANSWER that names no film backs the film off ([[OmdbBackoff]]). A lookup
 *     that could not be read (OMDb down, the free key's daily quota spent) throws out of
 *     `refreshOne` and records nothing, so the film is asked again next sweep —
 *     a spent quota once backed off every film still waiting behind it.
 *
 * Feature gate lives one level down in [[OMDbClient]]: with `OMDB_API_KEY`
 * unset, every method returns `None` without any HTTP call, so each
 * `refreshOne` is an immediate no-op. The whole refresher is only wired in
 * `WorkerWiring` when the key is present (see that file).
 *
 * Shares the per-row / full-corpus skeleton with the canonical refreshers via
 * [[CacheRefresher]].
 */
class OmdbBackfill(
  movieCache: MovieCache,
  search:     OmdbSearch,
  // Durable per-film backoff so a film OMDb can't resolve isn't re-probed on every daily sweep.
  backoff:    OmdbBackoff,
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(movieCache, cadenceRecorder) {

  override protected def sourceName: String = "OMDb"

  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      if (e.imdbId.isDefined || e.tmdbId.isEmpty) None // already identified, or the resolver's to identify
      else if (backoff.holds(key.cleanTitle, key.year)) None               // recently probed + missed → still backing off (no HTTP)
      else search.imdbIdOf(key, e, inSweep = backoff.sweeping) match {
        // A miss backs off: the next sweep skips the film until the (doubling) window elapses.
        case None => backoff.missed(key.cleanTitle, key.year); None
        case Some(imdbId) =>
          // `orElse` against the LIVE row: a canonical writer that won the race keeps its id.
          cache.putIfPresent(key, _.withImdbIdIfMissing(imdbId))
          logger.info(s"OMDb: '${search.label(key, e)}' (${key.year.getOrElse("?")}) recovered imdbId $imdbId")
          Some(s"imdbId $imdbId")
      }
    }

  private[services] def refreshAll(): BulkRefreshResult = {
    val snapshot = cache.entries
    val changed = new AtomicInteger(0)
    val failed  = new AtomicInteger(0)
    // The fleet's OMDb budget refused this sweep a permit: the rest of the rows wait for the next sweep, untouched --
    // nothing asked, nothing recorded, nothing failed (OmdbBudget).
    val spent    = new AtomicReference[Option[OMDbClient.BudgetSpentException]](None)
    val deferred = new AtomicInteger(0)
    // ONE batched read of the backoff stamps for the whole sweep (OmdbBackoff.during).
    backoff.during {
      logger.info(s"OMDb backfill: starting tick over ${snapshot.size} cached row(s).")
      walkEach("OMDb-backfill", snapshot) { case (key, e) =>
        if (spent.get.isDefined) { deferred.incrementAndGet(); () }
        else try refreshOne(key).foreach { v => recordCadenceChange(key, e.tmdbId, Some(v)); changed.incrementAndGet() }
        catch {
          case budget: OMDbClient.BudgetSpentException =>
            spent.compareAndSet(None, Some(budget))
            deferred.incrementAndGet()
          case NonFatal(t) =>
            logger.warn(s"OMDb backfill: '${key.cleanTitle}' (${key.year.getOrElse("?")}) not looked up — ${t.getClass.getSimpleName}: ${t.getMessage}")
            failed.incrementAndGet()
        }
      }
    }
    spent.get.foreach(b => logger.info(s"OMDb backfill: the fleet's OMDb budget has no bulk permit until ${b.retryAt}; " +
      s"${deferred.get} row(s) wait for a later sweep."))
    BulkRefreshResult.counts(walked = snapshot.size, changed = changed.get, discovered = 0, failed = failed.get,
      message = s"backfill done over ${snapshot.size} row(s) — ${changed.get} identifier(s) recovered, ${failed.get} lookup(s) failed" +
        spent.get.fold(".")(_ => s", ${deferred.get} left for a later sweep (OMDb budget)."))
  }
}
