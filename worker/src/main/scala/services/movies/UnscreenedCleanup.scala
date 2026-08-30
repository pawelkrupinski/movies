package services.movies

import models.SourceData
import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.util.Try

/**
 * Daily tick: drop rows that no cinema is currently showing — every cinema's
 * scrape tick has pruned its slot via `recordCinemaScrape`, leaving an empty
 * `cinemaData` view. Without this cleanup the cache
 * + Mongo grow forever, holding enrichment for thousands of films that
 * dropped out of all schedules months ago.
 *
 * Trade-off: a film that returns after a gap pays the full re-enrichment
 * cost on its next scrape (TMDB search → IMDb suggestion → MC/RT/Filmweb
 * URL discovery + rating scrapes). Acceptable given how rarely a real
 * re-screening happens versus the steady-state churn of one-off events,
 * festival items, anniversary screenings, and similar.
 *
 * TWO WITNESSES, because that trade-off is only acceptable when the row is
 * genuinely dead. An empty in-memory `cinemaData` is not proof of that: the
 * first tick fires 20s after EVERY boot (below), so a cache that has not
 * finished hydrating reads as "no cinema screens this film" for rows that are
 * playing tonight. On 2026-07-27 that cost 19 still-playing arthouse features
 * (`Filipinana (2026)`, `Clarissa (2026)`, `Błogosławieni niszczyciele (2026)`,
 * …) in a single pass 20s after a restart — and the delete cascade then cleared
 * the very `movie_slots` rows that disproved it, each logging `slots=3` on the
 * way out. The tell was that removals tracked BOOT COUNT, not the calendar:
 * 0 films dropped on each of 07-24/25/26 (1–3 boots), 32 on 07-27 (29 boots).
 *
 * So the in-memory view only NOMINATES a row; the REPOSITORY corroborates.
 * A row dies only when the durable store also reports no cinemas AND that read
 * actually succeeded — a FAILED read is not evidence of emptiness (it cannot
 * tell "no cinemas" from "Mongo did not answer"), which is why we take the
 * `*Checked` variant and not the bare `findById`. Both refusals are logged
 * via `RemovalAudit.cleanupSkipped` so a boot race that WOULD have deleted rows
 * stays visible even though nothing was removed.
 *
 * The witness is `MovieRepository.findByIdChecked` rather than `movie_slots`
 * directly, because mid-migration `movie_slots` is not the whole truth: a film
 * whose slots have not been rewritten since the split landed still carries its
 * cinemas in the `movies` document's embedded `sourceData`, and asking only the
 * slot store would convict it on the strength of a collection it was never
 * written to. `findByIdChecked` returns the row every other reader sees — the
 * UNION of `movie_slots` and the embedded map, per `SlotsRepository.merge` — and
 * reports a failed slot read as unreadable rather than as a film with no cinemas.
 * Exactly the rows a delete would clear, from exactly the read a serve would use.
 *
 * Lifecycle owned by `AppLoader` (`start()` schedules the daily tick;
 * `stop()` is registered as a shutdown hook). Per CLAUDE.md, the class
 * never self-subscribes or self-schedules.
 */
class UnscreenedCleanup(cache: MovieCache, repository: MovieRepository) extends Stoppable with Logging {
  // Fold titles with the rules the corpus was keyed under, not a process default.
  private val normalizer: services.movies.TitleNormalizer = cache.normalizer

  private val scheduler = DaemonExecutors.scheduler("unscreened-cleanup")

  // First run shortly after boot so newly hydrated rows get a pass; then
  // every 24h. The hour-of-day this lands on shifts with each restart, which
  // is fine — there's no consumer timing dependency.
  private val StartupDelaySeconds = 20L
  private val RunEveryHours       = 24L

  /** Walk every cached row; drop the ones with no cinema slot left. Returns
   *  the count of rows removed. Public so the backfill script (and tests)
   *  can invoke a one-shot pass; the daily scheduler calls the same method.
   *
   *  An empty in-memory `cinemaData` only NOMINATES a row — it never convicts
   *  it. Every candidate is corroborated against the durable record, and a row
   *  dies only when THAT reports no cinemas either, on a read that actually
   *  succeeded. See the class doc for why. */
  def removeUnscreened(): Int = {
    val candidates = cache.entries.collect { case (k, e) if !e.cinemaData.values.exists(alive) => k }
    val checked    = candidates.map(key => key -> repository.findByIdChecked(idOf(key)))

    // An ABSENT row (`None`, read fine) is nothing to keep and nothing to lose: the cache
    // holds a key the corpus doesn't, so dropping it is the whole point of this pass.
    val orphans    = checked.collect { case (k, (row, true)) if !holdsCinemas(row) => k }
    val stillHeld  = checked.collect { case (k, (row, true)) if holdsCinemas(row)  => k }
    val unreadable = checked.collect { case (k, (_, false))                        => k }

    RemovalAudit.cleanupSkipped("unscreened-cleanup", stillHeld.map(label),
      reason = "stored-record-still-holds-cinemas")
    RemovalAudit.cleanupSkipped("unscreened-cleanup", unreadable.map(label),
      reason = "stored-record-read-failed")

    if (orphans.nonEmpty) {
      logger.info(s"Unscreened-row cleanup: dropping ${orphans.size} row(s) with no current screenings.")
      RemovalAudit.filmsRemoved("unscreened-cleanup", orphans.map(label), reason = "no-current-screenings")
    }
    orphans.foreach(cache.invalidate)
    orphans.size
  }

  /** Does the stored row still name a cinema? The same `cinemaData` view the cache
   *  nominated on, so the two witnesses answer the same question of different stores
   *  rather than two questions of one. */
  private def holdsCinemas(row: Option[StoredMovieRecord]): Boolean =
    row.exists(_.record.cinemaData.values.exists(alive))

  /** Is this cinema slot evidence that a cinema still HAS this film, or is it a husk?
   *
   *  "A cinema-keyed slot exists" was the old test, and it is too weak: a slot can
   *  survive its listing and keep the key while losing everything in it. Prod holds 146
   *  rows whose every cinema slot carries no title at all — the row's display title then
   *  falls back to its `_id` (`web_movies` really does serve one reading
   *  "Klapskinokobietjakzyczebyniezwariowac") — and not one of them was ever nominated,
   *  because the husk counted as a cinema. 92 of them still hold a tmdbId, so
   *  `EnrichmentReaper` keeps refreshing their ratings forever.
   *
   *  A TITLE is the evidence a cinema named the film. SHOWTIMES are the evidence one is
   *  showing it, and either alone is enough to keep the row: two of those 146 are
   *  genuinely playing tonight under a slot that carries showtimes but no title
   *  (`najswietszeserce|2025`), and convicting on the missing title alone would have
   *  deleted a live film — the very failure this class exists to prevent.
   *
   *  `showtimesCount` is why one predicate can serve BOTH witnesses. The cache-resident
   *  view has been through `ShowtimesDigest.stripForCache`, which empties `showtimes` and
   *  records the count it replaced — so the nominating side can still tell a slot that
   *  HAD showtimes from one that never did, and asks the same question of its own store
   *  that the repository asks of the durable one. */
  private def alive(slot: SourceData): Boolean =
    slot.title.exists(_.trim.nonEmpty) || slot.showtimes.nonEmpty || slot.showtimesCount.exists(_ > 0)

  /** The `_id` the delete would cascade against — the same formula
   *  `MovieCache.invalidate` → `MovieRepository.delete` keys the row by, so the
   *  record we corroborate against is exactly the one the delete would clear. */
  private def idOf(key: CacheKey): String = StoredMovieRecord.idFor(key)

  private def label(key: CacheKey): String = s"${key.cleanTitle} (${key.year.getOrElse("—")})"

  def start(): Unit = {
    logger.info(s"Unscreened-row cleanup scheduled every ${RunEveryHours}h (first run in ${StartupDelaySeconds}s).")
    scheduler.scheduleAtFixedRate(
      () => Try(removeUnscreened()).recover {
        case exception => logger.warn(s"Unscreened-row cleanup tick failed: ${exception.getMessage}")
      },
      StartupDelaySeconds, RunEveryHours * 3600, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = scheduler.shutdown()
}
