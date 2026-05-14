package services.enrichment

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.Enrichment
import play.api.Logging

import java.util.concurrent.TimeUnit

/**
 * Normalised `(title, year)` lookup key. Diacritics stripped, lowercased,
 * whitespace squashed — so "Drzewo Magii" and "drzewo magii" don't end up as
 * two cache entries. Equality uses the normalised form; `cleanTitle` retains
 * the original casing for display in snapshots.
 */
private[enrichment] case class CacheKey(cleanTitle: String, year: Option[Int]) {
  private val normalized = EnrichmentService.normalize(cleanTitle)
  override def hashCode(): Int = (normalized, year).hashCode()
  override def equals(other: Any): Boolean = other match {
    case k: CacheKey => k.normalized == normalized && k.year == year
    case _           => false
  }
}

/**
 * In-memory enrichment store with write-through to Mongo via `EnrichmentRepo`.
 *
 * Two caches:
 *   - **Positive**: successful enrichments, never expire in-process (they
 *     change slowly; restarts re-warm via `hydrateFromRepo`).
 *   - **Negative**: known misses (events, festivals, retrospectives that
 *     don't match a real film), 24h TTL — failed TMDB lookups get retried
 *     about once a day. The daily TMDB-retry scheduler can clear the whole
 *     negative cache explicitly via `clearNegatives` so the next refresh tick
 *     gets a fresh shot at every previously-failed key.
 *
 * Pure reads (`get`, `isNegative`, `snapshot`, `entries`) have no side
 * effects — callers that want to *trigger* a lookup on miss go through
 * `EnrichmentService` (which owns the worker pool + dedup).
 */
class EnrichmentCache(repo: EnrichmentRepo) extends Logging {

  private val positive: Cache[CacheKey, Enrichment] = Caffeine.newBuilder().build()
  private val negative: Cache[CacheKey, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(24, TimeUnit.HOURS).build()

  hydrateFromRepo()

  private[enrichment] def keyOf(title: String, year: Option[Int]): CacheKey =
    CacheKey(EnrichmentService.searchTitle(title), year)

  /** Pure read — never blocks, never schedules. */
  private[enrichment] def get(key: CacheKey): Option[Enrichment] =
    Option(positive.getIfPresent(key))

  private[enrichment] def isNegative(key: CacheKey): Boolean =
    negative.getIfPresent(key) != null

  /** Write-through: positive cache + Mongo upsert. */
  private[enrichment] def put(key: CacheKey, e: Enrichment): Unit = {
    positive.put(key, e)
    repo.upsert(key.cleanTitle, key.year, e)
  }

  private[enrichment] def markMissing(key: CacheKey): Unit =
    negative.put(key, java.lang.Boolean.TRUE)

  /** Drop all negative entries — used by the daily TMDB retry to give every
   *  previously-failed key one fresh shot. New misses re-populate the cache
   *  organically as they happen. */
  private[enrichment] def clearNegatives(): Unit = negative.invalidateAll()

  /** Drop a row from positive cache + Mongo — used by `reEnrich` to clear the
   *  row before re-fetching every upstream source. */
  private[enrichment] def invalidate(key: CacheKey): Unit = {
    positive.invalidate(key)
    repo.delete(key.cleanTitle, key.year)
  }

  /** Stable snapshot for debug tooling — sorted by title (case-insensitive). */
  def snapshot(): Seq[(String, Option[Int], Enrichment)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => (k.cleanTitle, k.year, e) }
      .toSeq
      .sortBy { case (t, _, _) => t.toLowerCase }
  }

  /** Snapshot of (key, enrichment) pairs for the IMDb refresh loop. Copy so a
   *  concurrent `put` mid-iteration doesn't surprise the caller. */
  private[enrichment] def entries: Seq[(CacheKey, Enrichment)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
  }

  private def hydrateFromRepo(): Unit = {
    val rows = repo.findAll()
    rows.foreach { case (title, year, e) => positive.put(CacheKey(title, year), e) }
    if (rows.nonEmpty) logger.info(s"Hydrated ${rows.size} enrichment(s) from Mongo.")
  }
}
