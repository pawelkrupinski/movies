package services.enrichment

import models.{Imdb, Source, SourceData}
import services.cinemas.CountryNames
import services.movies.{CacheKey, MovieCache}
import tools.BoundedParallel

import java.util.Locale
import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Try}

/**
 * IMDb rating maintenance — the IMDb side of the `*Ratings` pattern.
 *
 * Two refresh paths (both driven by the queue):
 *   1. **Per-row refresh** (`refreshOne`): the `RatingHandler` runs this for
 *      one row once its TMDB stage has produced an `imdbId`.
 *   2. **Full-corpus refresh** (`refreshAll`): the operator-triggered bulk
 *      refresh, and the safety-net the `EnrichmentReaper` enqueues.
 *
 * Shared entry points live in [[CacheRefresher]].
 */
class ImdbRatings(
  cache: MovieCache,
  imdb:  ImdbClient,
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => (),
  // The deployment's language, used to canonicalise IMDb's always-English
  // country names into the deployment's own (Polish "Wielka Brytania" on
  // `kinowo`, English "United Kingdom" on the UK site). Defaults to Polish so
  // every existing single-country construction is unchanged.
  enrichmentLanguage: Locale = CountryNames.DefaultLanguage
) extends CacheRefresher(cache, cadenceRecorder) {

  override protected def sourceName: String = "IMDb"

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Look up the row, refresh the rating via the cheap `lookup` GraphQL call,
  // then best-effort fetch the full `details` to populate the SourceData(Imdb)
  // slot (synopsis, director, cast, …). Both calls are tolerant of failure —
  // the rating refresh and the slot refresh are independent so a failing
  // details fetch doesn't block the rating update. Skips rows without an
  // `imdbId` (TMDB-only — IMDb hasn't cross-referenced the film yet).
  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
      e.imdbId match {
        case None =>
          logger.info(s"IMDb: $label → no imdbId yet, skipping rating")
          None
        case Some(id) =>
          // Store at the precision the badge shows (`%.1f`), so a sub-decimal
          // vote drift the user can't see isn't a "change" — see RatingDisplay.
          val freshRating  = Try(imdb.lookup(id)).toOption.flatten.map(RatingDisplay.oneDecimal)
          val freshDetails = Try(imdb.details(id)).toOption.flatten
          // Write the rating whenever the displayed badge value changed; the
          // details slot writes as soon as its content changes.
          val ratingUpdate = freshRating.filter(r => !e.imdbRating.contains(r))
          val slotUpdate   = freshDetails.flatMap(d => makeSlot(d).filter(s => !e.data.get(Imdb).contains(s)))
          logger.info(s"IMDb: $label $id → rating ${freshRating.getOrElse("none")}" +
            ratingUpdate.fold("")(_ => s" (changed from ${e.imdbRating.getOrElse("—")})") +
            slotUpdate.fold("")(_ => " + details slot"))
          if (ratingUpdate.isDefined || slotUpdate.isDefined)
            cache.putIfPresent(key, current => current.copy(
              imdbRating = ratingUpdate.orElse(current.imdbRating),
              data       = slotUpdate.map(s => current.data + ((Imdb: Source) -> s)).getOrElse(current.data)
            ))
          // The cadence signal is the DISPLAYED rating moving (its badge text); a
          // details-slot-only refresh isn't a rating change.
          ratingUpdate.map(RatingDisplay.label)
      }
    }

  /** Translate an `ImdbClient.Details` into a `SourceData` slot — only the
   *  content fields (director, cast, runtime, year, countries, poster,
   *  title, originalTitle). Synopsis is intentionally left empty: IMDb's
   *  plot is always English and a Polish audience shouldn't see it bleed
   *  through the merged-synopsis "longest wins" rule. Returns None when
   *  every content field is empty, so a rating-only GraphQL response
   *  (the legacy `lookup` shape that recorded fixtures replay) doesn't
   *  churn an empty slot onto the record. Public for the refreshAll loop. */
  private[services] def makeSlot(d: ImdbClient.Details): Option[SourceData] = {
    val slot = SourceData(
      title          = d.title,
      originalTitle  = d.originalTitle,
      cast           = d.cast,
      director       = d.director,
      runtimeMinutes = d.runtimeMinutes,
      releaseYear    = d.releaseYear,
      // IMDb's `countriesOfOrigin.text` is always English ("United States",
      // "United Kingdom"); canonicalise into the deployment's language so the
      // merged dedup operates on the same strings the other sources write
      // (Polish "USA"/"Wielka Brytania" on `kinowo`, English as-is elsewhere).
      countries      = d.countries.map(c => CountryNames.canonical(c, enrichmentLanguage)).distinct,
      posterUrl      = d.posterUrl
    )
    val hasContent = slot.title.isDefined || slot.originalTitle.isDefined ||
                     slot.cast.nonEmpty || slot.director.nonEmpty || slot.runtimeMinutes.isDefined ||
                     slot.releaseYear.isDefined || slot.countries.nonEmpty ||
                     slot.posterUrl.isDefined
    if (hasContent) Some(slot) else None
  }

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  /** Walk every cached row with an `imdbId`, refreshing its rating + the
   *  SourceData(Imdb) slot. Skips rows without an `imdbId` (TMDB resolved
   *  them but IMDb hasn't cross-referenced yet — the daily TMDB-retry tick
   *  re-checks those). */
  private[services] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val withImdb  = snapshot.collect { case (k, e) if e.imdbId.isDefined => (k, e, e.imdbId.get) }
    val skipped   = snapshot.size - withImdb.size
    logger.info(s"IMDb refresh: starting tick over ${withImdb.size} cached row(s) with imdbId" +
                (if (skipped > 0) s" (skipping $skipped without imdbId)." else "."))
    val changed = new AtomicInteger(0)
    val failed  = new AtomicInteger(0)
    BoundedParallel.foreach("IMDb-refresh", withImdb, refreshConcurrency) { case (key, enrichment, id) =>
      val ratingResult  = Try(imdb.lookup(id))
      val detailsResult = Try(imdb.details(id))
      ratingResult match {
        case Failure(exception) =>
          failed.incrementAndGet()
          logger.debug(s"IMDb refresh: $id lookup failed: ${exception.getMessage}")
        case _ => ()
      }
      val freshRating  = ratingResult.toOption.flatten.map(RatingDisplay.oneDecimal)
      val freshDetails = detailsResult.toOption.flatten
      val ratingUpdate = freshRating.filter(r => !enrichment.imdbRating.contains(r))
      val slotUpdate   = freshDetails.flatMap(d => makeSlot(d).filter(s => !enrichment.data.get(Imdb).contains(s)))
      if (ratingUpdate.isDefined || slotUpdate.isDefined) {
        ratingUpdate.foreach(r => logger.debug(s"IMDb refresh: ${key.cleanTitle} $id ${enrichment.imdbRating.getOrElse("—")} → $r"))
        cache.putIfPresent(key, current => current.copy(
          imdbRating = ratingUpdate.orElse(current.imdbRating),
          data       = slotUpdate.map(s => current.data + ((Imdb: Source) -> s)).getOrElse(current.data)
        ))
        ratingUpdate.foreach(r => recordCadenceChange(key, enrichment.tmdbId, Some(RatingDisplay.label(r))))
        changed.incrementAndGet()
      }
    }
    val took = System.currentTimeMillis() - startedAt
    logger.info(s"IMDb refresh: tick done in ${took}ms — ${changed.get} changed, ${failed.get} failed, ${withImdb.size - changed.get - failed.get} unchanged.")
  }
}
