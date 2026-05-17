package services.enrichment

import models.{Imdb, Source, SourceData}
import services.cinemas.CountryNames
import services.movies.{CacheKey, MovieCache}
import services.events.{DomainEvent, ImdbIdResolved, TmdbResolved}

import scala.util.{Failure, Try}

/**
 * IMDb rating maintenance — the IMDb side of the `*Ratings` pattern.
 *
 * Two responsibilities:
 *   1. **Per-row refresh**: when a row's TMDB stage publishes `TmdbResolved`
 *      (or `ImdbIdResolver` recovers an id via `ImdbIdResolved`), fetch the
 *      current rating and write it back. Wire on the bus from `AppLoader`.
 *   2. **Periodic walk**: refresh every cached row hourly so live ratings
 *      stay close to imdb.com.
 *
 * Shared lifecycle + worker plumbing lives in [[PeriodicCacheRefresher]].
 */
class ImdbRatings(cache: MovieCache, imdb: ImdbClient)
    extends PeriodicCacheRefresher(
      name                = "IMDb",
      // First run fires shortly after startup so Mongo hydration has time
      // to populate the cache before the walk reads from it.
      startupDelaySeconds = 10L,
      refreshHours        = 1L,
      cache               = cache
    ) {

  // ── Event listeners ────────────────────────────────────────────────────────

  /** Bus listener: fetch the IMDb rating as soon as the TMDB stage produces an
   *  `imdbId`. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  /** Bus listener: `ImdbIdResolver` recovered the id for a TMDB-only row;
   *  fetch the rating now that we have it. */
  val onImdbIdResolved: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Look up the row, refresh the rating via the cheap `lookup` GraphQL call,
  // then best-effort fetch the full `details` to populate the SourceData(Imdb)
  // slot (synopsis, director, cast, …). Both calls are tolerant of failure —
  // the rating refresh and the slot refresh are independent so a failing
  // details fetch doesn't block the rating update. Skips rows without an
  // `imdbId` (TMDB-only — IMDb hasn't cross-referenced the film yet).
  protected def refreshOne(key: CacheKey): Unit =
    cache.get(key).flatMap(e => e.imdbId.map(id => (e, id))).foreach { case (e, id) =>
      val freshRating  = Try(imdb.lookup(id)).toOption.flatten
      val freshDetails = Try(imdb.details(id)).toOption.flatten
      val ratingUpdate = freshRating.filter(r => !e.imdbRating.contains(r))
      val slotUpdate   = freshDetails.flatMap(d => makeSlot(d).filter(s => !e.data.get(Imdb).contains(s)))
      if (ratingUpdate.isDefined || slotUpdate.isDefined) {
        ratingUpdate.foreach(r => logger.debug(s"IMDb: ${key.cleanTitle} $id ${e.imdbRating.getOrElse("—")} → $r"))
        cache.putIfPresent(key, current => current.copy(
          imdbRating = ratingUpdate.orElse(current.imdbRating),
          data       = slotUpdate.map(s => current.data + ((Imdb: Source) -> s)).getOrElse(current.data)
        ))
      }
    }

  /** Translate an `ImdbClient.Details` into a `SourceData` slot — only the
   *  content fields (synopsis, director, cast, runtime, year, countries,
   *  poster, title, originalTitle). Returns None when every content field
   *  is empty, so a rating-only GraphQL response (the legacy `lookup` shape
   *  that recorded fixtures replay) doesn't churn an empty slot onto the
   *  record. Public for the refreshAll loop. */
  private[services] def makeSlot(d: ImdbClient.Details): Option[SourceData] = {
    val slot = SourceData(
      title          = d.title,
      originalTitle  = d.originalTitle,
      synopsis       = d.synopsis,
      cast           = d.cast,
      director       = d.director,
      runtimeMinutes = d.runtimeMinutes,
      releaseYear    = d.releaseYear,
      // IMDb's `countriesOfOrigin.text` is English ("United States",
      // "United Kingdom"); canonicalise via CountryNames so the merged
      // dedup operates on the same strings cinemas write.
      countries      = d.countries.map(CountryNames.canonical).distinct,
      posterUrl      = d.posterUrl
    )
    val hasContent = slot.title.isDefined || slot.originalTitle.isDefined || slot.synopsis.isDefined ||
                     slot.cast.isDefined || slot.director.isDefined || slot.runtimeMinutes.isDefined ||
                     slot.releaseYear.isDefined || slot.countries.nonEmpty ||
                     slot.posterUrl.isDefined
    if (hasContent) Some(slot) else None
  }

  // ── Periodic walk ──────────────────────────────────────────────────────────

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
    var changed = 0
    var failed  = 0
    withImdb.foreach { case (key, enrichment, id) =>
      val ratingResult  = Try(imdb.lookup(id))
      val detailsResult = Try(imdb.details(id))
      ratingResult match {
        case Failure(ex) =>
          failed += 1
          logger.debug(s"IMDb refresh: $id lookup failed: ${ex.getMessage}")
        case _ => ()
      }
      val freshRating  = ratingResult.toOption.flatten
      val freshDetails = detailsResult.toOption.flatten
      val ratingUpdate = freshRating.filter(r => !enrichment.imdbRating.contains(r))
      val slotUpdate   = freshDetails.flatMap(d => makeSlot(d).filter(s => !enrichment.data.get(Imdb).contains(s)))
      if (ratingUpdate.isDefined || slotUpdate.isDefined) {
        ratingUpdate.foreach(r => logger.debug(s"IMDb refresh: ${key.cleanTitle} $id ${enrichment.imdbRating.getOrElse("—")} → $r"))
        cache.putIfPresent(key, current => current.copy(
          imdbRating = ratingUpdate.orElse(current.imdbRating),
          data       = slotUpdate.map(s => current.data + ((Imdb: Source) -> s)).getOrElse(current.data)
        ))
        changed += 1
      }
    }
    val took = System.currentTimeMillis() - startedAt
    logger.info(s"IMDb refresh: tick done in ${took}ms — $changed changed, $failed failed, ${withImdb.size - changed - failed} unchanged.")
  }
}
