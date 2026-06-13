package services.staging

import models.{MovieRecord, Source, SourceData}
import play.api.Logging
import services.cinemas.DetailEnricher

/**
 * Incubates newcomer films sitting in `pending_movies` through the SAME
 * enrichment steps, in the SAME order, the live `movies` pipeline runs — just
 * pointed at staging rows:
 *
 *   1. DETAIL first (for cinemas that defer it — `DetailEnricher`): fetch the
 *      cinema's per-film detail page and merge it into the staging row's slot,
 *      exactly like `EnrichDetailsHandler` does for `movies`. A cinema that
 *      `defersTmdbResolution` waits here until its detail lands, so TMDB gets the
 *      director / original-title / production-year hints (the reason detail runs
 *      first in production).
 *   2. TMDB resolve (`MovieService.resolveStagingRecord`): conclude the row —
 *      a hit writes tmdbId + the Tmdb slot, a definitive miss sets `tmdbNoMatch`.
 *      Both conclude it; a transient failure leaves it for the next tick.
 *
 * The moment a row concludes (`tmdbConcluded`), `onConcluded` fires so the
 * folder can merge it into `movies` (Phase 5). Nothing here writes to `movies`
 * or publishes rating events — that all happens after the fold.
 *
 * `resolveStaging` is `MovieService.resolveStagingRecord`, injected as a function
 * so the promoter depends on the abstraction, not the whole service.
 */
class StagingPromoter(
  stagingRepo:    StagingRepo,
  enrichers:      Seq[DetailEnricher],
  resolveStaging: (String, Option[Int], MovieRecord) => Option[MovieRecord],
  onConcluded:    StagingRecord => Unit
) extends Logging {

  /** Run one incubation pass over every staging row. Returns how many rows
   *  concluded this pass (for logging). */
  def runOnce(): Int =
    stagingRepo.findAll().count(promote)

  /** Advance ONE staging row by (at most) a detail fetch then a TMDB resolve.
   *  Returns true if the row concluded on this pass. */
  def promote(row: StagingRecord): Boolean = {
    val enricherOpt = enrichers.find(_.cinema == row.cinema)
    val afterDetail = enricherOpt.fold(row.record)(e => fetchDetailIfNeeded(row, e))

    // A `defersTmdbResolution` cinema waits until its detail is present so TMDB
    // gets the detail-supplied hints; everything else resolves straight away.
    val detailReady = enricherOpt.forall(e => !e.defersTmdbResolution || detailPresent(afterDetail, e.detailTarget))
    if (afterDetail.tmdbConcluded || !detailReady) false
    else resolveStaging(row.title, row.year, afterDetail) match {
      case Some(resolved) =>
        stagingRepo.upsert(row.cinema, row.title, row.year, resolved)
        val concluded = resolved.tmdbConcluded
        if (concluded) onConcluded(StagingRecord(row.cinema, row.title, row.year, resolved))
        concluded
      case None => false // transient TMDB failure — retry next pass
    }
  }

  /** Fetch + merge this cinema's detail into the staging row, once — skipped when
   *  the target slot already carries detail (so steady ticks don't re-fetch) or
   *  the listing left no `filmUrl`. Returns the (possibly) enriched record;
   *  persists it when it changed. */
  private def fetchDetailIfNeeded(row: StagingRecord, enricher: DetailEnricher): MovieRecord = {
    val target = enricher.detailTarget
    if (detailPresent(row.record, target)) row.record
    else row.record.data.get(row.cinema).flatMap(_.filmUrl).flatMap(enricher.fetchFilmDetail) match {
      case Some(detail) =>
        val merged = row.record.copy(
          data = row.record.data + (target -> detail.mergeInto(row.record.data.getOrElse(target, SourceData()))))
        stagingRepo.upsert(row.cinema, row.title, row.year, merged)
        merged
      case None => row.record // fetch failed/absent — leave for retry
    }
  }

  /** Has this cinema's detail already landed? Detail supplies synopsis / poster /
   *  director / original title, so any of those present in the target slot means
   *  the page was fetched. */
  private def detailPresent(rec: MovieRecord, target: Source): Boolean =
    rec.data.get(target).exists(s =>
      s.synopsis.isDefined || s.posterUrl.isDefined || s.director.nonEmpty || s.originalTitle.isDefined)
}
