package services.staging

import models.{MovieRecord, Source, SourceData, Tmdb}
import play.api.Logging
import services.cinemas.DetailEnricher
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{MovieRecordMerge, MovieService, TitleNormalizer}
import services.resolution.ResolutionKeys
import services.tasks.StagingTaskKeys

/**
 * The shared business logic for incubating a `pending_movies` newcomer — the
 * four enrichment steps the old monolithic `StagingPromoter.promoteFilm` ran
 * inline, factored out so the queue handlers (one task per step) and the
 * `StagingReaper` (which decides what to enqueue next) share ONE implementation
 * of each rule. None of it duplicates logic the direct path owns: it points the
 * SAME detail-fetch + `resolveStagingRecord` + IMDb-recovery at staging rows.
 *
 * Everything is keyed PER FILM by `anchor` = `sanitize(title)`, across every
 * cinema and year-variant. Within an anchor, resolution runs PER DISTINCT
 * HINT-COMBINATION (title + year + director set + original title): rows sharing
 * the same hints resolve once and stamp together; rows with different hints
 * resolve independently — no cross-combination merge before settle. Merging
 * across combinations happens later, at fold/settle (`FilmCanonicalizer`), which
 * clusters the independently-resolved rows by tmdbId / ±1-year. Folding is
 * per-year (each variant lands its own `movies` row).
 *
 * `resolveStaging` is `MovieService.resolveStagingRecord`, `recoverImdbId` is
 * `ImdbIdResolver.findIdFor` — injected as functions so this depends on the
 * abstractions, not the whole services.
 */
class StagingSteps(
  stagingRepository: StagingRepository,
  enrichers:         Seq[DetailEnricher],
  resolveStaging:    (String, Option[Int], MovieRecord) => Option[MovieRecord],
  recoverImdbId:     (String, Option[Int]) => Option[String],
  freshness:         FreshnessStore
) extends Logging {
  import StagingSteps._

  /** Every staging row of the film whose title sanitizes to `anchor`, `_id`-sorted
   *  (so `head` is deterministic), across all cinemas + year-variants. */
  def rowsFor(anchor: String): Seq[StagingRecord] =
    stagingRepository.findAll().filter(r => TitleNormalizer.sanitize(r.title) == anchor)

  def enricherFor(cinema: Source): Option[DetailEnricher] = enrichers.find(_.cinema == cinema)

  /** Whether the staging detail step has run for this row's cinema. Gates BOTH
   *  the move to TMDB resolution AND the eventual fold, so a cinema's detail
   *  (synopsis/poster/director) is on the row before it graduates — exactly like
   *  the promoter, which fetched detail for EVERY enricher cinema (deferring or
   *  not) before resolving. Readiness is "the fetch RAN" (marked fresh), not "the
   *  slot has content", since a detail page may carry none (Kino Rialto). A
   *  non-deferring cinema (display-only, e.g. Kino Muza) marks fresh even if its
   *  fetch failed — its detail never BLOCKS, but it's still fetched first. A cinema
   *  with no enricher at all has no detail to wait for. */
  def detailReady(row: StagingRecord): Boolean = enricherFor(row.cinema) match {
    case Some(_) => freshness.isFresh(StagingTaskKeys.detailKey(TitleNormalizer.sanitize(row.title), row.cinema.displayName), FreshnessKind.DetailEnrich)
    case None    => true
  }

  /** STEP 1 (per film + cinema): fetch + merge this cinema's per-film detail into
   *  EVERY one of its year-variant rows for the film, then mark the fetch fresh so
   *  `detailReady` lets resolution proceed. Returns true once the fetch has landed
   *  (or the cinema doesn't defer) — false when a deferred fetch still hasn't, so
   *  the task reschedules + retries. `mergeInto` is idempotent, so a re-fetch only
   *  fills gaps.
   *
   *  `giveUp` is the handler's "retry budget exhausted" signal: a deferred fetch
   *  that can NEVER land (e.g. a Filmweb-fallback row whose filmUrl points at
   *  Filmweb, which the cinema's own enricher can't parse) would otherwise
   *  reschedule forever. When set, we mark the detail fresh anyway and report
   *  ready, degrading the film to listing-only data — exactly what each
   *  `DetailEnricher` promises a missing/slow detail does (and what
   *  `EnrichDetailsHandler` already does on the direct path). */
  def fetchDetailFor(cinema: Source, anchor: String, giveUp: Boolean = false): Boolean = enricherFor(cinema) match {
    case None    => true                                                  // not a detail cinema — nothing owed
    case Some(e) =>
      val fetched = rowsFor(anchor).filter(_.cinema == cinema).forall(r => fetchDetailRow(r, e) || !e.defersTmdbResolution)
      if (!fetched && giveUp)
        logger.warn(s"Staging: giving up on ${cinema.displayName} detail for '$anchor' after repeated failures — degrading to listing-only")
      val ready = fetched || giveUp
      if (ready) freshness.markFresh(StagingTaskKeys.detailKey(anchor, cinema.displayName), FreshnessKind.DetailEnrich)
      ready
  }

  private def fetchDetailRow(row: StagingRecord, enricher: DetailEnricher): Boolean =
    enricher.nativeDetailRef(row.record) match {
      case None      => true                                             // nothing native to fetch (no filmUrl, or a Filmweb-fallback row) — not owed
      case Some(ref) =>
        val target = enricher.detailTarget
        enricher.fetchFilmDetail(ref) match {
          case Some(detail) =>
            val merged = row.record.copy(
              data = row.record.data + (target -> detail.mergeInto(row.record.data.getOrElse(target, SourceData()))))
            stagingRepository.upsertRow(row.copy(record = merged))
            logger.info(s"Staging: '${row.title}' ← detail from ${row.cinema.displayName}")
            true
          case None => detailPresent(row, target)                       // fetch failed — already-merged still counts
        }
    }

  private def detailPresent(row: StagingRecord, target: Source): Boolean =
    row.record.data.get(target).exists(s => s.synopsis.isDefined || s.cast.nonEmpty || s.director.nonEmpty)

  /** STEP 2 (per film): resolve each still-unconcluded HINT-COMBINATION among the
   *  anchor's rows independently — each group at its own lowest-present year over
   *  the union of just that group's slots — then stamp `tmdbId` / `tmdbNoMatch` /
   *  the `Tmdb` slot (and any IMDb id TMDB shipped) onto that group's rows only.
   *  No cross-combination merge: two cinemas reporting different directors/years
   *  resolve to whatever each one's hints say, and the fold reconciles them.
   *
   *  `AlreadyDone` only when EVERY row is concluded (so a partially-resolved
   *  anchor keeps getting re-enqueued for its remaining groups); `TransientFailure`
   *  if any group's resolve fails (already-stamped groups stay concluded, the
   *  reaper retries the rest). */
  def resolveAndStamp(anchor: String, giveUp: Boolean = false): ResolveResult = {
    val fresh = rowsFor(anchor)
    if (fresh.isEmpty || fresh.forall(_.record.tmdbConcluded)) AlreadyDone
    else if (!fresh.forall(detailReady)) DetailNotReady
    else {
      val outcomes = fresh.filterNot(_.record.tmdbConcluded)
        .groupBy(hintGroupKey).values.toSeq
        .map(resolveAndStampGroup(_, giveUp))
      if (outcomes.contains(TransientFailure)) TransientFailure else Resolved
    }
  }

  /** Resolve + stamp one hint-combination's rows. `giveUp` is the handler's
   *  "retry budget exhausted" signal: a lookup that keeps failing (`None`) would
   *  otherwise re-resolve forever, so we conclude the group as a no-match
   *  (`tmdbNoMatch = true`) — exactly a definitive `Success(None)` miss — and let
   *  it fold un-enriched, the resolve-step analogue of `fetchDetailFor`'s giveUp. */
  private def resolveAndStampGroup(group: Seq[StagingRecord], giveUp: Boolean): ResolveResult = {
    val resolveYear = group.flatMap(_.year).minOption
    val mergedHints = MovieRecordMerge.unionAll(group.map(_.record))
    resolveStaging(group.head.title, resolveYear, mergedHints) match {
      case None if giveUp =>
        group.foreach(r => stagingRepository.upsertRow(r.copy(record = r.record.copy(tmdbNoMatch = true))))
        logger.warn(s"Staging: giving up TMDB resolve for '${group.head.title}' (${resolveYear.getOrElse("?")}) after repeated failures — concluding as no-match (folds un-enriched).")
        Resolved
      case None => TransientFailure
      case Some(resolved) =>
        val tmdbSlot = resolved.data.get(Tmdb)
        group.foreach { r =>
          val stamped = r.record.copy(
            tmdbId      = resolved.tmdbId,
            imdbId      = resolved.imdbId,
            tmdbNoMatch = resolved.tmdbNoMatch,
            data        = tmdbSlot.fold(r.record.data)(s => r.record.data + (Tmdb -> s)))
          stagingRepository.upsertRow(r.copy(record = stamped))
        }
        logger.info(s"Staging: '${group.head.title}' (${resolveYear.getOrElse("?")}) → resolved (tmdbId=${resolved.tmdbId.getOrElse("—")}, noMatch=${resolved.tmdbNoMatch})")
        Resolved
    }
  }

  /** The hint-combination a row resolves under — title + year + director set +
   *  original title, the same hints the TMDB resolver and its cache key use, so
   *  rows that would resolve identically group together. */
  private def hintGroupKey(r: StagingRecord): String =
    ResolutionKeys.tmdb(r.title, r.year, r.record.director, r.record.cinemaOriginalTitle)

  /** STEP 3 (per film): recover a missing IMDb cross-reference and stamp it onto
   *  every row — the promoter's inline recovery, now its own retryable task. A
   *  staging row never enters the cache, so the event-driven `ImdbIdResolver`
   *  can't reach it. On a miss it gives up and lets the film fold with just the
   *  TMDB id, exactly as the promoter did.
   *
   *  Recovers from ANY row that carries a `tmdbId` but no `imdbId` — NOT just the
   *  `_id`-first row. A film's cinemas can disagree on year, and a late-arriving
   *  variant (no tmdbId yet) can sort ahead of the resolved one; keying off `head`
   *  then skipped recovery AND the freshness mark, so the reaper (whose gate fires
   *  on ANY needy row) re-enqueued this step forever. The mark is now
   *  UNCONDITIONAL: whenever the step runs for a non-empty group it records "done",
   *  so it's genuinely one-shot and can't hot-loop. */
  def recoverImdbFor(anchor: String): Unit = {
    val fresh = rowsFor(anchor)
    if (fresh.isEmpty) return
    // Per hint-group: a group resolved to its own tmdbId, so recover + stamp
    // each group's imdbId independently — never cross-stamp one group's id onto
    // another that resolved to a different film.
    fresh.groupBy(hintGroupKey).values.foreach { group =>
      group.find(r => r.record.tmdbId.isDefined && r.record.imdbId.isEmpty).foreach { needy =>
        val search = needy.record.originalTitle.getOrElse(MovieService.apiQuery(needy.title))
        recoverImdbId(search, group.flatMap(_.year).minOption).foreach { id =>
          group.foreach(r => stagingRepository.upsertRow(r.copy(record = r.record.copy(imdbId = Some(id)))))
          logger.info(s"Staging: '${needy.title}' ← recovered imdbId=$id")
        }
      }
    }
    // Best-effort + one-shot: mark done whenever the step runs (recovered,
    // not-found, or nothing to recover) so the reaper folds instead of
    // re-enqueuing forever.
    freshness.markFresh(StagingTaskKeys.imdbKey(anchor), FreshnessKind.ImdbRating)
  }

  /** Whether IMDb recovery has already been attempted for this film — so the
   *  reaper folds instead of re-enqueuing the (best-effort) step forever. */
  def imdbRecoveryDone(anchor: String): Boolean =
    freshness.isFresh(StagingTaskKeys.imdbKey(anchor), FreshnessKind.ImdbRating)
}

object StagingSteps {
  /** What `resolveAndStamp` decided — drives the handler's outcome + the reaper. */
  sealed trait ResolveResult
  case object Resolved         extends ResolveResult  // stamped a hit or tmdbNoMatch — film is concluded
  case object DetailNotReady   extends ResolveResult  // a deferred cinema still owes detail (safety net) — retry
  case object TransientFailure extends ResolveResult  // TMDB returned None — retry with backoff
  case object AlreadyDone      extends ResolveResult  // gone, or already concluded — nothing to do
}

/** The step an incubating film needs NEXT on its way out of `pending_movies`:
 *  detail → resolve-TMDB → resolve-IMDb → fold. Computed by
 *  [[StagingReaper.stepFor]] (the single source of truth the reaper acts on and
 *  the metrics count by); `label` is the Prometheus label value. */
sealed trait StagingStep { def label: String }
object StagingStep {
  case object Detail      extends StagingStep { val label = "detail"       }
  case object ResolveTmdb extends StagingStep { val label = "resolve_tmdb" }
  case object ResolveImdb extends StagingStep { val label = "resolve_imdb" }
  case object Fold        extends StagingStep { val label = "fold"         }
  val all: Seq[StagingStep] = Seq(Detail, ResolveTmdb, ResolveImdb, Fold)
}
