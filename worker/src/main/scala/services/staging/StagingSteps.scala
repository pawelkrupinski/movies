package services.staging

import models.{CinemaShowing, MovieRecord, Source, SourceData, Tmdb}
import play.api.Logging
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{MovieRecordMerge, MovieService, TitleNormalizer}
import services.resolution.ResolutionKeys
import services.tasks.StagingTaskKeys
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome}

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
  recoverImdbId:     (String, Option[Int], models.MovieRecord) => Option[String],
  freshness:         FreshnessStore
) extends Logging {
  // The staging rows anchor under their repository's country rules — take them
  // from it rather than a second copy that could disagree.
  /** The rules this staging pipeline anchors under — read by the task handlers
   *  that build dedup keys for the same rows. */
  given normalizer: services.movies.TitleNormalizer = stagingRepository.normalizer
  import StagingSteps._

  /** Every staging row of the film whose title sanitizes to `anchor`, `_id`-sorted
   *  (so `head` is deterministic), across all cinemas + year-variants. */
  /** Delegated, so a repository that can answer without decoding the whole collection
   *  does. The inline `findAll().filter(...)` this replaced ran on every staging event. */
  def rowsFor(anchor: String): Seq[StagingRecord] = stagingRepository.findByAnchor(anchor)

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
    case Some(_) => freshness.isFresh(StagingTaskKeys.detailKey(normalizer.sanitize(row.title), row.cinema.displayName), FreshnessKind.DetailEnrich)
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
        // Mirror EnrichDetailsHandler: a 1:1 venue's listing slot is keyed per shown
        // title (`CinemaShowing`), so target THAT slot — a bare-cinema target would
        // strand the detail (with its year/director) in a separate title-less slot
        // that never merges into the listing. A chain keeps its shared network source.
        val target =
          if (enricher.detailTarget == enricher.cinema) CinemaShowing.keyFor(enricher.cinema, row.title)
          else enricher.detailTarget
        enricher.fetchDetail(ref) match {
          case DetailFetchOutcome.Fetched(detail) =>
            val before = row.record.data.getOrElse(target, SourceData())
            val after  = detail.mergeInto(before)
            val merged = row.record.copy(data = row.record.data + (target -> after))
            stagingRepository.upsertRow(row.copy(record = merged))
            // Say whether the merge actually CONTRIBUTED anything. It used to log the
            // same line either way, so a detail page that parsed to nothing — or a
            // whole cinema's pages missing from a replay's fixture tree — read exactly
            // like one that worked. That silence is what made "Pokój 666" take an hour
            // to diagnose: the leg logged `← detail from Kino Iluzjon` and then sent the
            // film to TMDB with no year, and nothing in between said why.
            //
            // Still returns true either way: the fetch RAN, which is what `detailReady`
            // gates on, and holding the film back would spin it to the give-up budget
            // for a page that is simply thin. This is a diagnosis, not a gate.
            if (after == before)
              logger.info(s"Staging: '${row.title}' ← detail from ${row.cinema.displayName} " +
                          s"added NOTHING (page parsed to no new fields)")
            else
              logger.info(s"Staging: '${row.title}' ← detail from ${row.cinema.displayName}" +
                          s" (${SourceData.fieldsGained(before, after).mkString(", ")})")
            true
          case DetailFetchOutcome.Gone(code) =>
            // The page is gone for good (404/410), so no number of retries will land
            // this detail. Spinning the row to its give-up budget just delays the same
            // outcome, so treat it as nothing-more-owed and let the film through on its
            // listing alone — the `giveUp` degrade, arrived at immediately instead of
            // after the budget. Says so out loud, because "detail never came" and "the
            // page no longer exists" want different answers from whoever reads this log.
            logger.info(s"Staging: '${row.title}' detail page for ${row.cinema.displayName} is gone (HTTP $code) — " +
                        s"degrading to listing-only rather than retrying a dead url")
            true
          case DetailFetchOutcome.Failed => detailPresent(row, target)  // fetch failed — already-merged still counts
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
      // Re-resolve each hint-group that STILL has an unconcluded row against the
      // group's COMPLETE membership — concluded siblings included — so a late
      // cinema's hints re-decide the whole group's identity instead of the FIRST
      // partial-group resolution sticking forever. Skipping concluded rows here
      // (`filterNot(tmdbConcluded)`) is exactly what made the settled corpus
      // arrival-order-dependent: a row resolved against whichever siblings had
      // arrived when the reaper first fired, then was never revisited
      // (StagingOrderDeterminismSpec). `hintGroupKey` drops the stamped `Tmdb`
      // slot so a concluded row keeps grouping with its still-arriving siblings.
      val outcomes = fresh.groupBy(hintGroupKey).values.toSeq
        .filter(_.exists(!_.record.tmdbConcluded))
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
    // Resolve from CINEMA hints only — drop any stale stamped `Tmdb` slot a prior
    // (partial-group) resolution left on a concluded row, so a re-resolution is a
    // pure function of the cinemas' own data, not the answer it's replacing.
    val mergedHints = MovieRecordMerge.unionAll(group.map(r => r.record.copy(data = r.record.data - Tmdb)))
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
   *  rows that would resolve identically group together. Computed from CINEMA/
   *  detail hints only (the stamped `Tmdb` slot is dropped) so a row's group key
   *  stays STABLE across re-resolution: a concluded row keeps grouping with its
   *  still-arriving same-film siblings instead of drifting onto TMDB's own
   *  director/title — see `resolveAndStamp`. */
  private def hintGroupKey(r: StagingRecord): String = {
    val cinemaOnly = r.record.copy(data = r.record.data - Tmdb)
    ResolutionKeys.tmdb(r.title, r.year, cinemaOnly.director, cinemaOnly.cinemaOriginalTitle)
  }

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
    // Per RESOLVED FILM (tmdbId): recover + stamp each film's imdbId independently
    // — never cross-stamp one film's id onto another that resolved to a different
    // tmdbId. Grouping by tmdbId (not hint-group) gathers a film's year-VARIANTS
    // (one cinema reports the production year, another the release year) so the
    // recovery can try EVERY reported year, not just one group's: IMDb's release
    // year can sit at any of those (TMDB resolves "Chłopiec na krańcach świata" 2026
    // while IMDb + the cinemas have it 2025), so recovering with a single key year
    // flickered the id present/absent by which variant happened to survive to be
    // recovered (StagingOrderDeterminismSpec). The sorted year set is a pure function
    // of the film, and the per-year exact match still refuses a same-series sibling
    // ("Kicia Kocia w przedszkolu" 2024) whose year matches none of them.
    fresh.filter(_.record.tmdbId.isDefined).groupBy(_.record.tmdbId).values.foreach { group =>
      group.find(_.record.imdbId.isEmpty).foreach { needy =>
        val search = needy.record.originalTitle.getOrElse(MovieService.apiQuery(needy.title))
        val years  = group.flatMap(_.year).distinct.sorted
        val tries  = if (years.isEmpty) Seq(None) else years.map(Option(_))
        tries.iterator.flatMap(y => recoverImdbId(search, y, needy.record)).nextOption().foreach { id =>
          group.foreach(r => stagingRepository.upsertRow(r.copy(record = r.record.copy(imdbId = Some(id)))))
          logger.info(s"Staging: '${needy.title}' ← recovered imdbId=$id")
        }
      }
    }
    // And the rows TMDB could NOT name. They have no tmdbId to group by and do not
    // need one: every row under this anchor is the same film by construction, so they
    // are one group. This is where an id is worth most — a `tmdbNoMatch` row has
    // nothing to look a rating up by, and an imdbId is precisely what
    // `tmdb.findByImdbId` turns back into a tmdbId for the bare-title long tail
    // ("Stop Making Sense", "Złoto") that a year-less title search must refuse.
    //
    // Clearing `tmdbNoMatch` on a hit is what sends the film back through
    // `ResolveTmdb` for that reverse lookup — `stepFor` routes on `tmdbConcluded`, so
    // a row left concluded would fold with the id and never re-ask. It cannot cycle:
    // `imdbRecoveryDone` is stamped below, so the second pass through `ResolveTmdb`
    // goes on to Fold whatever it decides. Cleared ONLY on a hit — with no id there is
    // nothing new for TMDB to see, and un-concluding would buy a re-search that must
    // reach the same answer.
    //
    // Only when NOTHING under this anchor resolved. A tmdbId-less row sitting beside a
    // resolved one is a year-VARIANT of that same film (the cinemas disagree on
    // production vs release year), not an unnamed film — the loop above owns it, and
    // recovering for it separately would search on the variant's own title and stamp a
    // second id onto one film, which is the cross-stamp the grouping above exists to
    // prevent.
    val unidentified =
      if (fresh.exists(_.record.tmdbId.isDefined)) Seq.empty
      else fresh
        .filter(r => r.record.tmdbId.isEmpty && r.record.imdbId.isEmpty)
        .sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
    unidentified.headOption.foreach { needy =>
      val search = needy.record.originalTitle.getOrElse(MovieService.apiQuery(needy.title))
      val years  = unidentified.flatMap(_.year).distinct.sorted
      val tries  = if (years.isEmpty) Seq(None) else years.map(Option(_))
      tries.iterator.flatMap(y => recoverImdbId(search, y, needy.record)).nextOption().foreach { id =>
        unidentified.foreach(r => stagingRepository.upsertRow(
          r.copy(record = r.record.copy(imdbId = Some(id), tmdbNoMatch = false))))
        logger.info(s"Staging: '${needy.title}' ← recovered imdbId=$id for a film TMDB could not name")
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
  case object DetailNotReady   extends ResolveResult  // a cinema still owes detail — not due yet; the reaper re-enqueues
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
