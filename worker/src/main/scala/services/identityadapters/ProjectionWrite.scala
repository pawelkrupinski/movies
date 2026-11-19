package services.identityadapters

import models.{CacheKey, FilmId, LeanRecords, StoredMovieRecord}

import services.identity.*

import models.MovieRecord
import services.movies.MovieCache
import services.titles.TitleNormalizer

import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

/** The write of one planned projection ([[IdentityProjection]]): the FilmId map extended, the films that changed
 *  completed (their venue slots built in full by `rebuild` over the projection's index) and written, the ids no film carries retired, a film newly identified announced, and the projection
 *  counted and logged. Logs under the projection's `logger`. */
private[services] final class ProjectionWrite(cache: MovieCache, filmIds: FilmIdCounterStore, announce: (CacheKey, MovieRecord) => Unit,
                                              normalizer: TitleNormalizer, slotSource: VenueSlotSource, metrics: IdentityProjectionMetrics,
                                              agreementBacklog: () => Boolean, logger: play.api.Logger) {
  private val writes = new ProjectedFilmWrites(cache, logger)

  /** The films the last write could not write as planned. */
  private var lastUnwritten = Set.empty[FilmId]
  /** Of those, the films the store holds parked, as it holds them. */
  private var lastParked = Seq.empty[ProjectedFilm]

  /** The films of the last write that were written as planned, of `changed` its changed ones, and the films the store
   *  holds parked: what the index holds as written. */
  def written(changed: Seq[ProjectedFilm]): Seq[ProjectedFilm] = changed.filterNot(f => lastUnwritten(f.id)) ++ lastParked

  def apply(resolution: Resolution, index: ProjectionIndex, detailed: ProjectionDraft, plan: ProjectionPlan, stored: Seq[StoredMovieRecord], listings: Int,
            films: Int, started: tools.Stopwatch.Started,
            phases: ProjectionPhases, patch: Boolean, movedElsewhere: Set[String]): ProjectionTick = {
    val before    = stored.map(r => r.id -> r).toMap
    // A film another writer moved since this projection last wrote it may differ anywhere: compared and written whole. So
    // is every film of a store holding showtimes in the record: an untouched slot drafted lean is not the one it holds.
    val drafted   = plan.films.map(f =>
      if (f.touched.isDefined && (movedElsewhere(f.id.value) || !cache.holdsSlotsLean)) f.copy(touched = None) else f)
    // The map first: a film written under a fresh id must be numbered before anything can see it.
    if (plan.counterAdditions.nonEmpty) filmIds.insert(plan.counterAdditions)
    def moved(f: ProjectedFilm) = before.get(f.id).forall(s => s.key(normalizer) != f.key ||
      !f.touched.fold(LeanRecords.equal(f.record, s.record))(LeanRecords.equalAt(f.record, s.record, _)))
    // Compared again once complete: a venue whose lean slot differs from the stored one is built in full from its rows,
    // and a film whose built slots are the ones stored moved nothing. Written, every hourly projection rewrote it as it
    // was and counted it as drift — "(key)", nothing of it named — where a scoped one left it (prod PL, 2026-10-27).
    val (changed, unmoved) = phases("compare")(drafted.filter(moved)
      .pipe(films => detailed.complete(films, id => before.get(id).map(_.record), index, slotSource)).partition(moved))
    if (unmoved.nonEmpty) warnUnbuilt(unmoved, drafted)
    // A scoped projection writes a film it keeps under its key as only what moved; a projection of the whole corpus writes
    // every changed film whole — the hourly rewrite that puts right anything a patch could not see.
    val patchable: ProjectedFilm => Option[MovieRecord] =
      if (!patch) _ => None else f => before.get(f.id).filter(s => s.key(normalizer) == f.key).map(_.record)
    val (unwritten, parked) = phases("writes")(writes(changed, plan.retired, ProjectionWrite.independent(changed, plan.films, stored, normalizer),
      patchable))
    val declined  = unwritten.size
    announceIdentified(changed, before)
    val seconds = started.seconds
    metrics.projected(films, listings, plan.regroupings, seconds)
    lastUnwritten = unwritten
    lastParked    = parked
    val tick = ProjectionTick(Some(resolution), Some(plan), listings, changed.size - declined, plan.retired.size, declined, None, phases.all,
      changed = changed, backlog = agreementBacklog())
    logWritten(tick, plan, films, seconds, phases, patch, unwritten)
    tick
  }

  /** Films drafted from venue slots a build from their rows does not give, left as stored: logged with what the venue
   *  slot memo held for each (its lean slot) and what it missed. */
  private def warnUnbuilt(unmoved: Seq[ProjectedFilm], drafted: Seq[ProjectedFilm]): Unit = {
    val leanOf = drafted.iterator.map(f => f.id -> f.record).toMap
    logger.warn(s"identity projection: ${unmoved.size} film(s) drafted from venue slots a build from their rows does not give, " +
      s"left as stored: ${unmoved.sortBy(_.id.value).take(ProjectionWrite.FilmsNamed).map(f =>
        s"${f.id} (${ProjectionWrite.rebuilt(leanOf(f.id), f.record)})").mkString(", ")}")
  }

  /** A film now identified otherwise — by TMDB, or, one TMDB has no record of, by the fallback source's IMDb id — once
   *  its TMDB answer is whole, announced: announced while its details were still to come (TMDB failing), its IMDb-id
   *  recovery searched without the film's original title or director and was never asked again once they arrived. */
  private def announceIdentified(changed: Seq[ProjectedFilm], before: Map[FilmId, StoredMovieRecord]): Unit =
    changed.filter(f => !ProjectionWrite.awaitsDetails(f.record) && before.get(f.id).forall(s =>
      ProjectionWrite.awaitsDetails(s.record) || s.record.tmdbId != f.record.tmdbId || s.record.imdbId != f.record.imdbId)).foreach { f =>
      Try(announce(CacheKey.stored(f.title, f.key), f.record)).failed.foreach(e => logger.warn(s"identity projection: announcing ${f.id} (${f.title}) failed", e))
    }

  /** The written projection's log line, with its fields: which films, a handful, what a retire or decline burst is
   *  drilled into by. */
  private def logWritten(tick: ProjectionTick, plan: ProjectionPlan, films: Int, seconds: Double, phases: ProjectionPhases,
                         patch: Boolean, unwritten: Set[FilmId]): Unit =
    tools.LogFields.info(logger.underlyingLogger, f"identity projection: ${tick.listings} listings → $films films; wrote ${tick.written}, retired ${tick.retired}, " +
        s"declined ${tick.declined}; ${plan.regroupings}" +
        f" in $seconds%.1fs (${phases.render})",
      Seq("listings" -> tick.listings, "films" -> films, "written" -> tick.written, "retired" -> tick.retired, "declined" -> tick.declined,
        "seconds" -> math.round(seconds * 10) / 10.0, "scoped" -> patch,
        "retired_films" -> Option.when(plan.retired.nonEmpty)(plan.retired.take(ProjectionWrite.FilmsNamed).map(_.value).mkString(" ")),
        "declined_films" -> Option.when(unwritten.nonEmpty)(unwritten.take(ProjectionWrite.FilmsNamed).map(_.value).mkString(" "))))
}

private[services] object ProjectionWrite {

  /** How many films a projection's summary and warnings name, of those it retired or could not write. */
  val FilmsNamed = 50

  /** How many independent films a projection writes at once. */
  val WriteConcurrency = 8

  /** Whether `record` names a TMDB film whose details — its cross-reference ids among them — TMDB has not given yet. */
  def awaitsDetails(record: MovieRecord): Boolean = record.tmdbId.isDefined && !record.data.contains(models.Tmdb)

  /** The venue slots of `lean`, a film as drafted, that a build from their rows gave otherwise (`built`), each with the
   *  fields it differs in — `showtimes` for their digest — the first few named: the input a slot's memo key misses. */
  def rebuilt(lean: MovieRecord, built: MovieRecord): String = {
    val slots = lean.data.toSeq.flatMap { case (source, was) =>
      built.data.get(source).filterNot(LeanRecords.slotsEqual(was, _)).map { now =>
        val fields = ProjectionDrift.differing(was, now, Set("showtimes", "showtimesDigest", "showtimeStartMinutes")) ++ Option.when(models.ShowtimesDigest.slotDigest(was) != models.ShowtimesDigest.slotDigest(now))("showtimes")
        s"$source: ${fields.mkString(" ")}"
      }
    }.sorted
    (slots.take(3) ++ Option.when(slots.size > 3)(s"+${slots.size - 3} slots")).mkString("; ")
  }

  /** The films of `changed` whose write no other write can stand in the way of, nor be stood in the way
   *  of by: new to the store, under a key and a TMDB id no stored film holds and no other planned film
   *  takes. Written in any order, they land exactly as in the plan's. */
  def independent(changed: Seq[ProjectedFilm], planned: Seq[ProjectedFilm], stored: Seq[StoredMovieRecord],
                  normalizer: TitleNormalizer): Set[FilmId] = {
    val storedIds  = stored.map(_.id).toSet
    val heldKeys   = stored.map(_.key(normalizer)).toSet
    val heldTmdb   = stored.flatMap(_.record.tmdbId).toSet
    val keyTakers  = planned.groupMapReduce(_.key)(_ => 1)(_ + _)
    val tmdbTakers = planned.flatMap(_.record.tmdbId).groupMapReduce(identity)(_ => 1)(_ + _)
    changed.filter(f => !storedIds(f.id) && !heldKeys(f.key) && keyTakers(f.key) == 1 &&
      f.record.tmdbId.forall(id => !heldTmdb(id) && tmdbTakers(id) == 1)).map(_.id).toSet
  }
}
