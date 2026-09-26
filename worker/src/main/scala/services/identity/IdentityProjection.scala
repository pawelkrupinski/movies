package services.identity

import models.{Cinema, CinemaMovie, MovieRecord}
import play.api.Logging
import services.movies.{CacheKey, CinemaSlotBuilder, FilmId, ListingConstraints, MovieCache, ScreeningTokens, ShowtimesDigest,
  StoredMovieRecord, TitleNormalizer, WriteOutcome}

import java.time.{Clock, LocalDateTime, ZoneOffset}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** What one projection did: the resolution and the plan (none when it was refused before
 *  them), how many films it wrote and retired, the writes the store declined, and why it was
 *  refused, if it was. */
final case class ProjectionTick(resolution: Option[Resolution], plan: Option[ProjectionPlan], listings: Int, written: Int,
                                retired: Int, declined: Int, refused: Option[String]) {
  def wroteNothing: Boolean = written == 0 && retired == 0
}

/** Where a projection reports: its films and listings, what moved, the canary (the resolver's
 *  clusters against the films stored before it), a refusal, and how long it took. */
trait IdentityProjectionMetrics {
  def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit
  def refused(reason: IdentityProjectionMetrics.Refusal): Unit
}

object IdentityProjectionMetrics {
  enum Refusal {
    case Crossing, UnreadableMap, Shrink
    def label: String = toString.toLowerCase
  }
  val noop: IdentityProjectionMetrics = new IdentityProjectionMetrics {
    def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit = ()
    def refused(reason: Refusal): Unit = ()
  }
}

/**
 * THE IDENTITY PROJECTION (docs/design/identity-resolver.md §2, §8 phase 3 = programme phase 5):
 * in a cut-over country it replaces the landing's divert / redirect / re-key, the staging fold, the
 * settle's merges and splits and `UnresolvedTmdbReaper`'s concluding. Each projection:
 *
 *  1. reads every venue's accepted listing (`IdentityListingIntake`) — the observations;
 *  2. resolves them (`IdentityResolver`, over the calibrated `IdentityCalibration` and the pins);
 *  3. plans the films ([[IdentityProjectionPlan]]): ids by overlap through the persisted FilmId
 *     map, each film's slots from its listings, every showtime kept;
 *  4. refuses a plan that would empty the site ([[ProjectionGuard]]);
 *  5. fetches, BY ID, the TMDB details of a film new to its record (`details`);
 *  6. writes the films that changed and retires the ids no film carries (through the cache's
 *     projection funnel, no identity gate), extends the FilmId map, and announces a film whose
 *     TMDB answer changed to the enrichment chain (`announce`: IMDb-id recovery, ratings).
 *
 * `movies` / `movie_slots` / `screenings` keep their shape, so `ReadModelProjector` and every
 * enrichment keyed by the film read them unchanged, and switching the country back leaves rows the
 * landing reads. Every family is resolved on each projection: resolving the whole set is resolving
 * each touched family, and costs seconds per corpus (§17.3); only the films that changed are
 * written. A second projection over an unchanged listing set writes nothing (P2).
 */
final class IdentityProjection(
  listings:    () => Seq[(Cinema, Seq[CinemaMovie])],
  lookups:     () => IdentityLookups,
  pins:        PinStore,
  cache:       MovieCache,
  filmIds:     FilmIdCounterStore,
  details:     (MovieRecord, Int) => Option[MovieRecord],
  announce:    (CacheKey, MovieRecord) => Unit,
  normalizer:  TitleNormalizer,
  calibration: IdentityCalibration,
  slots:       CinemaSlotBuilder,
  tokens:      ScreeningTokens,
  metrics:     IdentityProjectionMetrics,
  clock:       Clock
) extends Logging {

  private val mapping = new FilmIdMapping(filmIds)
  private var consecutiveShrinks = 0

  /** One projection. Throws only what reading its inputs throws. */
  def tick(): ProjectionTick = synchronized {
    val started  = System.nanoTime()
    val corpus   = listings().flatMap { case (cinema, films) => films.map(cm => ProjectedListing(Listing.of(cinema, cm, normalizer), cm)) }
    val stored   = cache.snapshot()
    def refuse(reason: IdentityProjectionMetrics.Refusal, why: String, resolution: Option[Resolution] = None) = {
      metrics.refused(reason)
      logger.warn(s"identity projection refused (${reason.label}): $why; the stored films keep serving")
      ProjectionTick(resolution, None, corpus.size, 0, 0, 0, Some(why))
    }
    mapping.load() match {
      case Left(why) => refuse(IdentityProjectionMetrics.Refusal.UnreadableMap, why)
      case Right(counters) =>
        Try(IdentityResolver.resolve(corpus.map(_.listing), lookups(), normalizer, calibration, ListingConstraints.pinned(pins.all()))) match {
          case Failure(crossing: IdentityResolver.FamilyCrossing) =>
            refuse(IdentityProjectionMetrics.Refusal.Crossing, crossing.getMessage)
          case Failure(other) => throw other
          case Success(resolution) =>
            val at    = clock.instant()
            val draft = IdentityProjectionPlan.draft(corpus, resolution, stored, counters, normalizer, slots, tokens, at)
            ProjectionGuard.refusal(draft, stored, LocalDateTime.ofInstant(at, ZoneOffset.UTC)) match {
              case Some(why) if consecutiveShrinks < ProjectionGuard.Grace =>
                consecutiveShrinks += 1
                refuse(IdentityProjectionMetrics.Refusal.Shrink, s"$why (refusal $consecutiveShrinks of ${ProjectionGuard.Grace})",
                  Some(resolution))
              case shrink =>
                if (shrink.isDefined) logger.warn(s"identity projection: ${shrink.get} — held ${ProjectionGuard.Grace} projections, now written")
                consecutiveShrinks = 0
                write(resolution, draft, stored, corpus.size, started)
            }
        }
    }
  }

  /** [[tick]], for a scheduler that must keep running whatever one projection throws. */
  def tickQuietly(): Unit =
    try { tick(); () }
    catch { case NonFatal(e) => logger.warn(s"identity projection failed; the stored films keep serving: $e") }

  private def write(resolution: Resolution, draft: ProjectionDraft, stored: Seq[StoredMovieRecord], listings: Int, started: Long): ProjectionTick = {
    val detailed = draft.copy(drafts = draft.drafts.map { d =>
      // The details builder owns the TMDB side; what the projection derived stays the projection's.
      d.needsDetails.fold(d)(film => Try(details(d.record, film)).toOption.flatten.fold(d)(r =>
        d.copy(record = r.copy(searchTitle = d.record.searchTitle, retainedSynopses = d.record.retainedSynopses))))
    })
    val storedIds = stored.map(_.id).toSet
    val plan      = IdentityProjectionPlan.finish(detailed, normalizer, storedIds)
    val before    = stored.map(r => r.id -> r).toMap
    // The map first: a film written under a fresh id must be numbered before anything can see it.
    if (plan.counterAdditions.nonEmpty) filmIds.insert(plan.counterAdditions)
    val changed = plan.films.filter { f =>
      before.get(f.id).forall(s => s.key(normalizer) != f.key || !ShowtimesDigest.leanEqual(f.record, s.record))
    }
    val declined = writeAll(changed, plan.retired)
    changed.filter(f => before.get(f.id).forall(_.record.tmdbId != f.record.tmdbId)).foreach { f =>
      Try(announce(CacheKey.stored(f.title, f.key), f.record)).failed.foreach(e => logger.warn(s"identity projection: announcing ${f.id} failed: $e"))
    }
    val seconds = (System.nanoTime() - started) / 1e9
    metrics.projected(plan.films.size, listings, plan.regroupings, plan.canary, seconds)
    val tick = ProjectionTick(Some(resolution), Some(plan), listings, changed.size - declined, plan.retired.size, declined, None)
    if (!tick.wroteNothing || declined > 0)
      logger.info(f"identity projection: $listings listings → ${plan.films.size} films; wrote ${tick.written}, retired ${tick.retired}, " +
        s"declined $declined; ${plan.regroupings}; canary ${plan.canary.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}" +
        f" in $seconds%.1fs")
    tick
  }

  /** Write `films` and retire `retired`, in the order the store's unique `key` / `tmdbId` indexes
   *  allow: a film whose key or TMDB id another document still holds waits until the retirements
   *  and the other writes have freed it; a cycle between two kept films (each holding the other's
   *  key) is broken by parking one under a key and id of its own first. Returns how many films
   *  could not be written; the next projection tries them again. */
  private def writeAll(films: Seq[ProjectedFilm], retired: Seq[FilmId]): Int = {
    def attempt(fs: Seq[ProjectedFilm]): Seq[ProjectedFilm] =
      fs.filter(f => cache.writeProjected(f.id, CacheKey.stored(f.title, f.key), f.record) != WriteOutcome.Written)
    val first = attempt(films)
    retired.foreach { id =>
      val outcome = cache.retireProjected(id)
      if (outcome != WriteOutcome.Written) logger.warn(s"identity projection: retiring $id: $outcome")
    }
    val second = attempt(first)
    val parked = second.filter(f => cache.writeProjected(f.id, CacheKey.stored(f.title, s"~${f.id.value}|"), f.record.copy(tmdbId = None)) ==
      WriteOutcome.Written)
    val last = attempt(second)
    if (last.nonEmpty) logger.warn(s"identity projection: ${last.size} film(s) not written (e.g. ${last.head.id} under ${last.head.key}); " +
      s"${parked.size} parked; the next projection retries")
    last.size
  }
}
