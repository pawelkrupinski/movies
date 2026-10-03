package services.identity

import models.{Cinema, CinemaMovie, MovieRecord}
import play.api.Logging
import services.movies.{CacheKey, ListingKey, CinemaSlotBuilder, FilmId, ListingConstraints, MovieCache, ScreeningTokens, ShowtimesDigest,
  StoredMovieRecord, TitleNormalizer, WriteOutcome}

import java.time.{Clock, LocalDateTime, ZoneOffset}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** What one projection did: the resolution and the plan (none when it was refused before
 *  them), how many films it wrote and retired, the writes the store declined, and why it was
 *  refused, if it was. */
final case class ProjectionTick(resolution: Option[Resolution], plan: Option[ProjectionPlan], listings: Int, written: Int,
                                retired: Int, declined: Int, refused: Option[String], phases: Seq[ProjectionPhase] = Nil) {
  def wroteNothing: Boolean = written == 0 && retired == 0
}

/** One step of a projection: how long it took and what it allocated on the projecting thread (the
 *  details and writes it fans out allocate on their pools' threads, which this does not count). */
final case class ProjectionPhase(name: String, seconds: Double, allocatedBytes: Long) {
  def render: String = f"$name $seconds%.1fs/${allocatedBytes / 1e6}%.0fMB"
}

/** The phases of one projection, in the order they ran. */
private[identity] final class ProjectionPhases {
  private val threads = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private val done    = Vector.newBuilder[ProjectionPhase]

  def apply[A](name: String)(body: => A): A = {
    val before = threads.getCurrentThreadAllocatedBytes
    val timed  = tools.Stopwatch.timed(body)
    done += ProjectionPhase(name, timed.seconds, threads.getCurrentThreadAllocatedBytes - before)
    timed.value
  }

  def all: Seq[ProjectionPhase] = done.result()
  def render: String            = all.map(_.render).mkString(", ")
}

/** Where a projection reports: its films and listings, what moved, the canary (the resolver's
 *  clusters against the films stored before it), a refusal, and how long it took. */
trait IdentityProjectionMetrics {
  def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit
  def refused(reason: IdentityProjectionMetrics.Refusal): Unit
}

object IdentityProjectionMetrics {
  enum Refusal {
    case Crossing, UnreadableMap, Shrink, NotReady
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
 *     TMDB answer changed to the enrichment chain (`announce`: IMDb-id recovery, and every rating
 *     re-fetched — the record was built afresh, without its former identity's ratings).
 *
 * `movies` / `movie_slots` / `screenings` keep their shape, so `ReadModelProjector` and every
 * enrichment keyed by the film read them unchanged, and switching the country back leaves rows the
 * landing reads. Every family is resolved on each projection: resolving the whole set is resolving
 * each touched family — at a cost that grows with the answers the store holds (the UK shadow's
 * whole-corpus resolve took 774–835s on 2026-09-28, not the seconds §17.3 measured on an emptier
 * store); only the films that changed are written. A second projection over an unchanged listing set writes nothing (P2).
 */
final class IdentityProjection(
  listings:    () => Seq[(Cinema, Seq[CinemaMovie])],
  resolve:     Seq[Listing] => Option[IdentityProjection.Resolved],
  cache:       MovieCache,
  filmIds:     FilmIdCounterStore,
  details:     (MovieRecord, Int) => Option[MovieRecord],
  announce:    (CacheKey, MovieRecord) => Unit,
  normalizer:  TitleNormalizer,
  slots:       CinemaSlotBuilder,
  tokens:      ScreeningTokens,
  metrics:     IdentityProjectionMetrics,
  clock:       Clock
) extends Logging {

  private val mapping = new FilmIdMapping(filmIds)
  private var consecutiveShrinks = 0

  /** One projection. Throws only what reading its inputs throws. */
  def tick(): ProjectionTick = synchronized {
    val started  = tools.Stopwatch.start()
    val phases   = new ProjectionPhases
    val corpus   = phases("listings")(listings().flatMap { case (cinema, films) =>
      films.map(cm => ProjectedListing(Listing.of(cinema, cm, normalizer), cm)) })
    val stored   = phases("snapshot")(cache.snapshot())
    def refuse(reason: IdentityProjectionMetrics.Refusal, why: String, resolution: Option[Resolution] = None) = {
      metrics.refused(reason)
      logger.warn(s"identity projection refused (${reason.label}): $why; the stored films keep serving (${phases.render})")
      ProjectionTick(resolution, None, corpus.size, 0, 0, 0, Some(why), phases.all)
    }
    mapping.load() match {
      case Left(why) => refuse(IdentityProjectionMetrics.Refusal.UnreadableMap, why)
      case Right(counters) =>
        Try(phases("resolve")(resolve(corpus.map(_.listing)))) match {
          case Failure(crossing: IdentityResolver.FamilyCrossing) =>
            refuse(IdentityProjectionMetrics.Refusal.Crossing, crossing.getMessage)
          case Failure(other) => throw other
          case Success(None) =>
            refuse(IdentityProjectionMetrics.Refusal.NotReady, "the identity model is not ready")
          case Success(Some(IdentityProjection.Resolved(resolution, held))) =>
            val at    = clock.instant()
            // Exactly the listings the resolution decided: one that reached the intake after the
            // model's snapshot is projected by the next tick, never left out of its film by this one.
            val draft = phases("draft")(IdentityProjectionPlan.draft(corpus.filter(row => held(row.listing.key)), resolution, stored,
              counters, normalizer, slots, tokens, at))
            phases("guard")(ProjectionGuard.refusal(draft, stored, LocalDateTime.ofInstant(at, ZoneOffset.UTC))) match {
              case Some(why) if consecutiveShrinks < ProjectionGuard.Grace =>
                consecutiveShrinks += 1
                refuse(IdentityProjectionMetrics.Refusal.Shrink, s"$why (refusal $consecutiveShrinks of ${ProjectionGuard.Grace})",
                  Some(resolution))
              case shrink =>
                if (shrink.isDefined) logger.warn(s"identity projection: ${shrink.get} — held ${ProjectionGuard.Grace} projections, now written")
                consecutiveShrinks = 0
                write(resolution, draft, stored, corpus.size, started, phases)
            }
        }
    }
  }

  /** [[tick]], for a scheduler that must keep running whatever one projection throws. */
  def tickQuietly(): Unit =
    try { tick(); () }
    catch { case NonFatal(e) => logger.warn(s"identity projection failed; the stored films keep serving: $e") }

  private def write(resolution: Resolution, draft: ProjectionDraft, stored: Seq[StoredMovieRecord], listings: Int, started: tools.Stopwatch.Started,
                    phases: ProjectionPhases): ProjectionTick = {
    val detailed = phases("details")(draft.copy(drafts = IdentityProjection.detailed(draft.drafts, details)))
    val storedIds = stored.map(_.id).toSet
    val plan      = phases("finish")(IdentityProjectionPlan.finish(detailed, normalizer, storedIds))
    val before    = stored.map(r => r.id -> r).toMap
    // The map first: a film written under a fresh id must be numbered before anything can see it.
    if (plan.counterAdditions.nonEmpty) filmIds.insert(plan.counterAdditions)
    val changed = phases("compare")(plan.films.filter { f =>
      before.get(f.id).forall(s => s.key(normalizer) != f.key || !ShowtimesDigest.leanEqual(f.record, s.record))
    })
    val declined = phases("writes")(writeAll(changed, plan.retired, IdentityProjection.independent(changed, plan.films, stored, normalizer)))
    changed.filter(f => before.get(f.id).forall(_.record.tmdbId != f.record.tmdbId)).foreach { f =>
      Try(announce(CacheKey.stored(f.title, f.key), f.record)).failed.foreach(e => logger.warn(s"identity projection: announcing ${f.id} failed: $e"))
    }
    val seconds = started.seconds
    metrics.projected(plan.films.size, listings, plan.regroupings, plan.canary, seconds)
    val tick = ProjectionTick(Some(resolution), Some(plan), listings, changed.size - declined, plan.retired.size, declined, None, phases.all)
    logger.info(f"identity projection: $listings listings → ${plan.films.size} films; wrote ${tick.written}, retired ${tick.retired}, " +
        s"declined $declined; ${plan.regroupings}; canary ${plan.canary.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}" +
        f" in $seconds%.1fs (${phases.render})")
    tick
  }

  /** Write `films` and retire `retired`, in the order the store's unique `key` / `tmdbId` indexes
   *  allow: a film whose key or TMDB id another document still holds waits until the retirements
   *  and the other writes have freed it; a cycle between two kept films (each holding the other's
   *  key) is broken by parking one under a key and id of its own first. Returns how many films
   *  could not be written; the next projection tries them again.
   *
   *  The `independent` films — new ones whose key and TMDB id nothing else holds or takes — are
   *  written side by side first: no order among them, or with the rest, can decide anything, and
   *  one at a time a country's first projection waited on ~2,250 films' round-trips in a row (a US
   *  boot). The rest keep the plan's order. */
  private def writeAll(films: Seq[ProjectedFilm], retired: Seq[FilmId], independent: Set[FilmId]): Int = {
    def write(f: ProjectedFilm): Boolean = cache.writeProjected(f.id, CacheKey.stored(f.title, f.key), f.record) == WriteOutcome.Written
    def attempt(fs: Seq[ProjectedFilm]): Seq[ProjectedFilm] = fs.filterNot(write)
    val (apart, ordered) = films.partition(f => independent(f.id))
    val unwritten = tools.BoundedParallel.map("identity-projection-writes", apart, IdentityProjection.WriteConcurrency)(f => Option.unless(write(f))(f))
    val first = unwritten.flatten ++ attempt(ordered)
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

object IdentityProjection {
  /** How many films' TMDB details a projection fetches at once — within what TMDB tolerates (see the
   *  external-api-rate-limits budgets). */
  private[identity] val DetailsConcurrency = 8

  /** Each draft whose film is new to its record, with that film's TMDB details — fetched side by
   *  side, since each is its own film's: one at a time, a country's first projection waited on
   *  ~2,250 of them in a row (a US boot). The details builder owns the TMDB side; what the projection
   *  derived stays the projection's, and a fetch that fails leaves its draft as it was. */
  private[identity] def detailed(drafts: Seq[FilmDraft], details: (MovieRecord, Int) => Option[MovieRecord]): Seq[FilmDraft] =
    tools.BoundedParallel.map("identity-projection-details", drafts, DetailsConcurrency) { d =>
      d.needsDetails.fold(d)(film => Try(details(d.record, film)).toOption.flatten.fold(d)(r =>
        d.copy(record = r.copy(searchTitle = d.record.searchTitle, retainedSynopses = d.record.retainedSynopses))))
    }

  /** How many independent films a projection writes at once. */
  private[identity] val WriteConcurrency = 8

  /** The films of `changed` whose write no other write can stand in the way of, nor be stood in the way
   *  of by: new to the store, under a key and a TMDB id no stored film holds and no other planned film
   *  takes. Written in any order, they land exactly as in the plan's. */
  private[identity] def independent(changed: Seq[ProjectedFilm], planned: Seq[ProjectedFilm], stored: Seq[StoredMovieRecord],
                                    normalizer: TitleNormalizer): Set[FilmId] = {
    val storedIds  = stored.map(_.id).toSet
    val heldKeys   = stored.map(_.key(normalizer)).toSet
    val heldTmdb   = stored.flatMap(_.record.tmdbId).toSet
    val keyTakers  = planned.groupMapReduce(_.key)(_ => 1)(_ + _)
    val tmdbTakers = planned.flatMap(_.record.tmdbId).groupMapReduce(identity)(_ => 1)(_ + _)
    changed.filter(f => !storedIds(f.id) && !heldKeys(f.key) && keyTakers(f.key) == 1 &&
      f.record.tmdbId.forall(id => !heldTmdb(id) && tmdbTakers(id) == 1)).map(_.id).toSet
  }

  /** A resolution and the listings it decided. */
  final case class Resolved(resolution: Resolution, listings: Set[ListingKey])

  /** A WHOLE resolve of the listings a projection reads — the projection before the incremental
   *  model, and the reference the specs hold it to. */
  def resolving(lookups: () => IdentityLookups, pins: PinStore, normalizer: TitleNormalizer,
                calibration: IdentityCalibration): Seq[Listing] => Option[Resolved] = listings =>
    Some(Resolved(IdentityResolver.resolve(listings, lookups(), normalizer, calibration, ListingConstraints.pinned(pins.all())),
      listings.map(_.key).toSet))

  /** The incremental model, brought up to now on its own thread — no resolve here. */
  def modelled(model: IdentityModelService, timeout: FiniteDuration): Seq[Listing] => Option[Resolved] = _ =>
    model.current(timeout).map(snapshot => Resolved(snapshot.resolution, snapshot.listings.map(_.key).toSet))
}
