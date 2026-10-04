package services.identity

import java.util.Locale
import scala.util.chaining.scalaUtilChainingOps

import models.{Cinema, CinemaMovie, MovieRecord}
import play.api.Logging
import services.movies.{CacheKey, LeanRecords, ListingKey, CinemaSlotBuilder, FilmId, ListingConstraints, MovieCache, ScreeningTokens,
  StoredMovieRecord, TitleNormalizer, WriteOutcome}

import java.time.{Clock, LocalDateTime, ZoneOffset}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** What one projection did: the resolution and the plan (none when it was refused before
 *  them), how many films it wrote and retired, the writes the store declined, and why it was
 *  refused, if it was. */
final case class ProjectionTick(resolution: Option[Resolution], plan: Option[ProjectionPlan], listings: Int, written: Int,
                                retired: Int, declined: Int, refused: Option[String], phases: Seq[ProjectionPhase] = Nil,
                                slotsReused: Int = 0, slotsBuilt: Int = 0, slotMisses: (Int, Int, Int) = (0, 0, 0),
                                scoped: Boolean = false, changed: Seq[ProjectedFilm] = Nil) {
  def wroteNothing: Boolean = written == 0 && retired == 0
}

/** One step of a projection: how long it took and what it allocated on the projecting thread (the
 *  details and writes it fans out allocate on their pools' threads, which this does not count). */
final case class ProjectionPhase(name: String, seconds: Double, allocatedBytes: Long) {
  def render: String = f"$name $seconds%.1fs/${allocatedBytes / 1e6}%.0fMB"
}

/** The phases of one projection, in the order they ran. */
private[identity] final class ProjectionPhases {
  private val done = Vector.newBuilder[ProjectionPhase]

  def apply[A](name: String)(body: => A): A = {
    val (timed, allocated) = tools.ThreadAllocation.of(tools.Stopwatch.timed(body))
    done += ProjectionPhase(name, timed.seconds, allocated)
    timed.value
  }

  private var notes = Vector.empty[String]

  /** A fact about the projection the log line carries beside its phases. */
  def note(text: String): Unit = notes :+= text

  def all: Seq[ProjectionPhase] = done.result()
  def render: String            = (all.map(_.render) ++ notes).mkString(", ")
}

/** Where a projection reports: its films and listings, what moved, the canary (the resolver's
 *  clusters against the films stored before it), a refusal, and how long it took. */
trait IdentityProjectionMetrics {
  def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit
  def refused(reason: IdentityProjectionMetrics.Refusal): Unit
  /** A reconciling projection of the whole corpus changed `films` films a scoped projection would have left as they were. */
  def drifted(films: Int): Unit
}

object IdentityProjectionMetrics {
  enum Refusal {
    case Crossing, UnreadableMap, Shrink, NotReady
    /** The projection threw: nothing was written, the stored films keep serving. */
    case Failed
    def label: String = toString.toLowerCase(Locale.ROOT)
  }
  val noop: IdentityProjectionMetrics = new IdentityProjectionMetrics {
    def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit = ()
    def refused(reason: Refusal): Unit = ()
    def drifted(films: Int): Unit = ()
  }
}

/**
 * THE IDENTITY PROJECTION (docs/design/identity-resolver.md §2, §8 phase 3 = programme phase 5):
 * it replaced the landing's divert / redirect / re-key, the staging fold, the
 * settle's merges and splits and `UnresolvedTmdbReaper`'s concluding (all since deleted). Each projection:
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
  listings:    () => Seq[(Cinema, Seq[ProjectedListing])],
  rows:        Set[Cinema] => Map[Cinema, Seq[CinemaMovie]],
  resolve:     (() => Seq[Listing]) => Option[IdentityProjection.Resolved],
  cache:       MovieCache,
  filmIds:     FilmIdCounterStore,
  details:     (MovieRecord, Int) => Option[MovieRecord],
  announce:    (CacheKey, MovieRecord) => Unit,
  normalizer:  TitleNormalizer,
  slots:       CinemaSlotBuilder,
  tokens:      ScreeningTokens,
  metrics:     IdentityProjectionMetrics,
  clock:       Clock,
  fingerprints: VenueSlotFingerprints,
  /** The listings as [[listings]] gives them, read again only where this worker's own scrapes moved them: what a projection
   *  run on those ([[tickChanged]]) reads, leaving what other processes filed to the next periodic [[tick]]. */
  changedListings: Option[() => Seq[(Cinema, Seq[ProjectedListing])]] = None,
  /** Handed the listings each resolution was of, so the listings read for the next are the model's objects, not copies. */
  adopt:       Seq[Listing] => Unit = _ => (),
  /** How many projections of a scope run between two of the whole corpus ([[IdentityProjection.ScopedBetweenWhole]]). */
  scopedBetweenWhole: Int = IdentityProjection.ScopedBetweenWhole
) extends Logging {

  private val mapping = new FilmIdMapping(filmIds)
  private var consecutiveShrinks = 0
  /** The venue slots the last projection built, so this one rebuilds only the films whose listings changed. */
  private val slotMemo = new VenueSlotMemo(VenueSlotMemo.environment(normalizer))
  /** The fingerprints the store holds, as far as this worker knows: none read yet until the first projection. */
  private var recordedFingerprints: Option[Set[Long]] = None

  /** Seed the memo, before the first projection after a boot, with the slots the last run kept. */
  private def seedSlotMemo(): Unit = if (recordedFingerprints.isEmpty) {
    val recorded = Try(fingerprints.all()).fold(e => { logger.warn("identity projection: reading the venue slot fingerprints failed; " +
      "the first projection builds every slot", e); Set.empty[Long] }, identity)
    slotMemo.seed(recorded)
    recordedFingerprints = Some(recorded)
  }

  /** Record the fingerprints of the slots the memo now keeps, writing only those that moved. One that fails is
   *  tried again by the next projection; one left behind is a true fact, costing only its space. */
  private def recordSlotFingerprints(): Unit = {
    val known = recordedFingerprints.getOrElse(Set.empty)
    val now   = slotMemo.fingerprints
    if (now != known) Try(fingerprints.update(now -- known, known -- now)) match {
      case Success(_) => recordedFingerprints = Some(now)
      case Failure(e) => logger.warn("identity projection: recording the venue slot fingerprints failed; the next projection retries", e)
    }
  }

  /** That the last projection wrote everything it planned, and the FilmId map's size once it had. */
  private final case class Last(counters: Int)
  private var last: Option[Last] = None
  private var scopedSinceWhole = 0
  /** The index, kept from one projection to the next and moved where its inputs moved — afresh after any projection that
   *  did not write all it planned. */
  private var live = new LiveProjectionIndex(normalizer)
  /** Each film as last drafted, so a scoped projection works out only the venues of a film that moved. */
  private var shapes = FilmShapes()
  /** The kept index over `counters`: what a spec holds to the index built afresh from the same corpus. */
  private[identity] def keptIndex(counters: FilmIdCounters): ProjectionIndex = synchronized(live.index(counters))

  /** What moved under projections that were refused and wrote nothing: still to project. */
  private var carried = ProjectionScope.Changes.none

  /** One projection — of the corpus `whole` when asked, as the hourly reconciliation is, else of what moved. Throws only
   *  what reading its inputs throws. */
  def tick(whole: Boolean = false): ProjectionTick = synchronized { started = true; project(whole, light = false) }

  /** Whether a periodic projection has been asked for yet: until then, the boot's — of the whole corpus — waits its turn. */
  private var started = false

  /** A projection of what moved since the last, run as the identity model takes this worker's scrapes in rather than on
   *  the period: its listings read only where this worker's own scrapes moved them ([[changedListings]]), and never the
   *  whole corpus's reconcile or the slot fingerprints' record, which stay with the periodic [[tick]]. */
  def tickChanged(): Option[ProjectionTick] = synchronized {
    // With nothing written to build on — a projection refused, failed or declined a write — the next one reads and
    // projects the whole corpus. Until the period has asked for its first, that is the boot's, which waits its turn.
    if (last.isDefined) Some(project(whole = false, light = true))
    else Option.when(started)(project(whole = false, light = false))
  }

  /** Whether `tick` left nothing for another projection to do: not refused, every write taken, every written film's
   *  TMDB details fetched. One that did not is tried again ([[ProjectionTrigger.retry]]) — no five-minute period does. */
  def settled(tick: ProjectionTick): Boolean =
    tick.refused.isEmpty && tick.declined == 0 &&
      !tick.changed.exists(film => film.record.tmdbId.isDefined && !film.record.data.contains(models.Tmdb))

  private def project(whole: Boolean, light: Boolean): ProjectionTick = synchronized {
    // Until this projection has written everything it planned, the next one projects the whole corpus.
    val previous = last
    last = None
    val started  = tools.Stopwatch.start()
    val phases   = new ProjectionPhases
    val venues   = phases("listings")(if (light) changedListings.getOrElse(listings)() else listings())
    val listed   = venues.iterator.map(_._2.size).sum
    val stored   = phases("snapshot")(cache.snapshot())
    def refuse(reason: IdentityProjectionMetrics.Refusal, why: String, resolution: Option[Resolution] = None, keep: Boolean = false) = {
      metrics.refused(reason)
      // A refusal writes nothing, so what the last projection left is still what the stored films are of.
      if (keep) last = previous
      logger.warn(s"identity projection refused (${reason.label}): $why; the stored films keep serving (${phases.render})")
      ProjectionTick(resolution, None, listed, 0, 0, 0, Some(why), phases.all)
    }
    mapping.load() match {
      case Left(why) => refuse(IdentityProjectionMetrics.Refusal.UnreadableMap, why)
      case Right(counters) =>
        Try(phases("resolve")(resolve(() => venues.flatMap(_._2).map(_.listing)))) match {
          case Failure(crossing: IdentityResolver.FamilyCrossing) =>
            refuse(IdentityProjectionMetrics.Refusal.Crossing, crossing.getMessage, keep = true)
          case Failure(other) => throw other
          case Success(None) =>
            refuse(IdentityProjectionMetrics.Refusal.NotReady, "the identity model is not ready", keep = true)
          case Success(Some(IdentityProjection.Resolved(resolution, held, modelled))) =>
            adopt(modelled)
            val at    = clock.instant()
            phases("seed")(seedSlotMemo())
            if (previous.isEmpty) { live = new LiveProjectionIndex(normalizer); shapes = FilmShapes(); carried = ProjectionScope.Changes.none }
            // Exactly the listings the resolution decided: one that reached the intake after the
            // model's snapshot is projected by the next tick, never left out of its film by this one.
            val updated = phases("index")(live.update(venues.map { case (c, ls) => c.displayName -> ls }, held, resolution.decisions, stored))
            val index   = live.index(counters)
            // What moved since the last projection, closed over every film a moved one's draft can move: what a projection
            // of the scope drafts. None when the last one did not write all it planned, the FilmId map is not the one it
            // left, or a stored film is not numbered yet — each re-decided only by projecting the whole corpus.
            val changes = previous.filter(p => p.counters == counters.size && index.additions.isEmpty).map { _ =>
              carried ++ updated ++ ProjectionScope.standing(index)
            }
            val moved = changes.map(c => phases("scope")(ProjectionScope.close(index, c)))
            val reconcile = moved.isDefined && !light && (whole || scopedSinceWhole >= scopedBetweenWhole)
            val freed = changes.fold(Set.empty[String])(_.keys)
            val (scope, draft, detailed, plan) = drafted(moved.filterNot(_ => reconcile).getOrElse(ProjectionScope.Whole), freed,
              changes.fold(Set.empty[ListingKey])(_.listings), index, at, phases)
            val slotCounts = slotMemo.endTick(retainUnseen = !scope.whole)
            if (!light) phases("fingerprints")(recordSlotFingerprints())
            val misses = slotMemo.lastMisses()
            phases.note(s"venue slots reused ${slotCounts._1}, built ${slotCounts._2} " +
              s"(rows moved ${misses._1}, priors moved ${misses._2}, new ${misses._3})")
            if (!scope.whole) phases.note(s"scope ${scope.films.size} stored films, ${scope.clusters.size} clusters, ${scope.listings.size} listings")
            val scopeStored = if (scope.whole) stored else stored.filter(s => scope.films(s.id.value))
            phases("guard")(ProjectionGuard.refusal(draft, scopeStored, LocalDateTime.ofInstant(at, ZoneOffset.UTC), corpus = stored)) match {
              case Some(why) if consecutiveShrinks < ProjectionGuard.Grace =>
                consecutiveShrinks += 1
                carried = carried ++ updated
                shapes.discard()
                refuse(IdentityProjectionMetrics.Refusal.Shrink, s"$why (refusal $consecutiveShrinks of ${ProjectionGuard.Grace})",
                  Some(resolution), keep = true)
              case shrink =>
                if (shrink.isDefined) logger.warn(s"identity projection: ${shrink.get} — held ${ProjectionGuard.Grace} projections, now written")
                consecutiveShrinks = 0
                val canary    = if (scope.whole) draft.canary else IdentityProjectionPlan.canary(index)
                val films     = stored.size - scopeStored.size + plan.films.size
                val tick      = write(resolution, detailed, plan, stored, listed, films, canary, started, phases, patch = !scope.whole)
                if (reconcile) moved.foreach(drift(_, freed, index, tick))
                if (tick.declined == 0) {
                  // The index holds what was written: a film written counts as moved next time only if another writer moves it.
                  live.written(tick.changed, plan.retired)
                  shapes.commit(whole = scope.whole)
                  carried = ProjectionScope.Changes.none
                  last = Some(Last(counters.size + plan.counterAdditions.size))
                  // The hour between two reconciles is counted in periodic projections; one run on scrapes counts none.
                  scopedSinceWhole = if (scope.whole) 0 else if (light) scopedSinceWhole else scopedSinceWhole + 1
                }
                tick.copy(slotsReused = slotCounts._1, slotsBuilt = slotCounts._2, slotMisses = misses, scoped = !scope.whole)
            }
        }
    }
  }

  /** The drafts of `scope`, detailed and titled. A scope is closed again over every stored film outside it under one of
   *  its films' plain title keys, new or old — the older of two films of one title and year keeps the plain key, so
   *  their keys are decided together — until none is left outside. */
  private def drafted(start: ProjectionScope, freed: Set[String], changed: Set[ListingKey], index: ProjectionIndex, at: java.time.Instant,
                      phases: ProjectionPhases): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
    val storedIds = index.storedById.valuesIterator.map(_.id).toSet
    @scala.annotation.tailrec
    def loop(scope: ProjectionScope): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
      val draft    = phases("draft")(IdentityProjectionPlan.draftOf(index, scope, normalizer, slots, tokens, at, rows, slotMemo, shapes, changed))
      val detailed = phases("details")(draft.copy(drafts = IdentityProjection.detailed(draft.drafts, details)))
      val plan     = phases("finish")(IdentityProjectionPlan.finish(detailed, normalizer, storedIds))
      val holders  = if (scope.whole) Set.empty[String] else {
        val keys = plan.films.map(f => ProjectionScope.plainKey(f.key)) ++
          scope.films.toSeq.flatMap(index.storedById.get).map(r => ProjectionScope.plainKey(r.key(normalizer)))
        ProjectionScope.keyHolders(index, scope, keys.toSet ++ freed, normalizer)
      }
      if (holders.isEmpty) (scope, draft, detailed, plan)
      else loop(ProjectionScope.close(index, ProjectionScope.Changes(scope.listings, scope.films ++ holders)))
    }
    loop(start)
  }

  /** A reconciling projection of the whole corpus, checked against the scope its changes alone gave it — closed over key
   *  collisions too, read off this projection's own keys, which the scope's drafts would have had — so a film it wrote
   *  or retired outside that scope is one a scoped projection would have left wrong: counted and named. */
  private def drift(moved: ProjectionScope, freed: Set[String], index: ProjectionIndex, tick: ProjectionTick): Unit = tick.plan.foreach { plan =>
    def inside(scope: ProjectionScope, f: ProjectedFilm) = scope.films(f.id.value) || f.members.exists(scope.listings)
    @scala.annotation.tailrec
    def closed(scope: ProjectionScope): ProjectionScope = {
      val keys = plan.films.filter(inside(scope, _)).map(f => ProjectionScope.plainKey(f.key)) ++
        scope.films.toSeq.flatMap(index.storedById.get).map(r => ProjectionScope.plainKey(r.key(normalizer)))
      val holders = ProjectionScope.keyHolders(index, scope, keys.toSet ++ freed, normalizer)
      if (holders.isEmpty) scope else closed(ProjectionScope.close(index, ProjectionScope.Changes(scope.listings, scope.films ++ holders)))
    }
    val scope  = closed(moved)
    val missed = tick.changed.filterNot(inside(scope, _)).map(_.id.value) ++ plan.retired.map(_.value).filterNot(scope.films)
    metrics.drifted(missed.size)
    if (missed.nonEmpty) logger.warn(s"identity projection drift: the whole projection changed ${missed.size} film(s) the scoped one " +
      s"would have left: ${missed.sorted.take(20).mkString(", ")}")
  }

  /** [[tick]], for a scheduler that must keep running whatever one projection throws. */
  def tickQuietly(): Unit = { quietly(Some(tick())); () }

  /** The hourly projection of the whole corpus (`tick(whole = true)`), a failure logged as [[tickQuietly]]'s is; whether
   *  it [[settled]]. */
  def reconcileQuietly(): Boolean = quietly(Some(tick(whole = true)))

  /** [[tickChanged]], a failure logged as [[tickQuietly]]'s is; whether it [[settled]] (nothing to project is settled). */
  def tickChangedQuietly(): Boolean = quietly(tickChanged())

  private def quietly(run: => Option[ProjectionTick]): Boolean =
    try run.forall(settled)
    catch { case NonFatal(e) =>
      metrics.refused(IdentityProjectionMetrics.Refusal.Failed)
      logger.warn("identity projection failed; the stored films keep serving", e)
      false
    }

  private def write(resolution: Resolution, detailed: ProjectionDraft, plan: ProjectionPlan, stored: Seq[StoredMovieRecord], listings: Int,
                    films: Int, canary: Map[ShadowRelation, Int], started: tools.Stopwatch.Started,
                    phases: ProjectionPhases, patch: Boolean): ProjectionTick = {
    val before    = stored.map(r => r.id -> r).toMap
    // The map first: a film written under a fresh id must be numbered before anything can see it.
    if (plan.counterAdditions.nonEmpty) filmIds.insert(plan.counterAdditions)
    val changed = phases("compare")(plan.films.filter { f =>
      before.get(f.id).forall(s => s.key(normalizer) != f.key || !LeanRecords.equal(f.record, s.record))
    }.pipe(films => detailed.complete(films, id => before.get(id).map(_.record))))
    // A scoped projection writes a film it keeps under its key as only what moved; a projection of the whole corpus writes
    // every changed film whole — the hourly rewrite that puts right anything a patch could not see.
    val patchable: ProjectedFilm => Option[MovieRecord] =
      if (!patch) _ => None else f => before.get(f.id).filter(s => s.key(normalizer) == f.key).map(_.record)
    val declined = phases("writes")(writeAll(changed, plan.retired, IdentityProjection.independent(changed, plan.films, stored, normalizer),
      patchable))
    // a film now identified otherwise: by TMDB, or — one TMDB has no record of — by the fallback source's IMDb id
    changed.filter(f => before.get(f.id).forall(s => s.record.tmdbId != f.record.tmdbId || s.record.imdbId != f.record.imdbId)).foreach { f =>
      Try(announce(CacheKey.stored(f.title, f.key), f.record)).failed.foreach(e => logger.warn(s"identity projection: announcing ${f.id} (${f.title}) failed", e))
    }
    val seconds = started.seconds
    metrics.projected(films, listings, plan.regroupings, canary, seconds)
    val tick = ProjectionTick(Some(resolution), Some(plan), listings, changed.size - declined, plan.retired.size, declined, None, phases.all,
      changed = changed)
    logger.info(f"identity projection: $listings listings → $films films; wrote ${tick.written}, retired ${tick.retired}, " +
        s"declined $declined; ${plan.regroupings}; canary ${canary.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}" +
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
  private def writeAll(films: Seq[ProjectedFilm], retired: Seq[FilmId], independent: Set[FilmId],
                       patchable: ProjectedFilm => Option[MovieRecord]): Int = {
    def write(f: ProjectedFilm): Boolean = {
      val key = CacheKey.stored(f.title, f.key)
      patchable(f).fold(cache.writeProjected(f.id, key, f.record))(cache.patchProjected(f.id, key, _, f.record)) == WriteOutcome.Written
    }
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

  /** How many periodic projections of a scope run between two of the whole corpus — the reconciliation that would put right
   *  a film a scope missed, and counts it ([[IdentityProjectionMetrics.drifted]]). The worker's projections run on scrapes
   *  and are reconciled by the clock ([[ReconcileEvery]]); this paces [[tick]] alone, which an operator's settle runs. */
  private[identity] val ScopedBetweenWhole = 11

  /** How often the worker projects the whole corpus — the reconciliation, the archives' stamps read and the slot
   *  fingerprints recorded. Every other projection runs as the identity model takes this worker's scrapes in. */
  val ReconcileEvery: FiniteDuration = scala.concurrent.duration.Duration(1, java.util.concurrent.TimeUnit.HOURS)

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

  /** A resolution, the listings it decided, and the model's objects of them (none for a resolve of the listings read). */
  final case class Resolved(resolution: Resolution, listings: Set[ListingKey], modelled: Seq[Listing] = Nil)

  /** A WHOLE resolve of the listings a projection reads — the projection before the incremental
   *  model, and the reference the specs hold it to. */
  def resolving(lookups: () => IdentityLookups, pins: PinStore, normalizer: TitleNormalizer,
                calibration: IdentityCalibration): (() => Seq[Listing]) => Option[Resolved] = read => {
    val listings = read()
    Some(Resolved(IdentityResolver.resolve(listings, lookups(), normalizer, calibration, ListingConstraints.pinned(pins.all())),
      listings.map(_.key).toSet))
  }

  /** The incremental model, brought up to now on its own thread — no resolve here. */
  def modelled(model: IdentityModelService, timeout: FiniteDuration): (() => Seq[Listing]) => Option[Resolved] = _ =>
    model.current(timeout).map(snapshot => Resolved(snapshot.resolution, snapshot.listings.map(_.key).toSet, snapshot.listings))
}
