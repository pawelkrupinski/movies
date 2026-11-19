package services.identityadapters

import services.identity.*
import services.movies.LatestTitleYear

import java.util.Locale

import models.{Cinema, MovieRecord, ListingKey}
import play.api.Logging
import services.movies.{CacheKey, MovieCache, StoredMovieRecord}
import services.titles.TitleNormalizer

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
                                scoped: Boolean = false, changed: Seq[ProjectedFilm] = Nil, backlog: Boolean = false) {
  def wroteNothing: Boolean = written == 0 && retired == 0
  /** Whether projecting again would find nothing to do: it was taken, wrote nothing, and its agreement read every model
   *  take (`backlog`: some were left to a later projection's budget). A refused one wrote nothing too, but a model not
   *  ready yet (a boot whose resolve ran past its budget) has every film still to write. */
  def atRest: Boolean = refused.isEmpty && wroteNothing && !backlog
}

/** One step of a projection: how long it took and what it allocated on the projecting thread (the
 *  details and writes it fans out allocate on their pools' threads, which this does not count). */
final case class ProjectionPhase(name: String, seconds: Double, allocatedBytes: Long) {
  def render: String = f"$name $seconds%.1fs/${allocatedBytes / 1e6}%.0fMB"
}

/** The phases of one projection, in the order they ran. */
private[services] final class ProjectionPhases {
  private var done = Vector.empty[ProjectionPhase]

  def apply[A](name: String)(body: => A): A = {
    val (timed, allocated) = tools.ThreadAllocation.of(tools.Stopwatch.timed(body))
    done :+= ProjectionPhase(name, timed.seconds, allocated)
    timed.value
  }

  private var notes = Vector.empty[String]

  /** A fact about the projection the log line carries beside its phases. */
  def note(text: String): Unit = notes :+= text

  def all: Seq[ProjectionPhase] = done
  def render: String            = (all.map(_.render) ++ notes).mkString(", ")
}

/** Where a projection reports: its films and listings, what moved, a refusal, and how long it took. */
trait IdentityProjectionMetrics {
  def projected(films: Int, listings: Int, regroupings: Regroupings, seconds: Double): Unit
  def refused(reason: IdentityProjectionMetrics.Refusal): Unit
  /** A reconciling projection of the whole corpus changed `films` films a scoped projection would have left as they were. */
  def drifted(films: Int): Unit
  /** A reconciling projection of the whole corpus ran — `measured` when an earlier projection gave it the scope to check
   *  its drift against, unmeasured when none had (`drifted` is then not called). */
  def reconciled(measured: Boolean): Unit
  /** A written projection's change list ([[TakeChange]]): the listings whose TMDB film it moved — the live alarm on match
   *  changes — and those it decided first, each with its basis. */
  def takesChanged(changes: Seq[TakeChange]): Unit
  /** The match changes of a written projection that undid the listing's last film change ([[TakeReversals]]). */
  def takesReversed(reversals: Seq[TakeReversals.Reversal]): Unit = ()
  /** One projection's agreement phase, by its mode ([[IdentityProjection.Mode]]'s label): what it took and allocated on the
   *  projecting thread — the cost the `IdentityAgreementCostRegression` alert compares with its week. */
  def agreementPhase(mode: String, phase: ProjectionPhase): Unit
}

object IdentityProjectionMetrics {
  enum Refusal {
    case Crossing, UnreadableMap, Shrink, NotReady
    /** The projection threw: nothing was written, the stored films keep serving. */
    case Failed
    def label: String = toString.toLowerCase(Locale.ROOT)
  }
  val noop: IdentityProjectionMetrics = new IdentityProjectionMetrics {
    def projected(films: Int, listings: Int, regroupings: Regroupings, seconds: Double): Unit = ()
    def refused(reason: Refusal): Unit = ()
    def drifted(films: Int): Unit = ()
    def reconciled(measured: Boolean): Unit = ()
    def takesChanged(changes: Seq[TakeChange]): Unit = ()
    def agreementPhase(mode: String, phase: ProjectionPhase): Unit = ()
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
 *  5. fetches, BY ID, the TMDB details of a film new to its record ([[ProjectionDrafter]]);
 *  6. writes the films that changed and retires the ids no film carries (through the cache's
 *     projection funnel, no identity gate), extends the FilmId map, and announces a film whose
 *     TMDB answer changed, once its details are in, to the enrichment chain (`announce`: IMDb-id
 *     recovery, and every rating re-fetched — the record was built afresh, without its former
 *     identity's ratings).
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
  resolve:     (() => Seq[Listing]) => Option[IdentityProjection.Resolved],
  cache:       MovieCache,
  filmIds:     FilmIdCounterStore,
  announce:    (CacheKey, MovieRecord) => Unit,
  /** Each film's draft: its venue slots, TMDB details, title and key. */
  drafter:     ProjectionDrafter,
  /** How a written projection's take changes are told: the metrics, the change feed, the log and the take history. */
  takeReports: ProjectionTakeReports,
  normalizer:  TitleNormalizer,
  metrics:     IdentityProjectionMetrics,
  clock:       Clock,
  /** The listings as [[listings]] gives them, read again only where this worker's own scrapes moved them: what a projection
   *  run on those ([[tickChanged]]) reads, leaving what other processes filed to the next periodic [[tick]]. */
  changedListings: () => Seq[(Cinema, Seq[ProjectedListing])],
  /** Handed the listings each resolution was of, so the listings read for the next are the model's objects, not copies. */
  adopt:       Seq[Listing] => Unit = _ => (),
  /** How many projections of a scope run between two of the whole corpus ([[IdentityProjection.ScopedBetweenWhole]]). */
  scopedBetweenWhole: Int = IdentityProjection.ScopedBetweenWhole,
  /** The resolution as projected: the model's, with the no-matches other film databases agree on taken
   *  (`agreement.AgreementStage`), given each listing the resolution decided and the keys of those re-published since
   *  the last projection with a field the model's decisions do not move with (`LiveProjectionIndex.republished`; `None`
   *  when there is no last projection to tell by). */
  agreement:   (Resolution, ListingKey => Option[Listing], Option[Seq[ListingKey]]) => Resolution = (resolution, _, _) => resolution,
  /** Whether the agreement left model takes unread for a later projection (`agreement.AgreementStage.backlog`). */
  agreementBacklog: () => Boolean = () => false,
  /** Handed each WRITTEN projection of the whole corpus's resolution, as projected, and every listing it decided: what
   *  counts its suspect takes (`SuspectTakeCount.submit`), which must return at once — the count is no part of a projection. */
  suspects:    (Resolution, Seq[Listing]) => Unit = (_, _) => (),
  /** The venue pages (venue, page) whose read moved since the last call, each given once (`IndexedVenuePageFacts.takeMoved`):
   *  a venue slot is built from its listing's page ([[CinemaSlotBuilder.pages]]), so a listing whose page moved is a change
   *  like one re-scraped — left out, only the hourly projection of the whole corpus rebuilt its slot. Read off the one feed
   *  every input's change comes on; None when it no longer holds every filing since the last call: every listing on a
   *  page is then a change. */
  pagesMoved:  () => Option[Seq[(Cinema, String)]] = () => Some(Nil)
) extends Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  private val mapping = new FilmIdMapping(filmIds)
  private val writer  = new ProjectionWrite(cache, filmIds, announce, normalizer, metrics, agreementBacklog, logger)
  private val drift   = new ProjectionDrift(normalizer, metrics, logger)
  private var consecutiveShrinks = 0

  /** That the last projection wrote everything it planned, and the FilmId map's size once it had. */
  private final case class Last(counters: Int)
  private var last: Option[Last] = None

  /** What one projection read before resolving — the venues' listings and the stored films — and how it reports;
   *  `previous` is what the last projection left. */
  private final class Run(val mode: IdentityProjection.Mode, val previous: Option[Last], val started: tools.Stopwatch.Started,
                          val phases: ProjectionPhases, val venues: Seq[(Cinema, Seq[ProjectedListing])],
                          val stored: Seq[StoredMovieRecord]) {
    val listed: Int = venues.iterator.map(_._2.size).sum
    // the listings read, and by venue name: each worked out once, when first asked for
    lazy val read: Seq[Listing] = venues.flatMap(_._2).map(_.listing)
    lazy val byVenue: Seq[(String, Seq[ProjectedListing])] = venues.map { case (c, ls) => c.displayName -> ls }
    // built only when the agreement meets a decision it has not seen: a quiet tick builds none
    lazy val decided: Map[ListingKey, Listing] =
      venues.iterator.flatMap(_._2).map(projected => projected.listing.key -> projected.listing).toMap
  }
  private var scopedSinceWhole = 0
  /** The model's resolution the last projection read, and what the agreement made of it: the live takes the staging
   *  publisher reads. Both are shared with the model and the stage, nothing copied. */
  @volatile private var lastTaken: Option[IdentityProjection.Projected] = None
  def lastProjected: Option[IdentityProjection.Projected] = lastTaken
  /** The index, kept from one projection to the next and moved where its inputs moved — afresh after any projection that
   *  did not write all it planned. */
  private var live = new LiveProjectionIndex(normalizer)
  /** The kept index over `counters`: what a spec holds to the index built afresh from the same corpus. */
  private[services] def keptIndex(counters: FilmIdCounters): ProjectionIndex = synchronized(live.index(counters))

  /** What moved under projections that were refused and wrote nothing: still to project. */
  private var carried = ProjectionScope.Changes.none

  /** One projection — of the corpus `whole` when asked, as the hourly reconciliation is, else of what moved. Throws only
   *  what reading its inputs throws. */
  def tick(whole: Boolean = false): ProjectionTick = synchronized {
    started = true
    project(if (whole) IdentityProjection.Mode.Reconcile else IdentityProjection.Mode.Periodic)
  }

  /** Whether a periodic projection has been asked for yet: until then, the boot's — of the whole corpus — waits its turn. */
  private var started = false

  /** A projection of what moved since the last, run as the identity model takes this worker's scrapes in rather than on
   *  the period: its listings read only where this worker's own scrapes moved them ([[changedListings]]), and never the
   *  whole corpus's reconcile or the slot fingerprints' record, which stay with the periodic [[tick]]. */
  def tickChanged(): Option[ProjectionTick] = synchronized {
    // With nothing written to build on — a projection refused, failed or declined a write — the next one reads and
    // projects the whole corpus. Until the period has asked for its first, that is the boot's, which waits its turn.
    if (last.isDefined) Some(project(IdentityProjection.Mode.Changed))
    else Option.when(started)(project(IdentityProjection.Mode.Periodic))
  }

  /** Whether `tick` left nothing for another projection to do: not refused, every write taken, every written film's
   *  TMDB details fetched, its agreement's budget reaching every verdict and take (no `backlog`: a deploy's backfill
   *  goes on by itself, not only as scrapes and answers come). One that did not is tried again ([[EventTrigger.retry]])
   *  — no five-minute period does. */
  def settled(tick: ProjectionTick): Boolean =
    tick.refused.isEmpty && tick.declined == 0 && !tick.backlog &&
      !tick.changed.exists(film => ProjectionWrite.awaitsDetails(film.record))

  /** One projection, every line it logs on this thread carrying its id (`projection_id`, the clock's instant at its start),
   *  mode and trigger ([[ProjectionTriggers.next]]) — what ties a tick's take changes, summary and warnings together in VictoriaLogs. */
  private def project(mode: IdentityProjection.Mode): ProjectionTick = synchronized {
    trigger = triggers.next(mode)
    tools.LogFields.within("projection_id" -> clock.instant().toString, "projection_mode" -> mode.label,
      "projection_trigger" -> trigger)(projectNow(mode))
  }

  /** What asked for each projection, as its `trigger` names it. */
  private val triggers = new ProjectionTriggers
  /** The running projection's trigger ([[ProjectionTriggers.next]]), which its take-change lines name. */
  private var trigger = "deploy"
  /** Whether the stored films hold this process's own takes: once a projection of it wrote all it planned (its first is of
   *  the whole corpus). Until then a take moved on unchanged inputs is the code's ([[TakeCauses]]). */
  private var ownTakes = false

  /** Note what asks for the projection the caller is about to request: a scrape the model took in, an answer filed, a
   *  stored film another writer moved. The next projection names it as its `trigger` ([[ProjectionTriggers.next]]). */
  def asked(trigger: IdentityProjection.Trigger): Unit = triggers.asked(trigger)

  private def projectNow(mode: IdentityProjection.Mode): ProjectionTick = {
    // Until this projection has written everything it planned, the next one projects the whole corpus.
    val previous = last
    last = None
    val started  = tools.Stopwatch.start()
    val phases   = new ProjectionPhases
    val venues   = phases("listings")(if (mode.readsChanged) changedListings() else listings())
    val run      = new Run(mode, previous, started, phases, venues, phases("snapshot")(cache.snapshot()))
    mapping.load() match {
      case Left(why)       => refuse(run, IdentityProjectionMetrics.Refusal.UnreadableMap, why)
      case Right(counters) => resolveAndProject(run, counters)
    }
  }

  /** A refusal: counted, logged, and nothing written. */
  private def refuse(run: Run, reason: IdentityProjectionMetrics.Refusal, why: String,
                     resolution: Option[Resolution] = None, keep: Boolean = false): ProjectionTick = {
    metrics.refused(reason)
    // A refusal writes nothing, so what the last projection left is still what the stored films are of.
    if (keep) last = run.previous
    logger.warn(s"identity projection refused (${reason.label}): $why; the stored films keep serving (${run.phases.render})")
    ProjectionTick(resolution, None, run.listed, 0, 0, 0, Some(why), run.phases.all)
  }

  private def resolveAndProject(run: Run, counters: FilmIdCounters): ProjectionTick =
    Try(run.phases("resolve")(resolve(() => run.read))) match {
      case Failure(crossing: IdentityResolver.FamilyCrossing) =>
        refuse(run, IdentityProjectionMetrics.Refusal.Crossing, crossing.getMessage, keep = true)
      case Failure(other) => throw other
      case Success(None) =>
        refuse(run, IdentityProjectionMetrics.Refusal.NotReady, "the identity model is not ready", keep = true)
      case Success(Some(IdentityProjection.Resolved(modelResolution, held, modelled))) =>
        adopt(modelled)
        projectResolution(run, counters, planned(run, counters, modelResolution, held))
    }

  /** The resolution as projected, the index moved under it, and the drafts of the scope it moved. */
  private def planned(run: Run, counters: FilmIdCounters, modelResolution: Resolution,
                      held: Iterable[ListingKey]): IdentityProjection.Planned = {
    val phases = run.phases
    // what a venue re-published with a poster, days or names the model's decisions do not move with — unknown
    // to a projection with nothing to build on, whose index is read afresh
    val republished = run.previous.map(_ => live.republished(run.byVenue))
    val resolution  = phases("agreement")(agreement(modelResolution, key => run.decided.get(key), republished))
    phases.all.lastOption.foreach(metrics.agreementPhase(run.mode.label, _))
    lastTaken = Some(IdentityProjection.Projected(modelResolution, resolution, { val venues = run.venues
      () => venues.iterator.flatMap(_._2).map(_.listing) }))
    val at          = clock.instant()
    phases("seed")(drafter.seed())
    if (run.previous.isEmpty) { live = new LiveProjectionIndex(normalizer); drafter.forget(); carried = ProjectionScope.Changes.none }
    // Exactly the listings the resolution decided: one that reached the intake after the
    // model's snapshot is projected by the next tick, never left out of its film by this one.
    val updated = phases("index")(live.update(run.byVenue, held, resolution.decisions, run.stored) ++ pageChanges(run))
    val index   = live.index(counters)
    // What moved since the last projection, closed over every film a moved one's draft can move: what a projection
    // of the scope drafts. None when the last one did not write all it planned, the FilmId map is not the one it
    // left, or a stored film is not numbered yet — each re-decided only by projecting the whole corpus.
    val changes = run.previous.filter(p => p.counters == counters.size && index.additions.isEmpty).map { _ =>
      carried ++ updated ++ ProjectionScope.standing(index)
    }
    val moved       = changes.map(c => phases("scope")(ProjectionScope.close(index, c)))
    val reconciling = run.mode.reconciles(scopedSinceWhole, scopedBetweenWhole)
    val reconcile   = moved.isDefined && reconciling
    val freed       = changes.fold(Set.empty[String])(_.keys)
    val (scope, draft, detailed, plan) = drafter.drafted(moved.filterNot(_ => reconcile).getOrElse(ProjectionScope.Whole), freed,
      changes.fold(Set.empty[ListingKey])(_.listings), index, phases)
    IdentityProjection.Planned(resolution, at, updated, index, changes, moved, freed, reconciling, reconcile, scope, draft, detailed, plan)
  }

  /** The listings whose venue page moved since the last projection took them ([[pagesMoved]]): each one's slot is built
   *  from the page as it is read now. Taken before the draft reads a page, so a page moving after is the next one's. */
  private def pageChanges(run: Run): ProjectionScope.Changes = {
    val moved = pagesMoved().map(_.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap)
    if (moved.exists(_.isEmpty)) ProjectionScope.Changes.none
    else ProjectionScope.Changes(run.venues.iterator.flatMap { case (cinema, listings) =>
      // the feed no longer holding every filing since: any page may have moved
      val pages = moved.fold(Option[String => Boolean](_ => true))(_.get(cinema))
      pages.fold(Iterator.empty[ListingKey])(onPage => listings.iterator.map(_.listing.key).filter {
        case ListingKey.Native(_, page, _) => onPage(page)
        case _                             => false
      })
    }.toSet, Set.empty)
  }

  /** The planned projection, held back by the shrink guard or written. */
  private def projectResolution(run: Run, counters: FilmIdCounters,
                                p: IdentityProjection.Planned): ProjectionTick = {
    val (slotCounts, misses) = drafter.slotReport(run.mode.periodic, p.scope, run.phases)
    val scopeStored = if (p.scope.whole) run.stored else run.stored.filter(s => p.scope.films(s.id.value))
    run.phases("guard")(ProjectionGuard.refusal(p.draft, scopeStored, LocalDateTime.ofInstant(p.at, ZoneOffset.UTC), corpus = run.stored)) match {
      case Some(why) if consecutiveShrinks < ProjectionGuard.Grace =>
        consecutiveShrinks += 1
        carried = carried ++ p.updated
        drafter.discard()
        refuse(run, IdentityProjectionMetrics.Refusal.Shrink, s"$why (refusal $consecutiveShrinks of ${ProjectionGuard.Grace})",
          Some(p.resolution), keep = true)
      case shrink =>
        shrink.foreach(why => logger.warn(s"identity projection: $why — held ${ProjectionGuard.Grace} projections, now written"))
        consecutiveShrinks = 0
        writePlanned(run, counters, p, scopeStored)
          .copy(slotsReused = slotCounts._1, slotsBuilt = slotCounts._2, slotMisses = misses, scoped = !p.scope.whole)
    }
  }

  /** Write the planned projection, then hand on its suspect takes, check its drift, and — once every write was taken —
   *  move what the next projection builds on. */
  private def writePlanned(run: Run, counters: FilmIdCounters, p: IdentityProjection.Planned,
                           scopeStored: Seq[StoredMovieRecord]): ProjectionTick = {
    val films = run.stored.size - scopeStored.size + p.plan.films.size
    val tick  = writer(p.resolution, p.detailed, p.plan, run.stored, run.listed, films, run.started, run.phases, patch = !p.scope.whole,
      movedElsewhere = p.changes.fold(Set.empty[String])(_.films))
    // the corpus's suspect takes, of a projection of the whole of it once written — what is served, never what the
    // guard held back — handed to a count of its own, off this projection's path: never a reason to stop one
    if (p.scope.whole && run.mode.periodic) Try(suspects(p.resolution, run.read)).failed.foreach(e =>
      logger.warn("identity projection: handing the suspect takes to their count failed", e))
    if (p.reconciling) metrics.reconciled(measured = p.reconcile)
    if (p.reconcile) p.moved.foreach(drift(_, p.freed, p.index, tick, run.stored))
    // The index holds what was written: a film written counts as moved next time only if another writer moves it. After a
    // declined write the next projection builds it afresh, so it is moved here only to read the change list off it.
    val written = writer.written(tick.changed)
    takeReports(live.written(written, p.plan.retired), p.resolution, run.stored, written, live, trigger, ownTakes,
      fresh = run.previous.isEmpty)
    if (tick.declined == 0) {
      ownTakes = true
      drafter.commit(whole = p.scope.whole)
      carried = ProjectionScope.Changes.none
      last = Some(Last(counters.size + p.plan.counterAdditions.size))
      // The hour between two reconciles is counted in periodic projections; one run on scrapes counts none.
      scopedSinceWhole = if (p.scope.whole) 0 else if (run.mode.periodic) scopedSinceWhole + 1 else scopedSinceWhole
    }
    tick
  }

  /** [[tick]] — the boot's first projection, of the whole corpus — for a caller that must keep running whatever one
   *  projection throws: a failure is counted and logged, never thrown; whether it [[settled]]. */
  def projectQuietly(): Boolean = quietly(Some(tick()))

  /** The hourly projection of the whole corpus (`tick(whole = true)`), a failure logged as [[projectQuietly]]'s is; whether
   *  it [[settled]]. */
  def reconcileQuietly(): Boolean = quietly(Some(tick(whole = true)))

  /** [[tickChanged]], a failure logged as [[projectQuietly]]'s is; whether it [[settled]] (nothing to project is settled). */
  def tickChangedQuietly(): Boolean = quietly(tickChanged())

  private def quietly(run: => Option[ProjectionTick]): Boolean =
    try run.forall(settled)
    catch { case NonFatal(e) =>
      metrics.refused(IdentityProjectionMetrics.Refusal.Failed)
      logger.warn("identity projection failed; the stored films keep serving", e)
      false
    }

}

object IdentityProjection {

  /** A projection's plan before the guard: the resolution as projected (at `at`), what the index moved (`updated`), the
   *  changes and scope they close over (none when the projection is of the whole corpus), and the scope's drafts. */
  private[services] final case class Planned(resolution: Resolution, at: java.time.Instant, updated: ProjectionScope.Changes,
                                             index: ProjectionIndex, changes: Option[ProjectionScope.Changes],
                                             moved: Option[ProjectionScope], freed: Set[String], reconciling: Boolean,
                                             reconcile: Boolean, scope: ProjectionScope, draft: ProjectionDraft,
                                             detailed: ProjectionDraft, plan: ProjectionPlan)
  /** How many periodic projections of a scope run between two of the whole corpus — the reconciliation that would put right
   *  a film a scope missed, and counts it ([[IdentityProjectionMetrics.drifted]]). The worker's projections run on scrapes
   *  and are reconciled by the clock ([[ReconcileEvery]]); this paces [[tick]] alone, which an operator's settle runs. */
  private[services] val ScopedBetweenWhole = 11

  /** How often the worker projects the whole corpus — the reconciliation, the archives' stamps read and the slot
   *  fingerprints recorded. Every other projection runs as the identity model takes this worker's scrapes in. */
  val ReconcileEvery: FiniteDuration = scala.concurrent.duration.Duration(1, java.util.concurrent.TimeUnit.HOURS)

  /** How many take changes one projection logs a line each for; past it, the tick's summary line counts the rest by
   *  kind and basis, and the counter has them all. Ten times the most one tick moved in the week to 2026-10-14 (211,
   *  worker-uk; p50 1, the other countries ≤ 13), so an ordinary bad deploy is logged whole: what a capped line hid
   *  was exactly the drill-down a burst needs. A resolver change re-taking a whole country (~10k listings) still logs
   *  no more than ~2.5 MB of lines. */
  private[services] val TakeChangesLogged = 2000

  /** What asks for a projection of what moved ([[IdentityProjection.asked]]), as its `trigger` names it. */
  enum Trigger(val label: String) {
    /** The identity model took in scrapes that moved it. */
    case Scrape     extends Trigger("scrape")
    /** The agreement filed an answer (a family's, a poster's, a catalogue's, a TMDB find). */
    case Answer     extends Trigger("answer")
    /** Another writer moved a stored film (`MovieCache.onChanged`). */
    case FilmChange extends Trigger("film-change")
  }

  /** What a projection is run as, and so what it reads and does besides projecting. */
  private[services] enum Mode(val label: String) {
    /** The period's projection (and an operator's settle): of what moved, and every [[ScopedBetweenWhole]]-th of the whole
     *  corpus. */
    case Periodic  extends Mode("periodic")
    /** The hourly projection of the whole corpus. */
    case Reconcile extends Mode("whole")
    /** One run as the identity model takes this worker's scrapes in ([[IdentityProjection.tickChanged]]). */
    case Changed   extends Mode("changed")

    /** Whether its listings are read only where this worker's own scrapes moved them. */
    def readsChanged: Boolean = this == Changed
    /** Whether it is one of the period's — the reconcile's included: it records the slot fingerprints, hands a written
     *  projection of the whole corpus's takes to the suspect count, and counts towards the next reconcile. */
    def periodic: Boolean = this != Changed
    /** Whether it projects the whole corpus to reconcile, `scopedSinceWhole` scoped periodic projections having run since
     *  the last: the reconcile always, a periodic one after `between` of them, one run on scrapes never. */
    def reconciles(scopedSinceWhole: Int, between: Int): Boolean = this match {
      case Reconcile => true
      case Periodic  => scopedSinceWhole >= between
      case Changed   => false
    }
  }

  /** What a projection read of the model (`model`) and took after the agreement (`taken`); `read`: the listings it read
   *  (a scoped one's, its scope's), walked afresh by each caller off the projection's thread — the projection's own,
   *  shared, nothing copied. */
  final case class Projected(model: Resolution, taken: Resolution, read: () => Iterator[Listing] = () => Iterator.empty)

  /** A resolution, the listings it decided, and the model's objects of them (none for a resolve of the listings read). */
  final case class Resolved(resolution: Resolution, listings: Iterable[ListingKey], modelled: Seq[Listing] = Nil)
}
