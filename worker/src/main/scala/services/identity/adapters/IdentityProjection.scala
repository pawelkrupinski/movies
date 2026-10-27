package services.identity
package adapters

import services.movies.LatestTitleYear

import java.util.Locale
import scala.util.chaining.scalaUtilChainingOps

import models.{Cinema, CinemaMovie, MovieRecord}
import play.api.Logging
import services.movies.{CacheKey, LeanRecords, ListingKey, CinemaSlotBuilder, FilmId, MovieCache, ScreeningTokens,
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
private[identity] final class ProjectionPhases {
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
 *  5. fetches, BY ID, the TMDB details of a film new to its record (`details`);
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
  /** The log lines of the take changes a projection writes. */
  takeLines:   TakeChangeLines,
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
  /** Handed each written projection's change list, as [[IdentityProjectionMetrics.takesChanged]] is, which must return
   *  at once: a harness's tap. */
  changeFeed:  Seq[TakeChange] => Unit = _ => (),
  /** The venue pages (venue, page) whose read moved since the last call, each given once (`IndexedVenuePageFacts.takeMoved`):
   *  a venue slot is built from its listing's page ([[CinemaSlotBuilder.pages]]), so a listing whose page moved is a change
   *  like one re-scraped — left out, only the hourly projection of the whole corpus rebuilt its slot. Read off the one feed
   *  every input's change comes on; None when it no longer holds every filing since the last call: every listing on a
   *  page is then a change. */
  pagesMoved:  () => Option[Seq[(Cinema, String)]] = () => Some(Nil),
  /** The venue slot memo, made under its environment (`VenueSlotMemo.environment`). */
  slotMemoOf:  Long => VenueSlotMemo = new VenueSlotMemo(_)
) extends Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  private val mapping = new FilmIdMapping(filmIds)
  private var consecutiveShrinks = 0
  /** The venue slots the last projection built, so this one rebuilds only the films whose listings changed. */
  private val slotMemo = slotMemoOf(VenueSlotMemo.environment(normalizer))
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
  /** Each film as last drafted, so a scoped projection works out only the venues of a film that moved. */
  private var shapes = FilmShapes()
  /** The kept index over `counters`: what a spec holds to the index built afresh from the same corpus. */
  private[identity] def keptIndex(counters: FilmIdCounters): ProjectionIndex = synchronized(live.index(counters))

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
      !tick.changed.exists(film => IdentityProjection.awaitsDetails(film.record))

  /** One projection, every line it logs on this thread carrying its id (`projection_id`, the clock's instant at its start),
   *  mode and trigger ([[triggerOf]]) — what ties a tick's take changes, summary and warnings together in VictoriaLogs. */
  private def project(mode: IdentityProjection.Mode): ProjectionTick = synchronized {
    trigger = triggerOf(mode)
    tools.LogFields.within("projection_id" -> clock.instant().toString, "projection_mode" -> mode.label,
      "projection_trigger" -> trigger)(projectNow(mode))
  }

  /** What asked for a projection since the last one began ([[asked]]): drained by the next. */
  private val askedFor = new java.util.concurrent.atomic.AtomicReference(Set.empty[IdentityProjection.Trigger])
  /** Whether this process has projected yet: its first projection is a deploy's (or a pod restart's). */
  private var projected = false
  /** The running projection's trigger ([[triggerOf]]), which its take-change lines name. */
  private var trigger = "deploy"
  /** Whether the stored films hold this process's own takes: once a projection of it wrote all it planned (its first is of
   *  the whole corpus). Until then a take moved on unchanged inputs is the code's ([[TakeCauses]]). */
  private var ownTakes = false

  /** Note what asks for the projection the caller is about to request: a scrape the model took in, an answer filed, a
   *  stored film another writer moved. The next projection names it as its `trigger` ([[triggerOf]]). */
  def asked(trigger: IdentityProjection.Trigger): Unit = { askedFor.updateAndGet(_ + trigger); () }

  /** What set this projection off: `deploy` for the process's first, `periodic` for the hourly reconcile or another tick of
   *  the period, else what [[asked]] for it since the last (`scrape`, `answer`, `film-change`, several joined by `+`), or
   *  `retry` for an unsettled projection run again with nothing new asked. */
  private def triggerOf(mode: IdentityProjection.Mode): String = {
    val first = !projected
    projected = true
    val asked = askedFor.getAndSet(Set.empty)
    if (first) "deploy"
    else if (mode != IdentityProjection.Mode.Changed) "periodic"
    else if (asked.isEmpty) "retry"
    else asked.toSeq.map(_.label).sorted.mkString("+")
  }

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
    phases("seed")(seedSlotMemo())
    if (run.previous.isEmpty) { live = new LiveProjectionIndex(normalizer); shapes = FilmShapes(); carried = ProjectionScope.Changes.none }
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
    val (scope, draft, detailed, plan) = drafted(moved.filterNot(_ => reconcile).getOrElse(ProjectionScope.Whole), freed,
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
    val (slotCounts, misses) = slotReport(run, p.scope)
    val scopeStored = if (p.scope.whole) run.stored else run.stored.filter(s => p.scope.films(s.id.value))
    run.phases("guard")(ProjectionGuard.refusal(p.draft, scopeStored, LocalDateTime.ofInstant(p.at, ZoneOffset.UTC), corpus = run.stored)) match {
      case Some(why) if consecutiveShrinks < ProjectionGuard.Grace =>
        consecutiveShrinks += 1
        carried = carried ++ p.updated
        shapes.discard()
        refuse(run, IdentityProjectionMetrics.Refusal.Shrink, s"$why (refusal $consecutiveShrinks of ${ProjectionGuard.Grace})",
          Some(p.resolution), keep = true)
      case shrink =>
        shrink.foreach(why => logger.warn(s"identity projection: $why — held ${ProjectionGuard.Grace} projections, now written"))
        consecutiveShrinks = 0
        writePlanned(run, counters, p, scopeStored)
          .copy(slotsReused = slotCounts._1, slotsBuilt = slotCounts._2, slotMisses = misses, scoped = !p.scope.whole)
    }
  }

  /** Close the slot memo's tick (recording its fingerprints on a periodic projection) and note what it reused and
   *  built: the reused and built counts, and the misses by cause. */
  private def slotReport(run: Run, scope: ProjectionScope): ((Int, Int), (Int, Int, Int)) = {
    val slotCounts = slotMemo.endTick(retainUnseen = !scope.whole)
    if (run.mode.periodic) run.phases("fingerprints")(recordSlotFingerprints())
    val misses = slotMemo.lastMisses()
    run.phases.note(s"venue slots reused ${slotCounts._1}, built ${slotCounts._2} " +
      s"(rows moved ${misses._1}, priors moved ${misses._2}, new ${misses._3})")
    if (!scope.whole) run.phases.note(s"scope ${scope.films.size} stored films, ${scope.clusters.size} clusters, ${scope.listings.size} listings")
    (slotCounts, misses)
  }

  /** Write the planned projection, then hand on its suspect takes, check its drift, and — once every write was taken —
   *  move what the next projection builds on. */
  private def writePlanned(run: Run, counters: FilmIdCounters, p: IdentityProjection.Planned,
                           scopeStored: Seq[StoredMovieRecord]): ProjectionTick = {
    val films = run.stored.size - scopeStored.size + p.plan.films.size
    val tick  = write(p.resolution, p.detailed, p.plan, run.stored, run.listed, films, run.started, run.phases, patch = !p.scope.whole,
      movedElsewhere = p.changes.fold(Set.empty[String])(_.films))
    // the corpus's suspect takes, of a projection of the whole of it once written — what is served, never what the
    // guard held back — handed to a count of its own, off this projection's path: never a reason to stop one
    if (p.scope.whole && run.mode.periodic) Try(suspects(p.resolution, run.read)).failed.foreach(e =>
      logger.warn("identity projection: handing the suspect takes to their count failed", e))
    if (p.reconciling) metrics.reconciled(measured = p.reconcile)
    if (p.reconcile) p.moved.foreach(drift(_, p.freed, p.index, tick, run.stored))
    // The index holds what was written: a film written counts as moved next time only if another writer moves it. After a
    // declined write the next projection builds it afresh, so it is moved here only to read the change list off it.
    val written = tick.changed.filterNot(f => lastUnwritten(f.id)) ++ lastParked
    reportTakes(live.written(written, p.plan.retired), p.resolution, run.stored, written, fresh = run.previous.isEmpty)
    if (tick.declined == 0) {
      ownTakes = true
      shapes.commit(whole = p.scope.whole)
      carried = ProjectionScope.Changes.none
      last = Some(Last(counters.size + p.plan.counterAdditions.size))
      // The hour between two reconciles is counted in periodic projections; one run on scrapes counts none.
      scopedSinceWhole = if (p.scope.whole) 0 else if (run.mode.periodic) scopedSinceWhole + 1 else scopedSinceWhole
    }
    tick
  }

  /** The drafts of `scope`, detailed and titled. A scope is closed again over every stored film outside it under one of
   *  its films' plain title keys, new or old — the older of two films of one title and year keeps the plain key, so
   *  their keys are decided together — until none is left outside. */
  private def drafted(start: ProjectionScope, freed: Set[String], changed: Set[ListingKey], index: ProjectionIndex,
                      phases: ProjectionPhases): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
    val storedIds = index.storedById.valuesIterator.map(_.id).toSet
    @scala.annotation.tailrec
    def loop(scope: ProjectionScope): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
      val draft    = phases("draft")(IdentityProjectionPlan.draftOf(index, scope, normalizer, slots, tokens, rows, slotMemo, shapes, changed))
      val detailed = phases("details")(draft.copy(drafts = IdentityProjection.detailed(draft.drafts, details)))
      val plan     = phases("finish")(IdentityProjectionPlan.finish(detailed, normalizer, storedIds))
      (if (scope.whole) None else ProjectionScope.widened(index, scope, plan.films.map(_.key), freed, normalizer)) match {
        case Some(wider) => loop(wider)
        case None        => (scope, draft, detailed, plan)
      }
    }
    loop(start)
  }

  /** A reconciling projection of the whole corpus, checked against the scope its changes alone gave it — closed over key
   *  collisions too, read off this projection's own keys, which the scope's drafts would have had — so a film it wrote
   *  or retired outside that scope is one a scoped projection would have left wrong: counted and named, each film written
   *  with what of it moved ([[IdentityProjection.moved]]) — the input the scope did not see. */
  private def drift(moved: ProjectionScope, freed: Set[String], index: ProjectionIndex, tick: ProjectionTick,
                    stored: Seq[StoredMovieRecord]): Unit = tick.plan.foreach { plan =>
    def inside(scope: ProjectionScope, f: ProjectedFilm) = scope.films(f.id.value) || f.members.exists(scope.listings)
    @scala.annotation.tailrec
    def closed(scope: ProjectionScope): ProjectionScope =
      ProjectionScope.widened(index, scope, plan.films.filter(inside(scope, _)).map(_.key), freed, normalizer) match {
        case Some(wider) => closed(wider)
        case None        => scope
      }
    val scope  = closed(moved)
    val before = stored.iterator.filter(r => !scope.films(r.id.value)).map(r => r.id -> r).toMap
    // a film that moved its key alone names both keys: the key a projection derives is the drift to read then
    val missed = tick.changed.filterNot(inside(scope, _)).map(f => f.id.value -> before.get(f.id).fold("new")(s =>
      IdentityProjection.moved(s.record, f.record) match {
        case "key" => s"key ${s.key(normalizer)} -> ${f.key}, titled '${s.title}' -> '${f.title}'"
        case what  => what
      })) ++ plan.retired.map(_.value).filterNot(scope.films).map(_ -> "retired")
    metrics.drifted(missed.size)
    if (missed.nonEmpty) logger.warn(s"identity projection drift: the whole projection changed ${missed.size} film(s) the scoped one " +
      s"would have left: ${missed.sortBy(_._1).take(20).map { case (id, what) => s"$id ($what)" }.mkString(", ")}")
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

  /** The films the last [[write]] could not write as planned. */
  private var lastUnwritten = Set.empty[FilmId]
  /** Of those, the films the store holds parked, as it holds them. */
  private var lastParked = Seq.empty[ProjectedFilm]

  private def write(resolution: Resolution, detailed: ProjectionDraft, plan: ProjectionPlan, stored: Seq[StoredMovieRecord], listings: Int,
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
      .pipe(films => detailed.complete(films, id => before.get(id).map(_.record))).partition(moved))
    if (unmoved.nonEmpty) {
      // a lean slot no build from its rows gives: what the venue slot memo held for it, and what it missed
      val leanOf = drafted.iterator.map(f => f.id -> f.record).toMap
      logger.warn(s"identity projection: ${unmoved.size} film(s) drafted from venue slots a build from their rows does not give, " +
        s"left as stored: ${unmoved.sortBy(_.id.value).take(IdentityProjection.FilmsNamed).map(f =>
          s"${f.id} (${IdentityProjection.rebuilt(leanOf(f.id), f.record)})").mkString(", ")}")
    }
    // A scoped projection writes a film it keeps under its key as only what moved; a projection of the whole corpus writes
    // every changed film whole — the hourly rewrite that puts right anything a patch could not see.
    val patchable: ProjectedFilm => Option[MovieRecord] =
      if (!patch) _ => None else f => before.get(f.id).filter(s => s.key(normalizer) == f.key).map(_.record)
    val (unwritten, parked) = phases("writes")(writeAll(changed, plan.retired, IdentityProjection.independent(changed, plan.films, stored, normalizer),
      patchable))
    val declined  = unwritten.size
    // A film now identified otherwise — by TMDB, or, one TMDB has no record of, by the fallback source's IMDb id — once
    // its TMDB answer is whole: announced while its details were still to come (TMDB failing), its IMDb-id recovery
    // searched without the film's original title or director and was never asked again once they arrived.
    changed.filter(f => !IdentityProjection.awaitsDetails(f.record) && before.get(f.id).forall(s =>
      IdentityProjection.awaitsDetails(s.record) || s.record.tmdbId != f.record.tmdbId || s.record.imdbId != f.record.imdbId)).foreach { f =>
      Try(announce(CacheKey.stored(f.title, f.key), f.record)).failed.foreach(e => logger.warn(s"identity projection: announcing ${f.id} (${f.title}) failed", e))
    }
    val seconds = started.seconds
    metrics.projected(films, listings, plan.regroupings, seconds)
    lastUnwritten = unwritten
    lastParked    = parked
    val tick = ProjectionTick(Some(resolution), Some(plan), listings, changed.size - declined, plan.retired.size, declined, None, phases.all,
      changed = changed, backlog = agreementBacklog())
    tools.LogFields.info(logger.underlyingLogger, f"identity projection: $listings listings → $films films; wrote ${tick.written}, retired ${tick.retired}, " +
        s"declined $declined; ${plan.regroupings}" +
        f" in $seconds%.1fs (${phases.render})",
      Seq("listings" -> listings, "films" -> films, "written" -> tick.written, "retired" -> tick.retired, "declined" -> declined,
        "seconds" -> math.round(seconds * 10) / 10.0, "scoped" -> patch,
        // which films, a handful: what a retire or decline burst is drilled into by
        "retired_films" -> Option.when(plan.retired.nonEmpty)(plan.retired.take(IdentityProjection.FilmsNamed).map(_.value).mkString(" ")),
        "declined_films" -> Option.when(unwritten.nonEmpty)(unwritten.take(IdentityProjection.FilmsNamed).map(_.value).mkString(" "))))
    tick
  }

  /** The projection's CHANGE LIST ([[LiveProjectionIndex.written]]: each listing whose stored film the writes moved to
   *  another TMDB film, or placed on one for the first time — what the site shows for it, read off the store as written,
   *  so a film the store declined changes no take until a later projection writes it), each with the basis of the
   *  decision it replaced and of the one that holds it now, and why it moved ([[TakeCauses]]; `fresh`: the index was built
   *  afresh for this projection, so it knows no listing as re-published). Handed whole to the metrics and to [[changeFeed]]; its match
   *  changes ([[TakeChange.moved]]) logged one line each (the first [[IdentityProjection.TakeChangesLogged]]) for
   *  drill-down, and summed up in one line by kind and basis; the lines and their fields are [[TakeChangeLines]]'. The
   *  country, build and projection id are every worker log line's own fields. */
  private def reportTakes(changed: Seq[TakeChange], resolution: Resolution, stored: Seq[StoredMovieRecord],
                          written: Seq[ProjectedFilm], fresh: Boolean): Unit =
    if (changed.nonEmpty) {
      val wanted    = changed.iterator.map(_.listing).toSet
      val decisions = resolution.decisions.iterator.flatMap(d => d.members.iterator.filter(wanted).map(_ -> d)).toMap
      lazy val before = stored.iterator.map(r => r.id -> r).toMap
      val reading   = TakeCauses.Reading(id => before.get(FilmId(id)), k => live.listing(k).isDefined, live.publishedAt,
        Option.unless(fresh)(live.lastRepublished), k => decisions.get(k).fold(Seq.empty[ListingKey])(_.members), trigger, ownTakes)
      val changes   = changed.map { t =>
        val prior = live.priorTake(t.listing)
        val take  = t.copy(basisBefore = prior.map(_.basis), basisAfter = decisions.get(t.listing).map(_.basis), prior = prior,
          published = live.listing(t.listing), formerly = live.formerly(t.listing), trigger = Some(trigger))
        take.copy(cause = TakeCauses.of(take, reading))
      }
      metrics.takesChanged(changes)
      changeFeed(changes)
      val takes     = changes.filter(_.moved)
      if (takes.nonEmpty) {
        val moved  = takes.iterator.map(_.listing).toSet
        val now    = written.iterator.flatMap(f => f.members.iterator.filter(moved).map(_ -> f)).toMap
        val logged = takes.take(IdentityProjection.TakeChangesLogged)
        logged.foreach { t =>
          log(takeLines.of(t, t.previousFilm.flatMap(id => before.get(FilmId(id))).map(_.title),
            now.get(t.listing).map(f => f.id.value -> f.title), decisions.get(t.listing), live.listing(t.listing),
            live.priorTake(t.listing), Some(trigger)))
        }
        log(takeLines.summary(takes, logged.size))
      }
    }

  private def log(line: TakeChangeLines.Line): Unit = tools.LogFields.info(logger.underlyingLogger, line.message, line.fields)

  /** Write `films` and retire `retired`, in the order the store's unique `key` / `tmdbId` indexes
   *  allow: a film whose key or TMDB id another document still holds waits until the retirements
   *  and the other writes have freed it; a cycle between two kept films (each holding the other's
   *  key) is broken by parking one under a key and id of its own first. Returns the films not written as planned
   *  (declined, or parked without their TMDB id) — the next projection tries them again — and, of those, the ones the
   *  store holds parked, as it holds them.
   *
   *  The `independent` films — new ones whose key and TMDB id nothing else holds or takes — are
   *  written side by side first: no order among them, or with the rest, can decide anything, and
   *  one at a time a country's first projection waited on ~2,250 films' round-trips in a row (a US
   *  boot). The rest keep the plan's order. */
  private def writeAll(films: Seq[ProjectedFilm], retired: Seq[FilmId], independent: Set[FilmId],
                       patchable: ProjectedFilm => Option[MovieRecord]): (Set[FilmId], Seq[ProjectedFilm]) = {
    def write(f: ProjectedFilm): Boolean = {
      val key = CacheKey.stored(f.title, f.key)
      patchable(f).fold(cache.writeProjected(f.id, key, f.record))(cache.patchProjected(f.id, key, _, f.record, f.touched)) == WriteOutcome.Written
    }
    def attempt(fs: Seq[ProjectedFilm]): Seq[ProjectedFilm] = fs.filterNot(write)
    val (apart, ordered) = films.partition(f => independent(f.id))
    val unwritten = tools.BoundedParallel.map("identity-projection-writes", apart, IdentityProjection.WriteConcurrency)(f => Option.unless(write(f))(f))
    val first = unwritten.flatten ++ attempt(ordered)
    retired.foreach { id =>
      val outcome = cache.retireProjected(id)
      if (outcome != WriteOutcome.Written) tools.LogFields.warn(logger.underlyingLogger, s"identity projection: retiring $id: $outcome",
        Seq("film" -> id.value, "outcome" -> outcome.toString))
    }
    val second = attempt(first)
    val parked = second.map(f => f.copy(key = s"~${f.id.value}|", record = f.record.copy(tmdbId = None), touched = None))
      .filter(f => cache.writeProjected(f.id, CacheKey.stored(f.title, f.key), f.record) == WriteOutcome.Written)
    val last = attempt(second)
    if (last.nonEmpty) tools.LogFields.warn(logger.underlyingLogger, s"identity projection: ${last.size} film(s) not written " +
      s"(e.g. ${last.head.id} under ${last.head.key}); ${parked.size} parked; the next projection retries",
      Seq("declined" -> last.size, "parked" -> parked.size,
        "declined_films" -> last.take(IdentityProjection.FilmsNamed).map(f => s"${f.id.value}=${f.key}").mkString(" ")))
    val declined = last.map(_.id).toSet
    (declined, parked.filter(f => declined(f.id)))
  }
}

object IdentityProjection {

  /** A projection's plan before the guard: the resolution as projected (at `at`), what the index moved (`updated`), the
   *  changes and scope they close over (none when the projection is of the whole corpus), and the scope's drafts. */
  private[identity] final case class Planned(resolution: Resolution, at: java.time.Instant, updated: ProjectionScope.Changes,
                                             index: ProjectionIndex, changes: Option[ProjectionScope.Changes],
                                             moved: Option[ProjectionScope], freed: Set[String], reconciling: Boolean,
                                             reconcile: Boolean, scope: ProjectionScope, draft: ProjectionDraft,
                                             detailed: ProjectionDraft, plan: ProjectionPlan)
  /** What of a film moved from `was` to `now`, briefly: its own fields (by name), and the sources whose slots differ
   *  ([[LeanRecords.slotsEqual]]), the first few named, else its key — what a drift line names each film it missed by. */
  private[identity] def moved(was: MovieRecord, now: MovieRecord): String = {
    val fields  = was.productElementNames.zip(was.productIterator.zip(now.productIterator))
      .collect { case (name, (a, b)) if name != "data" && a != b => name }.toSeq
    val sources = (was.data.keySet ++ now.data.keySet).toSeq.filterNot(s => (was.data.get(s), now.data.get(s)) match {
      case (Some(a), Some(b)) => LeanRecords.slotsEqual(a, b)
      case _                  => false
    }).map(_.toString).sorted
    val named   = fields ++ sources.take(3) ++ Option.when(sources.size > 3)(s"+${sources.size - 3} slots")
    if (named.isEmpty) "key" else named.mkString(" ")
  }

  /** The venue slots of `lean`, a film as drafted, that a build from their rows gave otherwise (`built`), each with the
   *  fields it differs in — `showtimes` for their digest — the first few named: the input a slot's memo key misses. */
  private[identity] def rebuilt(lean: MovieRecord, built: MovieRecord): String = {
    val slots = lean.data.toSeq.flatMap { case (source, was) =>
      built.data.get(source).filterNot(LeanRecords.slotsEqual(was, _)).map { now =>
        val fields = was.productElementNames.zip(was.productIterator.zip(now.productIterator)).collect {
          case (name, (a, b)) if !Set("showtimes", "showtimesDigest", "showtimeStartMinutes")(name) && a != b => name
        }.toSeq ++ Option.when(services.movies.ShowtimesDigest.slotDigest(was) != services.movies.ShowtimesDigest.slotDigest(now))("showtimes")
        s"$source: ${fields.mkString(" ")}"
      }
    }.sorted
    (slots.take(3) ++ Option.when(slots.size > 3)(s"+${slots.size - 3} slots")).mkString("; ")
  }

  /** Whether `record` names a TMDB film whose details — its cross-reference ids among them — TMDB has not given yet. */
  private[identity] def awaitsDetails(record: MovieRecord): Boolean = record.tmdbId.isDefined && !record.data.contains(models.Tmdb)

  /** How many films' TMDB details a projection fetches at once — within what TMDB tolerates (see the
   *  external-api-rate-limits budgets). */
  private[identity] val DetailsConcurrency = 8

  /** Each draft whose film is new to its record, with that film's TMDB details — fetched side by
   *  side, since each is its own film's: one at a time, a country's first projection waited on
   *  ~2,250 of them in a row (a US boot). The details builder owns the TMDB side; what the projection
   *  derived stays the projection's, and a fetch that fails leaves its draft as it was. */
  private[identity] def detailed(drafts: Seq[FilmDraft], details: (MovieRecord, Int) => Option[MovieRecord]): Seq[FilmDraft] =
    tools.BoundedParallel.map("identity-projection-details", drafts, DetailsConcurrency) { d =>
      // Details can move any field and source of the record: the film is compared and written whole.
      d.needsDetails.fold(d)(film => Try(details(d.record, film)).toOption.flatten.fold(d)(r =>
        d.copy(record = r.copy(searchTitle = d.record.searchTitle, retainedSynopses = d.record.retainedSynopses), touched = None)))
    }

  /** How many periodic projections of a scope run between two of the whole corpus — the reconciliation that would put right
   *  a film a scope missed, and counts it ([[IdentityProjectionMetrics.drifted]]). The worker's projections run on scrapes
   *  and are reconciled by the clock ([[ReconcileEvery]]); this paces [[tick]] alone, which an operator's settle runs. */
  private[identity] val ScopedBetweenWhole = 11

  /** How often the worker projects the whole corpus — the reconciliation, the archives' stamps read and the slot
   *  fingerprints recorded. Every other projection runs as the identity model takes this worker's scrapes in. */
  val ReconcileEvery: FiniteDuration = scala.concurrent.duration.Duration(1, java.util.concurrent.TimeUnit.HOURS)

  /** How many take changes one projection logs a line each for; past it, the tick's summary line counts the rest by
   *  kind and basis, and the counter has them all. Ten times the most one tick moved in the week to 2026-10-14 (211,
   *  worker-uk; p50 1, the other countries ≤ 13), so an ordinary bad deploy is logged whole: what a capped line hid
   *  was exactly the drill-down a burst needs. A resolver change re-taking a whole country (~10k listings) still logs
   *  no more than ~2.5 MB of lines. */
  private[identity] val TakeChangesLogged = 2000

  /** How many films a projection's summary and warnings name, of those it retired or could not write. */
  private[identity] val FilmsNamed = 50

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
  private[identity] enum Mode(val label: String) {
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

  /** The incremental model, brought up to now on its own thread — no resolve here. */
  def modelled(model: IdentityModelService, timeout: FiniteDuration): (() => Seq[Listing]) => Option[Resolved] = _ =>
    model.current(timeout).map(snapshot => Resolved(snapshot.resolution, snapshot.listings.view.map(_.key), snapshot.listings))
}
