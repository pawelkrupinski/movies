package services.identity
package adapters

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.movies.TitleNormalizer

import java.util.concurrent.{ConcurrentHashMap, ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/** The model as a reader sees it: its resolution, what its lookups do not know yet, its listings. */
final case class ModelSnapshot(resolution: Resolution, gaps: AnswersChanged, listings: Seq[Listing],
                               questions: Seq[(Set[CandidateQuery], Boolean)] = Nil)

/** What the model did with one drained batch of events. */
final case class ModelBatch(venues: Int, observations: Int, familiesResolved: Int, families: Int, seconds: Double,
                            sizes: IncrementalResolver.FamilySizes) {
  /** Whether it moved anything a projection reads: a venue's listings, or a family decided again. */
  def moved: Boolean = venues > 0 || familiesResolved > 0
}

/** The model's gauges; the Prometheus ones live in `services.metrics`. */
trait IdentityModelMetrics {
  def batch(batch: ModelBatch): Unit
  def rebuilt(): Unit
  /** A take-up (at boot, a retry, or a rebuild) failed: the model is down until one succeeds. */
  def takeUpFailed(): Unit
  /** The input gate holds `count` listings, the oldest waiting `oldestSeconds` (0 with none): after every drain and take-up. */
  def held(count: Int, oldestSeconds: Double): Unit = ()
  /** Held listings taken in, each after waiting so many seconds; `atBoot`: by a take-up, its answers filed while down. */
  def promoted(heldSeconds: Seq[Double], atBoot: Boolean): Unit = ()
  /** Held listings taken in past the hold's bound, its questions still open: the backstop, never the rule. */
  def bounded(heldSeconds: Seq[Double]): Unit = ()
  /** After every drain and take-up: the listings billed (`present`), the departed ones the model holds as kept, and the
   *  departed ones the store holds as inputs ([[ListingRetention]]): the retention window's cost, watched before it bites. */
  def retention(present: Int, kept: Int, departed: Int): Unit = ()
  /** Departed listings deleted from the store as twins of a listing billed again under another key
   *  ([[ListingRetention.returnedUnderAnotherKey]]): `atBoot` by a take-up, else by a drain. */
  def twinsDeleted(count: Int, atBoot: Boolean): Unit = ()
}
object IdentityModelMetrics {
  val Silent: IdentityModelMetrics = new IdentityModelMetrics {
    def batch(batch: ModelBatch): Unit = (); def rebuilt(): Unit = (); def takeUpFailed(): Unit = ()
  }
}

/**
 * The identity model of one country, kept current by the pipeline's own events instead of a
 * whole-corpus resolve on a schedule (docs/design/identity-resolver.md §20):
 *
 *  - a venue's archived scrape ([[venueScraped]]) — its listings now, diffed against what the model
 *    holds there: a listing it no longer lists leaves the model, so a film nobody shows any more
 *    is swept by the event that stopped showing it;
 *  - new content filed under a key a question reads ([[observed]]: a TMDB store document, a venue
 *    page, a proposal) — mapped back through
 *    [[ObservationReads]] to the questions that read it.
 *
 * Events are queued from any thread and drained on ONE thread a moment after they arrive ([[wake]]: a burst together,
 * within `settle` of its first event — no period runs between events), as one
 * [[IncrementalResolver.batch]], so a family several venues touch in that window is resolved once.
 * On start it takes up the model its store kept ([[IncrementalResolver.restore]]) over the
 * archive's listings. A drain that fails leaves the engine half-updated, so the model is rebuilt
 * from its store rather than carried on. Readers on other threads ask the model's thread for a
 * snapshot ([[current]], [[peek]]), built only when asked.
 */
final class IdentityModelService(
  newModel:   () => IncrementalResolver,
  reads:      ObservationReads,
  archive:    () => Seq[Listing],
  normalizer: TitleNormalizer,
  settle:     FiniteDuration,
  scheduler:  ScheduledExecutorService,
  metrics:    IdentityModelMetrics = IdentityModelMetrics.Silent,
  /** Told of each batch the model drained: what moved, for whoever projects the model ([[EventTrigger]]). */
  batched:    ModelBatch => Unit = _ => (),
  // What the model's lookups report of their reading (`TrackedLookups.render`), for the take-up log.
  reading:    () => String = () => "",
  /** Run on the model's thread before each drain: what turns queued announcements into observed keys
   *  (`VenuePageIndex.settle`), so the drain that follows takes them in. One that throws is logged, and
   *  the drain goes on: it must leave what it could not read for its next run. */
  beforeDrain: () => Unit = () => (),
  /** Which listings' venue pages are unread, each asked for as it is taken in or taken up (a cut-over country). */
  pageWait:   PageWait = PageWait.Never,
  /** Told each time the model is taken up — at boot, and a take-up retried after one failed: the first moment a
   *  projection of it can run (the boot's, which every projection on scrapes waits for). On the model's thread. */
  takenUp:    () => Unit = () => (),
  clock:      java.time.Clock,
  /** Where each listing the model takes in, re-published with another field or poster, or lets go is kept, before and
   *  after ([[HistoryEntry.Listing]]), by the build `build`: what a replay of a moment's takes resolves. */
  history:    IdentityHistory = IdentityHistory.Discard,
  build:      String = "",
  /** Every listing that left, with its last-seen facts and departure ([[ListingRetention.Departed]]), persisted: the
   *  input the kept listings are read from on every drain and take-up ([[ListingRetention.retainedAt]]). */
  retainedStore: RetainedListingStore = new RetainedListingStore.InMemory(),
  /** The listings the history last saw the model hold ([[RetainedListingStore.fromHistory]]): what a take-up reads
   *  the departed listings from when the store was never written. */
  heldBefore: () => Seq[Listing] = () => Nil,
  /** The model's input gate (docs/design/identity-structural-fixes.md, fix 1): a listing's version is taken in only once
   *  every question its own facts put to the film databases' families is answered or given up on. */
  readiness:  ListingReadiness = ListingReadiness.Always,
  /** The listings the gate holds, persisted: what a take-up resumes ([[PendingListingStore]]). */
  pendingStore: PendingListingStore = new PendingListingStore.InMemory(),
  /** Whether a listing new to the model, its version held, is shown meanwhile on its own, Pending -- or not at all. */
  newWhilePending: IdentityModelService.NewWhilePending = IdentityModelService.NewWhilePending.Default,
  /** The backstop: a listing held this long is taken in regardless, and counted (`promoted{at="bound"}`). */
  maxHold: java.time.Duration = ModelInputGate.MaxHold,
  /** How long a departed listing stays an input ([[ListingRetention.live]]): the worker's typed config
   *  (`settings.IdentityRetentionWindow`), [[ListingRetention.DefaultMaxAge]] unless set. */
  retentionWindow: java.time.Duration = ListingRetention.DefaultMaxAge
) extends Logging {

  private val venues       = new ConcurrentHashMap[String, Seq[Listing]]()
  private val observations = new ConcurrentHashMap[String, ObservationReads.Change]()
  private var model: Option[IncrementalResolver] = scala.None
  /** The model's input gate (fix 1): the listings whose newest version is held on their own questions, read and moved on
   *  the model's thread. */
  private val inputGate = new ModelInputGate(readiness, pendingStore, clock, metrics, maxHold)

  /** A venue's scrape was archived with `films`: its listings now, read as the take-up reads the archive's
   *  ([[ModelRead]]). Read whole, a venue scraped since boot fired rules a venue taken up lean never did — and the
   *  measurement, replaying recordings, could not tell which prod held. */
  def venueScraped(cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
    venues.put(cinema.displayName, ModelRead.corpus(Seq(cinema -> films), normalizer)); wake()
  }

  /** New content was filed under `key` (the venue page index, the proposal index): every question that reads it is
   *  asked again. */
  def observed(key: String): Unit = queue(key, reads.announced())
  /** New content was filed under `key` in the TMDB store, by whichever question's ask fetched it, if one did: that
   *  question has read what it filed, and the questions asked since read it too, so only those asked before it are
   *  asked again ([[ObservationReads.changedBy]]). A take-up over an empty store files every answer it asks for, and
   *  each woke its own question: its first drain resolved every family a second time (Germany sample, 2026-10-14). */
  def filed(key: String): Unit = queue(key, reads.changed())
  private def queue(key: String, change: ObservationReads.Change): Unit = { observations.merge(key, change, (held, now) => held.merge(now)); wake() }

  /** Drain soon: `quiet` after the last event of a burst, never more than `settle` after its first — so a burst several
   *  venues land in is resolved together, as the fixed `settle` period batched it, and an event alone is taken in within
   *  seconds. Nothing runs between events: there is no period, only the deadlines a drain leaves ([[drainAndReschedule]]).
   *  Called by every event this model queues, and by whoever files what a drain reads (`VenuePageIndex.pageRead`). */
  def wake(): Unit = wakeAfter(None)

  private val wakeLock = new Object
  private var wakeDue: Option[(java.util.concurrent.ScheduledFuture[?], Long)] = scala.None
  private var burstSince = 0L
  /** A drain at the next event's debounce, or at `at` (a deadline) when that is sooner than any drain already due. */
  private def wakeAfter(at: Option[java.time.Instant]): Unit = wakeLock.synchronized {
    val now = clock.millis()
    val due = at.fold {
      if (wakeDue.isEmpty) burstSince = now
      math.min(now + IdentityModelService.Quiet.min(settle).toMillis, burstSince + settle.toMillis)
    }(_.toEpochMilli)
    if (wakeDue.forall { case (_, dueAt) => at.isEmpty || due < dueAt }) {
      wakeDue.foreach(_._1.cancel(false))
      val run: Runnable = () => { wakeLock.synchronized { wakeDue = scala.None }; drainAndReschedule() }
      wakeDue = scala.util.Try(scheduler.schedule(run, math.max(0L, due - now), TimeUnit.MILLISECONDS)).toOption.map(_ -> due)
    }
  }

  /** The model brought up to NOW — taken up if it is not yet, every queued event drained — on its
   *  own thread: what a projection reads. `None` when it cannot be had within `timeout` (a rebuild
   *  still running) or the model could not be taken up. */
  def current(timeout: FiniteDuration): Option[ModelSnapshot] = onModel(timeout) {
    // A take-up is tried here only if none was yet; one that failed is retried by [[tick]] alone, on its
    // backoff — never again at once, which held the model's thread for a second full take-up
    // (117–259 s on US) per refused projection, with nothing to drain meanwhile.
    if (model.isEmpty && retryAt.isEmpty) tryTakeUp()
    catchUp()
    model.map(snapshotOf)
  }

  /** Each listing's family, the model brought up to now as [[current]] brings it — which a snapshot's resolution does
   *  not carry. `None` as [[current]]'s. */
  def familyOf(timeout: FiniteDuration): Option[Map[services.movies.ListingKey, Int]] = onModel(timeout) {
    catchUp()
    model.map(_.familyOf)
  }

  /** The model caught up with what queued, if it has been taken up — never a take-up: what the
   *  fill reads, which must not wait on one. */
  def peek(timeout: FiniteDuration): Option[ModelSnapshot] = onModel(timeout) {
    catchUp()
    model.map(snapshotOf)
  }

  // A snapshot is built only when read, on the model's thread — never per drain: on the US corpus
  // one is ~100k listings' worth of maps, and a drain runs every few seconds.
  private def snapshotOf(engine: IncrementalResolver) = {
    val resolution = engine.resolution
    val shown      = newWhilePending.shown(inputGate.heldNew)
    if (shown.isEmpty) ModelSnapshot(resolution, engine.gaps, engine.listings, QuestionRenewals.of(engine.familyQuestions))
    else ModelSnapshot(resolution.copy(decisions = resolution.decisions ++ shown.map(IdentityModelService.waiting)), engine.gaps,
      engine.listings ++ shown.map(_.newest), QuestionRenewals.of(engine.familyQuestions))
  }

  // A request its reader gave up on is cancelled: one the model's thread has not reached yet never runs
  // (a take-up and a snapshot for nobody, delaying the next reader), one it is running finishes.
  private def onModel[A](timeout: FiniteDuration)(body: => Option[A]): Option[A] =
    scala.util.Try(scheduler.submit[Option[A]](() => body)).toOption.flatMap { request =>
      try request.get(timeout.toMillis, TimeUnit.MILLISECONDS)
      catch { case NonFatal(_) => request.cancel(false); scala.None }
    }

  @volatile private var tookUp = false
  /** Whether the take-up [[start]] scheduled has finished — taken up, or failed and left to [[tick]]
   *  to retry: the boot work a worker's readiness waits on. */
  def takeUpSettled: Boolean = tookUp

  def start(): Unit = {
    scheduler.execute(() => try tryTakeUp() finally { tookUp = true; drainAndReschedule() })
    ()
  }

  /** A drain, then the one deadline it leaves, if any: a failed take-up's retry, or — a settle of announced pages
   *  failed — another try one `settle` on. */
  private def drainAndReschedule(): Unit = {
    tick()
    val takeUpRetry = if (model.isEmpty) retryAt else scala.None
    val settleRetry = Option.when(settleFailed)(clock.instant().plusMillis(settle.toMillis))
    (takeUpRetry ++ settleRetry).minOption.foreach(at => wakeAfter(Some(at)))
  }
  @volatile private var settleFailed = false

  // When a model that could not be taken up is next tried, and how long the wait after that one grows to.
  // Touched only on the model's thread.
  private var retryAt: Option[java.time.Instant] = scala.None
  private var retryDelay: FiniteDuration          = IdentityModelService.FirstRetry

  /** What the scheduler runs every `settle`: a take-up retried once its backoff has passed, when the
   *  last one failed — [[current]] tries a take-up only before the first has run, and [[peek]] never
   *  does, so nothing else takes the model up again — then a drain. */
  private[identity] def tick(): Unit = {
    if (model.isEmpty && retryAt.exists(at => !clock.instant().isBefore(at))) tryTakeUp()
    safely("drain")(drain())
  }

  private def tryTakeUp(): Unit =
    try takeUp() catch { case NonFatal(e) => logger.error(s"identity model: take-up failed: $e", e); takeUpFailed() }

  private def takeUpFailed(): Unit = {
    metrics.takeUpFailed()
    model   = scala.None
    retryAt = Some(clock.instant().plusMillis(retryDelay.toMillis))
    logger.warn(s"identity model: down — take-up retried in ${retryDelay.toSeconds}s")
    retryDelay = (retryDelay * 2).min(IdentityModelService.MaxRetry)
  }

  /** Drain what has queued, on the calling thread — the scheduler's, or a test's. */
  def drain(): Option[ModelBatch] = model.flatMap { engine =>
    // A settle that fails (a venue page's read timing out) has touched no engine state and leaves its pages
    // noted for the next one (`VenuePageIndex.settle`): this drain goes on without them. Thrown on, it reached
    // `safely`, which rebuilt the whole model from its store — minutes on US — over one read.
    settleFailed = false
    try beforeDrain()
    catch { case NonFatal(e) =>
      settleFailed = true
      logger.warn(s"identity model: settling announced pages failed — retried at the next drain: $e", e)
    }
    val scraped = venues.keySet.asScala.toSeq.flatMap(venue => Option(venues.remove(venue)).map(venue -> _))
    val changes = observations.keySet.asScala.toSeq.flatMap(key => Option(observations.remove(key)).map(key -> _)).toMap
    admit(engine, scraped)
    val (gated, promoted) = inputGate.admit(engine, scraped)
    Option.when(scraped.nonEmpty || changes.nonEmpty || promoted.nonEmpty) {
      val started = tools.Stopwatch.start()
      val before  = engine.familiesResolved
      val scrapedSeen = gated ++ promoted
      // a held listing's stand-in (its version under the key its venue billed it by before) stays until it is taken in
      val standing    = inputGate.standing
      val leaving     = scraped.flatMap { case (venue, now) => engine.heldAt(venue) -- now.map(_.key) -- standing } ++
        inputGate.takeReleasedStandIns().filterNot(standing)
      // a listing leaving loses its showings, its facts kept while a present listing of the country bills its title -- read
      // afresh from every listing that left on each drain; one back replaces what was kept of it (ListingRetention)
      val (seen, gone) = retaining(engine, scrapedSeen, leaving)
      kept(engine, seen, gone)
      engine.batch(seen, gone, reads.changedBy(changes))
      val batch = ModelBatch(scraped.size, changes.size, engine.familiesResolved - before, engine.familyCount, started.seconds,
        engine.sizes)
      metrics.batch(batch)
      reportRetention(engine)
      batched(batch)
      batch
    }
  }

  private def reportRetention(engine: IncrementalResolver): Unit =
    metrics.retention(engine.heldCount - retained.size, retained.size, departed.size)

  /** Every listing that left, with its last-seen facts and when it left ([[ListingRetention.Departed]]): an INPUT,
   *  persisted in `retainedStore` as it moves, and read back by a take-up ([[resumed]]). */
  private val departed = new ConcurrentHashMap[services.movies.ListingKey, ListingRetention.Departed]()
  /** The departed listings the model holds as kept now: a cache of [[ListingRetention.retainedAt]] of the inputs, never
   *  a decision of its own -- each drain recomputes it whole and moves the model by the difference. */
  private val retained = java.util.concurrent.ConcurrentHashMap.newKeySet[services.movies.ListingKey]()
  /** Each held listing's title as [[ListingRetention]] reads it -- by the one title relation ([[TitleRelation]]) -- by the
   *  listing object it was read off. */
  private val titles = new java.util.WeakHashMap[Listing, TitleRelation.Forms]()
  private def formsOf(listing: Listing): TitleRelation.Forms = titles.synchronized {
    Option(titles.get(listing)).getOrElse { val t = TitleRelation.of(listing, normalizer); titles.put(listing, t); t }
  }

  /** `seen` and the keys `leaving` as the model is handed them under [[ListingRetention]], a pure function of the inputs
   *  (R1, no stickiness): a listing back is billed again and is no departed one; a billed listing leaving is departed,
   *  with its facts as last held and the drain's time; a departed one past the retention window is no input. Then
   *  the listings kept are recomputed WHOLE from the departed and the billed ones, whatever order they came in -- one a
   *  related arrival bills again is kept again -- and the model moves by the difference from what it held as kept. */
  private def retaining(engine: IncrementalResolver, seen: Seq[Listing],
                        leaving: Iterable[services.movies.ListingKey]): (Seq[Listing], Seq[services.movies.ListingKey]) = {
    val now      = clock.millis()
    val seenKeys = seen.iterator.map(_.key).toSet
    val back     = seenKeys.filter(departed.containsKey)
    back.foreach(departed.remove)
    // a listing held as kept that its venue's page no longer names left before: it keeps the departure it has
    val leftAll  = leaving.iterator.filterNot(k => retained.contains(k) || seenKeys(k)).flatMap(k => engine.held(k).map(k -> _))
      .map { case (k, listing) => k -> ListingRetention.Departed(listing, now) }.toMap
    // a departed listing its own venue bills again under another key (a lent page and year, another case) is back, not
    // departed: one that leaves as its twin arrives is never written, one already stored is deleted -- that one alone, so
    // the delete never reaches the archive's expired row of an earlier departure under a key leaving now
    val twins    = if (seen.isEmpty || (departed.isEmpty && leftAll.isEmpty)) Set.empty[services.movies.ListingKey]
                   else ListingRetention.returnedUnderAnotherKey((departed.values.asScala ++ leftAll.values).map(_.listing).toSeq, seen)
                     .iterator.map(_.key).toSet
    val left     = leftAll -- twins
    val stored   = twins.filter(departed.containsKey)
    stored.foreach(departed.remove)
    if (stored.nonEmpty) deleteTwins(stored, atBoot = false)
    left.foreach { case (k, d) => departed.put(k, d) }
    val aged     = departed.asScala.collect { case (k, d) if !ListingRetention.live(d, now, retentionWindow) => k }.toSeq
    aged.foreach(departed.remove)
    val going    = leaving.toSet
    if (departed.isEmpty && retained.isEmpty) {
      if (back.nonEmpty || aged.nonEmpty) persist(retainedStore.update(Nil, back ++ aged))
      (seen, going.toSeq)
    } else {
      val present  = engine.listings.iterator.filterNot(l => going(l.key) || retained.contains(l.key) || seenKeys(l.key)).toSeq ++ seen
      val kept     = ListingRetention.retainedAt(departed.values.asScala.toSeq.sortBy(d => services.movies.ListingKey.serialised(d.listing.key)),
        present, now, normalizer, formsOf, retentionWindow)
      val keptKeys = kept.iterator.map(_.key).toSet
      val newly    = kept.filterNot(l => retained.contains(l.key))
      val dropped  = (going ++ retained.asScala).filterNot(k => keptKeys(k) || seenKeys(k)).toSeq
      retained.clear(); keptKeys.foreach(retained.add)
      if (back.nonEmpty || left.nonEmpty || aged.nonEmpty) persist(retainedStore.update(left.values.toSeq, back ++ aged))
      (seen ++ newly, dropped)
    }
  }

  /** The kept set a take-up resumes, against `present` (the listings billed now): [[ListingRetention.retainedAt]] of the
   *  departed listings the store holds -- the history's last held listings when it was never written. A departed listing
   *  persisted before departures were dated ([[ListingRetention.Unknown]]) is dated to this take-up and written back so
   *  (the store's migration: nothing it held is lost). Nothing billed read, or the store unread, resumes none and writes
   *  nothing: no read is a reason to forget an input. */
  private def resumed(present: Seq[Listing]): Seq[Listing] = {
    retained.clear(); departed.clear()
    if (present.isEmpty) Nil
    else scala.util.Try(retainedStore.read()).fold(
      failed => { logger.warn(s"identity model: the departed listings could not be read -- none resumed: $failed"); Nil },
      read => {
        val now     = clock.millis()
        val stored  = read.getOrElse(heldBefore().map(ListingRetention.Departed(_, ListingRetention.Unknown)))
        val dated   = stored.map(d => if (d.at == ListingRetention.Unknown) d.copy(at = now) else d)
        val billed  = present.iterator.map(_.key).toSet
        val (inputs, aged) = dated.filterNot(d => billed(d.listing.key)).partition(ListingRetention.live(_, now, retentionWindow))
        val twins = ListingRetention.returnedUnderAnotherKey(inputs.map(_.listing), present).iterator.map(_.key).toSet
        val live  = inputs.filterNot(d => twins(d.listing.key))
        if (twins.nonEmpty) deleteTwins(twins, atBoot = true)
        live.foreach(d => departed.put(d.listing.key, d))
        if (read.isEmpty || stored.exists(_.at == ListingRetention.Unknown) || aged.nonEmpty || live.size + twins.size != stored.size)
          persist(retainedStore.replace(live))
        val kept = ListingRetention.retainedAt(live, present, now, normalizer, formsOf, retentionWindow)
        kept.foreach(l => retained.add(l.key))
        if (stored.nonEmpty) logger.info(s"identity model: ${kept.size} kept listing(s) resumed of ${live.size} departed, ${aged.size} past ${retentionWindow.toDays} days")
        kept
      })
  }

  /** `twins` (departed listings billed again under another key) deleted from the store, logged and counted: duplicates, so
   *  deleted, never expired. A delete that fails is logged; the next take-up or drain finds them again. */
  private def deleteTwins(twins: Set[services.movies.ListingKey], atBoot: Boolean): Unit = {
    persist(retainedStore.delete(twins))
    metrics.twinsDeleted(twins.size, atBoot)
    logger.info(s"identity model: ${twins.size} departed listing(s) deleted as twins of a listing billed again under another key" +
      (if (atBoot) " (take-up)" else ""))
  }

  /** A write of the departed listings: one that fails is logged, never the drain's failure -- the next move writes again. */
  private def persist(write: => Unit): Unit =
    try write catch { case NonFatal(e) => logger.warn(s"identity model: the departed listings could not be written: $e") }

  /** `seen` and `gone`, as they move what `engine` holds, into the history: a listing new to it, held otherwise — the
   *  model's fields or its poster — or let go. Its screening days alone moving is no entry: a scrape adds days daily,
   *  and the model reads only their season. */
  private def kept(engine: IncrementalResolver, seen: Seq[Listing], gone: Seq[services.movies.ListingKey]): Unit =
    if (history ne IdentityHistory.Discard) {
      val at = clock.millis()
      def key(k: services.movies.ListingKey) = services.movies.ListingKey.serialised(k)
      val moved = seen.flatMap { listing =>
        val was = engine.held(listing.key)
        Option.when(!was.contains(listing) || was.exists(_.poster != listing.poster))(
          HistoryEntry(HistoryEntry.Listing, key(listing.key), at, was.map(ListingBson.doc), Some(ListingBson.doc(listing)), build))
      }
      val left = gone.flatMap(k => engine.held(k).map(was => HistoryEntry(HistoryEntry.Listing, key(k), at, Some(ListingBson.doc(was)), None, build)))
      history.append(moved ++ left)
    }

  /** A NEW listing whose venue page is unread (`pageWait`) has its page asked for, once. Whether it is taken in meanwhile
   *  is the input gate's ([[gate]]): held while the read was never attempted, then taken in -- decided Pending on the
   *  page while it stays unread (step 6, the owner 2026-10-31). */
  private def admit(engine: IncrementalResolver, scraped: Seq[(String, Seq[Listing])]): Unit =
    scraped.foreach { case (venue, listings) =>
      listings.filter(l => !engine.heldAt(venue).contains(l.key) && pageWait.awaiting(l)).foreach(pageWait.request)
    }

  /** Take up the model the store kept, over the archive's listings; what queued meanwhile follows. */
  def takeUp(): Unit = {
    val started = tools.Stopwatch.start()
    val engine  = newModel()
    val present = archive()
    val held    = inputGate.resume(present) ++ resumed(present)
    engine.restore(held)
    // Each listing taken up whose venue page is unread has it asked, as `admit` asks a new one's: it is decided Pending on
    // the page, and nothing else asks for it -- a kept listing no film row names, or one billed before the take-up whose
    // ask a restart dropped. Never asked, its family's take stayed provisional at rest until some later scrape moved it
    // (Verify scrape recording #10, US kept Alamo Drafthouse listings). The queue keeps one task per page.
    held.filter(pageWait.awaiting).foreach(pageWait.request)
    model = Some(engine)
    retryAt = scala.None; retryDelay = IdentityModelService.FirstRetry
    // The gauges from the moment the model is up, not from its first event.
    val sizes   = engine.sizes
    metrics.batch(ModelBatch(0, 0, engine.familiesResolved, engine.familyCount, started.seconds, sizes))
    reportRetention(engine)
    logger.info(s"identity model: taken up — ${engine.heldCount} listings in ${engine.familyCount} families, " +
      s"${engine.familiesResolved} re-resolved (${engine.timings.render}; ${reading()}); ${sizes.render}")
    safely("announce the take-up")(takenUp())
  }

  /** Drain what queued into the model, if it has been taken up. */
  private def catchUp(): Unit = model.foreach(_ => safely("catch up") { drain(); () })

  private def safely(what: String)(body: => Unit): Unit =
    try body
    catch { case NonFatal(e) =>
      logger.error(s"identity model: $what failed — rebuilding from its store: $e", e)
      metrics.rebuilt()
      try takeUp() catch { case NonFatal(again) => logger.error(s"identity model: rebuild failed: $again", again); takeUpFailed() }
    }
}

object IdentityModelService {
  /** What is shown of a listing new to the model while its version is held: on its own, Pending on its questions (the
   *  main session's first choice (a), 2026-11-06), or nothing until it is taken in (b). The one place to switch. */
  enum NewWhilePending {
    case Shown, Hidden
    def shown(held: Seq[ModelInputGate.Held]): Seq[ModelInputGate.Held] = if (this == Shown) held else Nil
  }
  object NewWhilePending {
    /** (b), the main session's switch of 2026-11-06: shown on its own, a listing the site already showed on a film but the
     *  model did not hold was written with no film, and its film came back once it was taken in -- a reversal the gate
     *  made (DE "IF PIGEONS TURNED TO GOLD", 44 min). Hidden, nothing is written for it until it is taken in. */
    val Default: NewWhilePending = Hidden
  }

  /** A held new listing as the projection reads it: its own decision, no film, Pending on its questions -- `unanswered`
   *  set so the agreement stage leaves it as it is, as it leaves any decision the model's questions still hold. */
  private[adapters] def waiting(held: ModelInputGate.Held): ResolverDecision =
    ResolverDecision(Seq(held.newest.key), scala.None, 0.0, ResolverDecision.Basis.NoEvidence,
      Seq(s"held: ${held.waits.size} question(s) of its own not answered yet"), unanswered = held.waits.size, awaits = held.waits)(DecisionTrace.Empty)

  /** The wait before a failed take-up is first retried; it doubles with each failure after, up to [[MaxRetry]]. */
  val FirstRetry: FiniteDuration = FiniteDuration(1, TimeUnit.MINUTES)
  val MaxRetry: FiniteDuration   = FiniteDuration(30, TimeUnit.MINUTES)
  /** How long a drain waits after an event for the rest of its burst (never past the `settle` cap after the first). */
  val Quiet: FiniteDuration = FiniteDuration(2, TimeUnit.SECONDS)
}
