package services.identity

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
                            sizes: IncrementalResolver.FamilySizes)

/** The model's gauges; the Prometheus ones live in `services.metrics`. */
trait IdentityModelMetrics {
  def batch(batch: ModelBatch): Unit
  def rebuilt(): Unit
}
object IdentityModelMetrics {
  val Silent: IdentityModelMetrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = (); def rebuilt(): Unit = () }
}

/**
 * The identity model of one country, kept current by the pipeline's own events instead of a
 * whole-corpus resolve on a schedule (docs/design/identity-resolver.md §20):
 *
 *  - a venue's archived scrape ([[venueScraped]]) — its listings now, diffed against what the model
 *    holds there: a listing it no longer lists leaves the model, so a film nobody shows any more
 *    is swept by the event that stopped showing it;
 *  - new content the observation store files ([[observed]]) — mapped back through
 *    [[ObservationReads]] to the questions that read it.
 *
 * Events are queued from any thread and drained on ONE thread every `settle`, as one
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
  // What the model's lookups report of their reading (`TrackedLookups.render`), for the take-up log.
  reading:    () => String = () => "",
  /** Run on the model's thread before each drain: what turns queued announcements into observed keys
   *  (`VenuePageIndex.settle`), so the drain that follows takes them in. */
  beforeDrain: () => Unit = () => (),
  /** Which new listings wait for their venue page before they are taken in (a cut-over country). */
  pageWait:   PageWait = PageWait.Never,
  clock:      java.time.Clock = java.time.Clock.systemUTC()
) extends Logging {

  // New listings waiting for their venue page, by venue, with when each began to wait. Touched only on
  // the model's thread (`drain`).
  private val waiting = scala.collection.mutable.HashMap.empty[String, Map[services.movies.ListingKey, (Listing, java.time.Instant)]]

  private val venues       = new ConcurrentHashMap[String, Seq[Listing]]()
  private val observations = ConcurrentHashMap.newKeySet[String]()
  private var model: Option[IncrementalResolver] = scala.None

  /** A venue's scrape was archived with `films`: its listings now. */
  def venueScraped(cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
    venues.put(cinema.displayName, Listing.distinct(Listing.all(Seq(cinema -> films), normalizer))); ()
  }

  /** The observation store filed new content under `key`. */
  def observed(key: String): Unit = { observations.add(key); () }

  /** The model brought up to NOW — taken up if it is not yet, every queued event drained — on its
   *  own thread: what a projection reads. `None` when it cannot be had within `timeout` (a rebuild
   *  still running) or the model could not be taken up. */
  def current(timeout: FiniteDuration): Option[ModelSnapshot] = onModel(timeout) {
    safely("catch up") { if (model.isEmpty) takeUp(); drain(); () }
    model.map(snapshotOf)
  }

  /** The model caught up with what queued, if it has been taken up — never a take-up: what the
   *  shadow tick and the fill read, which must not wait on one. */
  def peek(timeout: FiniteDuration): Option[ModelSnapshot] = onModel(timeout) {
    model.flatMap { _ => safely("catch up") { drain(); () }; model.map(snapshotOf) }
  }

  /** Every store key the model's current questions read, taken on the model's thread so no
   *  question is caught half re-asked; None until the model is taken up (`TmdbStoreSweep`). */
  def reachable(timeout: FiniteDuration): Option[Set[String]] = onModel(timeout)(model.map(_ => reads.keySet))

  // A snapshot is built only when read, on the model's thread — never per drain: on the US corpus
  // one is ~100k listings' worth of maps, and a drain runs every few seconds.
  private def snapshotOf(engine: IncrementalResolver) =
    ModelSnapshot(engine.resolution, engine.gaps, engine.listings, TmdbRefreshes.of(engine.familyQuestions))

  private def onModel[A](timeout: FiniteDuration)(body: => Option[A]): Option[A] =
    scala.util.Try(scheduler.submit[Option[A]](() => body).get(timeout.toMillis, TimeUnit.MILLISECONDS)).toOption.flatten

  @volatile private var tookUp = false
  /** Whether the take-up [[start]] scheduled has finished — taken up, or failed and left to the
   *  drains to rebuild: the boot work a worker's readiness waits on. */
  def takeUpSettled: Boolean = tookUp

  def start(): Unit = {
    scheduler.execute(() => try safely("restore")(takeUp()) finally tookUp = true)
    scheduler.scheduleWithFixedDelay(() => safely("drain")(drain()), settle.toMillis, settle.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Drain what has queued, on the calling thread — the scheduler's, or a test's. */
  def drain(): Option[ModelBatch] = model.flatMap { engine =>
    beforeDrain()
    val scraped = venues.keySet.asScala.toSeq.flatMap(venue => Option(venues.remove(venue)).map(venue -> _))
    val keys    = observations.asScala.toSeq.filter(observations.remove)
    val (admitted, released) = admit(engine, scraped)
    Option.when(scraped.nonEmpty || keys.nonEmpty || released.nonEmpty) {
      val started = tools.Stopwatch.start()
      val before  = engine.familiesResolved
      val seen    = admitted.flatMap(_._2) ++ released
      val gone    = scraped.flatMap { case (venue, now) => engine.heldAt(venue) -- now.map(_.key) }
      engine.batch(seen, gone, reads.changedBy(keys))
      val batch = ModelBatch(scraped.size, keys.size, engine.familiesResolved - before, engine.familyCount, started.seconds,
        engine.sizes)
      metrics.batch(batch)
      batch
    }
  }

  /** Each scraped venue's listings the model takes in now — all but a NEW one whose venue page is unread
   *  (`pageWait`), which waits, its page asked for once — and the waiting listings released this drain:
   *  their page answered, or `pageWait.limit` passed. A waiting listing its venue no longer lists is
   *  forgotten; one the model already holds never waits. */
  private def admit(engine: IncrementalResolver, scraped: Seq[(String, Seq[Listing])]): (Seq[(String, Seq[Listing])], Seq[Listing]) = {
    val now = clock.instant()
    val admitted = scraped.map { case (venue, listings) =>
      val before        = waiting.getOrElse(venue, Map.empty)
      val (wait, take)  = listings.partition(l => !engine.heldAt(venue).contains(l.key) && pageWait.awaiting(l))
      val stillWaiting  = wait.map(l => l.key -> (l, before.get(l.key).fold(now)(_._2))).toMap
      wait.filterNot(l => before.contains(l.key)).foreach(pageWait.request)
      if (stillWaiting.isEmpty) waiting.remove(venue) else waiting(venue) = stillWaiting
      venue -> take
    }
    val deadline = now.minusMillis(pageWait.limit.toMillis)
    val released = waiting.toSeq.flatMap { case (venue, held) =>
      val (go, stay) = held.partition { case (_, (l, since)) => !pageWait.awaiting(l) || !since.isAfter(deadline) }
      if (stay.isEmpty) waiting.remove(venue) else waiting(venue) = stay
      go.values.map(_._1)
    }
    (admitted, released)
  }

  /** Take up the model the store kept, over the archive's listings; what queued meanwhile follows. */
  def takeUp(): Unit = {
    val started = tools.Stopwatch.start()
    val engine  = newModel()
    engine.restore(archive())
    model = Some(engine)
    // The gauges from the moment the model is up, not from its first event.
    val sizes   = engine.sizes
    metrics.batch(ModelBatch(0, 0, engine.familiesResolved, engine.familyCount, started.seconds, sizes))
    logger.info(s"identity model: taken up — ${engine.heldCount} listings in ${engine.familyCount} families, " +
      s"${engine.familiesResolved} re-resolved (${engine.timings.render}; ${reading()}); ${sizes.render}")
  }

  private def safely(what: String)(body: => Unit): Unit =
    try body
    catch { case NonFatal(e) =>
      logger.error(s"identity model: $what failed — rebuilding from its store: $e", e)
      metrics.rebuilt()
      try takeUp() catch { case NonFatal(again) => logger.error(s"identity model: rebuild failed: $again", again); model = scala.None }
    }
}
