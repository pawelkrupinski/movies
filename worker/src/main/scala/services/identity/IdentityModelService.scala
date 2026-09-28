package services.identity

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.movies.TitleNormalizer

import java.util.concurrent.{ConcurrentHashMap, ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/** What the model did with one drained batch of events. */
final case class ModelBatch(venues: Int, observations: Int, familiesResolved: Int, families: Int, seconds: Double)

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
 * from its store rather than carried on. Readers on other threads see [[resolution]] and [[gaps]]
 * as of the last drain.
 */
final class IdentityModelService(
  newModel:   () => IncrementalResolver,
  reads:      ObservationReads,
  archive:    () => Seq[Listing],
  normalizer: TitleNormalizer,
  settle:     FiniteDuration,
  scheduler:  ScheduledExecutorService,
  metrics:    IdentityModelMetrics = IdentityModelMetrics.Silent
) extends Logging {

  private val venues       = new ConcurrentHashMap[String, Seq[Listing]]()
  private val observations = ConcurrentHashMap.newKeySet[String]()
  private var model: Option[IncrementalResolver] = scala.None
  @volatile private var current: Option[(Resolution, AnswersChanged, Seq[Listing])] = scala.None

  /** A venue's scrape was archived with `films`: its listings now. */
  def venueScraped(cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
    venues.put(cinema.displayName, Listing.distinct(Listing.all(Seq(cinema -> films), normalizer))); ()
  }

  /** The observation store filed new content under `key`. */
  def observed(key: String): Unit = { observations.add(key); () }

  /** The model as of the last drain, if it has been taken up yet. */
  def resolution: Option[Resolution] = current.map(_._1)
  /** What the model's lookups do not know yet, as of the last drain. */
  def gaps: AnswersChanged = current.fold(AnswersChanged.Empty)(_._2)
  /** The listings the model held at the last drain. */
  def listings: Seq[Listing] = current.fold(Seq.empty[Listing])(_._3)

  /** The model brought up to NOW — taken up if it is not yet, every queued event drained — on its
   *  own thread, with the listings it decided: what a projection reads. `None` when it cannot be
   *  had within `timeout` (a rebuild still running) or the model could not be taken up. */
  def current(timeout: FiniteDuration): Option[(Resolution, Seq[Listing])] =
    scala.util.Try(scheduler.submit[Option[(Resolution, Seq[Listing])]] { () =>
      safely("catch up") { if (model.isEmpty) takeUp(); drain(); () }
      current.map { case (resolution, _, listings) => resolution -> listings }
    }.get(timeout.toMillis, TimeUnit.MILLISECONDS)).toOption.flatten

  def start(): Unit = {
    scheduler.execute(() => safely("restore")(takeUp()))
    scheduler.scheduleWithFixedDelay(() => safely("drain")(drain()), settle.toMillis, settle.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Drain what has queued, on the calling thread — the scheduler's, or a test's. */
  def drain(): Option[ModelBatch] = model.flatMap { engine =>
    val scraped = venues.keySet.asScala.toSeq.flatMap(venue => Option(venues.remove(venue)).map(venue -> _))
    val keys    = observations.asScala.toSeq.filter(observations.remove)
    Option.when(scraped.nonEmpty || keys.nonEmpty) {
      val started = System.nanoTime()
      val before  = engine.familiesResolved
      val seen    = scraped.flatMap(_._2)
      val gone    = scraped.flatMap { case (venue, now) => engine.heldAt(venue) -- now.map(_.key) }
      engine.batch(seen, gone, reads.changedBy(keys))
      publish(engine)
      val batch = ModelBatch(scraped.size, keys.size, engine.familiesResolved - before, engine.decisions.size, (System.nanoTime() - started) / 1e9)
      metrics.batch(batch)
      batch
    }
  }

  /** Take up the model the store kept, over the archive's listings; what queued meanwhile follows. */
  def takeUp(): Unit = {
    val engine = newModel()
    engine.restore(archive())
    model = Some(engine)
    publish(engine)
    logger.info(s"identity model: taken up — ${engine.listings.size} listings in ${engine.familyOf.values.toSet.size} families, " +
      s"${engine.familiesResolved} re-resolved (${engine.timings.render})")
  }

  private def publish(engine: IncrementalResolver): Unit = current = Some((engine.resolution, engine.gaps, engine.listings))

  private def safely(what: String)(body: => Unit): Unit =
    try body
    catch { case NonFatal(e) =>
      logger.error(s"identity model: $what failed — rebuilding from its store: $e", e)
      metrics.rebuilt()
      try takeUp() catch { case NonFatal(again) => logger.error(s"identity model: rebuild failed: $again", again); model = scala.None }
    }
}
