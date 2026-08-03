package services.metrics

import play.api.Logging
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
 * The sample-on-a-timer scaffolding every census in this package shares: take one
 * reading now so the series exists from boot rather than from the first tick, then
 * keep taking one every [[sampleInterval]], and never let a failed reading kill the
 * schedule.
 *
 * That last part is the reason this is shared rather than retyped. A census
 * measures the thing nothing else can see — a cinema that stopped being scraped, a
 * rating source that stopped running — so a sampler that dies on one bad tick
 * leaves a FLAT line, which reads exactly like health. Every implementation has to
 * wrap its tick in the same `Try`, and "every implementation has to remember" is
 * how one of them eventually doesn't.
 */
trait SampledCensus extends Logging {

  /** Names the daemon thread and the failure logs — kebab-case, e.g. `rating-run-census`. */
  protected def censusName: String

  /** How often to take a reading. Match it to how fast the measured thing moves:
   *  scrape staleness shifts by the minute, a cinema going barren by the day. */
  protected def sampleInterval: FiniteDuration

  /** Take one reading and publish it. Called on the caller's thread once at
   *  [[start]], then on this census's own scheduler. Implementations keep it
   *  cheap and side-effect-free beyond writing gauges. */
  def sample(): Unit

  private lazy val scheduler = DaemonExecutors.scheduler(censusName)

  private def sampleQuietly(occasion: String): Unit = {
    Try(sample()).recover { case e => logger.warn(s"$censusName $occasion failed: ${e.getMessage}") }
    ()
  }

  def start(): Unit = {
    sampleQuietly("initial sample")
    scheduler.scheduleAtFixedRate(() => sampleQuietly("sample tick"),
      sampleInterval.toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}
