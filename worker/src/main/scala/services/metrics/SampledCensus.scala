package services.metrics

import play.api.Logging
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
 * The sample-on-a-timer scaffolding every census in this package shares: take the first
 * reading shortly after boot ([[firstSampleDelay]]) rather than a whole interval later, then
 * keep taking one every [[sampleInterval]], and never let a failed reading kill the
 * schedule.
 *
 * The first reading runs on the census's own thread, never the caller's: taken at `start`
 * on the boot thread, the censuses held a US worker's boot for ~40 s (the listing-key shadow
 * read alone 116 s) for gauges nothing reads in the boot's first minutes, while competing for
 * CPU with the cache hydrate and the identity model's take-up.
 *
 * That last part is the reason this is shared rather than retyped. A census
 * measures the thing nothing else can see — a cinema that stopped being scraped, a
 * rating source that stopped running — so a sampler that dies on one bad tick
 * leaves a FLAT line, which reads exactly like health. Every implementation has to
 * wrap its tick in the same `Try`, and "every implementation has to remember" is
 * how one of them eventually doesn't.
 */
trait SampledCensus extends services.Stoppable with Logging {

  /** Names the daemon thread and the failure logs — kebab-case, e.g. `rating-run-census`. */
  protected def censusName: String

  /** How often to take a reading. Match it to how fast the measured thing moves:
   *  scrape staleness shifts by the minute, a cinema going barren by the day. */
  protected def sampleInterval: FiniteDuration

  /** Take one reading and publish it, on this census's own scheduler. Implementations keep
   *  it cheap and side-effect-free beyond writing gauges. */
  def sample(): Unit

  private lazy val scheduler = DaemonExecutors.scheduler(censusName)

  private def sampleQuietly(occasion: String): Unit = {
    Try(sample()).recover { case e => logger.warn(s"$censusName $occasion failed: ${e.getMessage}") }
    ()
  }

  /** Which of [[SampledCensus.Slots]] this census takes its first reading in: 0 for a light one. */
  protected def firstSampleSlot: Int = 0

  /** When the first reading runs: after the boot's own work, in this census's slot, never later than one interval. */
  protected def firstSampleDelay: FiniteDuration = SampledCensus.firstDelay(firstSampleSlot, sampleInterval)

  def start(): Unit = {
    scheduler.scheduleAtFixedRate(() => sampleQuietly("sample tick"),
      firstSampleDelay.toMillis, sampleInterval.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object SampledCensus {
  /** Past a restart's heavy stretch (cache hydrate, the projector's seed, the identity take-up). */
  val FirstSampleDelay: FiniteDuration = scala.concurrent.duration.Duration(2, "minutes")

  /** How far apart two heavy readers' first readings are. */
  val SlotSpacing: FiniteDuration = scala.concurrent.duration.Duration(1, "minute")

  /** A first reading in `slot`: [[FirstSampleDelay]] plus a [[SlotSpacing]] per slot, never later than `interval`. */
  def firstDelay(slot: Int, interval: FiniteDuration): FiniteDuration = (FirstSampleDelay + SlotSpacing * slot.toLong).min(interval)

  /** The whole-collection readers' first-reading slots, one each and named in one place. All of them used to read at
   *  two minutes: on a us boot the corpus scan, the stranded side-row cleanup and a listing-key read held
   *  ~250 MB live at once, the old generation filled, and four back-to-back full GCs paused ~10 s (JFR, 2026-10-03). */
  object Slots {
    val CorpusScan        = 0
    val RetiredVenues     = 1
    val StrandedSideRows  = 2
    val all: Seq[Int] = Seq(CorpusScan, RetiredVenues, StrandedSideRows)
  }
}
