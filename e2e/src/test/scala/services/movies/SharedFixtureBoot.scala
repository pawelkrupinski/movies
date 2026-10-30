package services.movies

import org.scalatest.{BeforeAndAfterAll, Suite}
import tools.FixtureTestWiring

import java.util.{Timer, TimerTask}
import scala.concurrent.duration.*

/**
 * One value for the suites that only READ it, made when the first of them asks and stopped once none holds it.
 *
 * Holders [[acquire]] it and [[release]] it when done; the first acquire makes it (the others wait for it rather than
 * make their own), and the last release stops it after `linger` unless another acquire comes first. The linger is for
 * a suite that starts a moment after the previous holder finished: without it, it pays the whole make again. Nothing
 * outlives the linger: the stop runs on a daemon timer of its own, which ends with it, so a kept sbt server holds no
 * value and no thread once its run is over (see [[StopsItsWirings]]). A failed make holds nothing and is not kept.
 */
final class Shared[A](make: () => A, stop: A => Unit, linger: FiniteDuration) {
  private var holders = 0
  private var current: Option[A] = None
  private var pendingStop: Option[Timer] = None

  def acquire(): A = synchronized {
    pendingStop.foreach(_.cancel())
    pendingStop = None
    val value = current.getOrElse {
      val made = make()
      current = Some(made)
      made
    }
    holders += 1
    value
  }

  def release(): Unit = synchronized {
    holders -= 1
    if (holders == 0 && current.isDefined) {
      val timer = new Timer("shared-fixture-linger", true)
      timer.schedule(new TimerTask { def run(): Unit = { stopIfUnheld(timer); timer.cancel() } }, linger.toMillis)
      pendingStop = Some(timer)
    }
  }

  private def stopIfUnheld(timer: Timer): Unit = {
    val stopping = synchronized {
      if (holders == 0 && pendingStop.contains(timer)) {
        pendingStop = None
        val value = current
        current = None
        value
      } else None
    }
    stopping.foreach(stop)
  }

  /** Whether a value is made and not yet stopped. */
  def live: Boolean = synchronized(current.isDefined)
}

/**
 * The Polish fixture boot (`08-06-2026`, [[FixtureTestWiring.bootStartup]]), ONE for the suites that only read it.
 *
 * Each read-only reader booted its own: CinemaSlotInvariantsSpec and SearchQueryMarkersSpec (through
 * [[CorpusShapeSpec]]) and ServedOutputInvariantsSpec, three boots of one deterministic pipeline (`bootStartup` runs
 * no scheduler or change stream) side by side on the `e2e (rest)` row, each ~100-150 s of a 4-CPU runner
 * (measure runs 4052, 4053, 2026-10-30). A suite that WRITES to a booted wiring (a further tick, a cleanup) boots its
 * own, as before.
 */
object SharedFixtureBoot {
  private val boot = new Shared[FixtureTestWiring](
    () => stoppedWhenUnheld(new FixtureTestWiring("08-06-2026")),
    _.stop(),
    linger = 30.seconds)

  /** `wiring`, booted; [[Shared]] stops it once no suite holds it. The marker [[E2eWiringsStoppedSpec]] reads. */
  private def stoppedWhenUnheld(wiring: FixtureTestWiring): FixtureTestWiring =
    try { wiring.bootStartup(); wiring }
    catch { case e: Throwable => wiring.stop(); throw e }

  def acquire(): FixtureTestWiring = boot.acquire()
  def release(): Unit = boot.release()
}

/** A suite reading the shared fixture boot: acquired on first use, released after the suite's last test. */
trait ReadsSharedFixtureBoot extends BeforeAndAfterAll { this: Suite =>
  @volatile private var held = false

  protected lazy val sharedFixtureBoot: FixtureTestWiring = {
    val wiring = SharedFixtureBoot.acquire()
    held = true
    wiring
  }

  override protected def afterAll(): Unit =
    try if (held) { held = false; SharedFixtureBoot.release() }
    finally super.afterAll()
}
