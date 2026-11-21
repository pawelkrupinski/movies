package services.movies

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.concurrent.duration.*
import scala.util.Try

import play.api.Logging

/**
 * Reopens ONE change-stream cursor after it dies.
 *
 * WHY THIS EXISTS. A change stream's `onError` is terminal: the cursor is gone and the
 * driver will not bring it back. Both watchers here only ever cleared their subscription
 * reference and logged *"a reopen resumes from the persisted token"* — but nothing
 * actually drove that reopen. `MovieChangeStream.ensureWatching` runs on
 * REGISTRATION, and the worker registers its two consumers (`MovieCache`,
 * `ReadModelProjector`) once at boot, so after the last registration the stream had no
 * path back. One terminal error — a network blip, a primary step-down, an unusable
 * resume token — killed the worker's change stream until the process restarted.
 *
 * On 2026-08-29 that turned a one-off migration hiccup into a silent outage: the Mongo
 * dump/restore invalidated every persisted token, the two boot registrations burned the
 * only two reopen attempts the accident of registration order gave us, and all three
 * workers ran for hours with a dead stream — zero read-model projections, only prune
 * deletes. See [[ChangeStreamResumeToken.isInvalid]] for the other half of that fix.
 *
 * The delays back off so a Mongo that is genuinely down is not hammered, and reset once
 * the stream proves itself by DELIVERING AN EVENT ([[opened]]) rather than merely
 * subscribing — a cursor that dies immediately after every open would otherwise retry at
 * the first delay for ever.
 *
 * `schedule` is the seam: production passes a daemon scheduler
 * ([[ChangeStreamReopen.DaemonSchedule]]), specs pass a recorder that fires the
 * runnable when they say so. [[close]] closes it too.
 */
final class ChangeStreamReopen(
  name:     String,
  reopen:   () => Unit,
  schedule: ChangeStreamReopen.Schedule,
  delays:   Seq[FiniteDuration] = ChangeStreamReopen.DefaultDelays
) extends Logging with AutoCloseable {

  private val attempt = new AtomicInteger(0)
  private val pending = new AtomicBoolean(false)
  private val stopped = new AtomicBoolean(false)

  /** The stream delivered an event — it is healthy, so the next failure starts the
   *  backoff from the beginning. Deliberately NOT called on subscribe: a cursor that
   *  opens and dies straight away has proved nothing. */
  def opened(): Unit = attempt.set(0)

  /** The cursor died. Schedules exactly one reopen (a second call while one is already
   *  pending is a no-op — both the movies and screenings observers can report the same
   *  death through `onError` and `onComplete`). Does nothing once [[close]]d. */
  def failed(): Unit =
    if (!stopped.get() && pending.compareAndSet(false, true)) {
      val delay = delays(attempt.getAndIncrement().min(delays.size - 1))
      logger.info(s"$name change stream: reopening in ${delay.toSeconds}s.")
      schedule(delay, () => {
        pending.set(false)
        if (!stopped.get()) Try(reopen()).failed.foreach { e =>
          logger.warn(s"$name change stream: reopen failed (${e.getMessage}) — will retry.")
          failed()
        }
      })
    }

  /** Stop reopening for good — the repository or the watch is closing — and release the schedule. */
  override def close(): Unit = { stopped.set(true); schedule.close() }
}

object ChangeStreamReopen {
  /** Capped backoff: quick enough that a blip costs seconds, slow enough that a Mongo
   *  that is down for an hour sees one attempt a minute rather than thousands. */
  val DefaultDelays: Seq[FiniteDuration] = Seq(1.second, 5.seconds, 15.seconds, 60.seconds)

  /** When a reopen runs: `run` after `delay`. Closed with its driver. */
  trait Schedule extends AutoCloseable {
    def apply(delay: FiniteDuration, run: () => Unit): Unit
    override def close(): Unit = ()
  }

  /** Production wiring: a daemon scheduler per driver ([[DaemonSchedule]]). */
  def onDaemonScheduler(name: String, reopen: () => Unit): ChangeStreamReopen =
    new ChangeStreamReopen(name, reopen, new DaemonSchedule(name))

  /** The production schedule: one daemon thread, `<name>-reopen`, started by the first reopen it
   *  schedules (most cursors never die, so most drivers never start one) and ended when its driver
   *  closes -- a side-collection watch builds a driver per watch, and each one that had ever
   *  reopened left its thread parked for the life of the process. The scheduled body only
   *  re-subscribes a cursor, so it never blocks the thread for long. Also handed in on its own by a
   *  stream that builds its [[ChangeStreamReopen]] itself ([[services.tasks.TaskInsertStream]]). */
  final class DaemonSchedule(name: String) extends Schedule {
    private var scheduler: Option[java.util.concurrent.ScheduledExecutorService] = None
    private var closed = false

    def apply(delay: FiniteDuration, run: () => Unit): Unit = synchronized {
      if (!closed) {
        val running = scheduler.getOrElse(tools.DaemonExecutors.scheduler(s"$name-reopen"))
        scheduler = Some(running)
        running.schedule((() => run()): Runnable, delay.toMillis, TimeUnit.MILLISECONDS); ()
      }
    }

    override def close(): Unit = synchronized { closed = true; scheduler.foreach(_.shutdownNow()); scheduler = None }
  }
}
