package services.sharecards

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

import play.api.Logging
import tools.{LogThrottle, ThreadPools}

/**
 * A country's side of draining the fleet queue, one [[tick]] every [[RescrapePacing.TickEvery]]
 * on its own daemon thread — never a task: a request waiting on the quota or on Facebook holds
 * no task-worker slot, so card renders never queue behind re-scrapes.
 *
 * Each tick expands the country's due films into their pages ([[RescrapeFilmExpansion]]), then sends
 * at most one page ([[RescrapePageSender]]).
 */
class FacebookRescrapeDrain(expansion: RescrapeFilmExpansion, sender: RescrapePageSender, country: String,
                            stopwatch: tools.Stopwatch,
                            // Where each completed tick is reported (`ScheduledTickStale`).
                            ticks: tools.TickRecorder = tools.TickRecorder.Unrecorded,
                            threadPools: tools.ThreadPools) extends tools.Stoppable with Logging {
  import RescrapePacing.TickEvery

  /** One drain tick: [[tools.TickRun.Incomplete]] when a film's pages could not be read (that film deferred or dropped,
   *  logged) — a drain whose reads fail every tick must go stale. Facebook's own answers (refused, rate-limited) are its
   *  word on a page, and the holds they bring are waits: they complete. */
  def tick(): tools.TickRun = {
    val expanded = expansion.expandDue()
    sender.sendDue()
    expanded
  }

  // One tick at a time on one thread; a failed tick (the fleet database unreachable) is logged
  // at most once per ten minutes and the next tick tries again.
  private val failures = new LogThrottle(10.minutes.toNanos, stopwatch)
  private var scheduler = Option.empty[ScheduledExecutorService]

  def start(): Unit = synchronized {
    if (scheduler.isEmpty) {
      val s = threadPools.scheduler(s"facebook-rescrapes-$country")
      ticks.started("facebook-rescrape-drain", TickEvery, TickEvery)
      s.scheduleWithFixedDelay(() => Try(tick()) match {
        case Success(tools.TickRun.Completed)  => ticks.completed("facebook-rescrape-drain", TickEvery)
        case Success(tools.TickRun.Incomplete) => ()   // a film's pages unreadable: logged by the tick
        case Failure(e) => failures.admit().foreach(suppressed => logger.warn(
          s"share card: re-scrape drain failed (${e.getClass.getSimpleName}: ${e.getMessage}); $suppressed more since the last report"))
      }, TickEvery.toMillis, TickEvery.toMillis, TimeUnit.MILLISECONDS)
      scheduler = Some(s)
    }
  }

  def stop(): Unit = synchronized { scheduler.foreach(_.shutdownNow()); scheduler = None }
}
