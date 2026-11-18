package services.identityadapters

import scala.concurrent.duration.FiniteDuration

/** The lookup fill's own claimed schedule: [[LookupFill.start]] every `interval`, first after `initialDelay`, on the
 *  replica that wins the window's claim. It records only that it started: [[LookupFill.round]] completes the tick
 *  ([[LookupFill.Tick]], on the fill's `ticks`), not the hand-off -- a round that hangs or fails every time is seen there
 *  or nowhere. */
object LookupFillSchedule {
  def apply(fill: LookupFill, interval: => FiniteDuration, initialDelay: FiniteDuration, runStore: services.schedule.ScheduledRunStore,
            clock: java.time.Clock, ticks: tools.TickRecorder, threadPools: tools.ThreadPools): services.tasks.ClaimedPeriodicTask =
    new services.tasks.ClaimedPeriodicTask(LookupFill.Tick, () => { fill.start(); tools.TickRun.Completed }, interval, initialDelay, runStore, clock,
      tools.TickRecorder.completedByTheWork(ticks), threadPools)
}
