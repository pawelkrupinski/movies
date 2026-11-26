package tools

import java.util.concurrent.{ScheduledFuture, ScheduledThreadPoolExecutor, TimeUnit}

import scala.collection.mutable.ArrayBuffer

/** A scheduler that runs nothing and keeps how it was asked to: each `scheduleAtFixedRate` as (name-less) initial delay and
 *  period in milliseconds, each one-shot `schedule` as its delay. For a spec pinning HOW a loop arms itself. */
final class RecordingScheduler extends ScheduledThreadPoolExecutor(1) {
  val fixedRates = ArrayBuffer.empty[(Long, Long)]
  val oneShots   = ArrayBuffer.empty[Long]

  override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long, unit: TimeUnit): ScheduledFuture[?] = {
    fixedRates += ((unit.toMillis(initialDelay), unit.toMillis(period))); null
  }
  override def schedule(command: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture[?] = {
    oneShots += unit.toMillis(delay); null
  }
}
