package services.readmodel

import play.api.Logging
import settings.{ReadModelColdRetryInterval, ReadModelReloadInterval}
import tools.{ScheduledTick, Stoppable, ThreadPools, TickRecorder, TickRun}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

/** What keeps [[WebReadModel]] current over a pod's life: at boot it hydrates the model and opens
 *  its change streams ([[ReadModelStreams.open]]), then runs two ticks on one scheduler — the
 *  drift-checked backstop ([[backstopTick]]) and the cold retry that reads a model that never read
 *  whole and reopens a stream that ended ([[ReadModelStreams.coldRetryTick]]) — and stops both, then
 *  the streams, at shutdown. */
class ReadModelUpkeep(
    model: WebReadModel,
    streams: ReadModelStreams,
    // The backstop reload and the cold-start retry cadences (`KINOWO_READMODEL_RELOAD_SECONDS` /
    // `…_COLD_RETRY_SECONDS`, resolved by the web root); the compiled-in ones for specs.
    reloadInterval:    ReadModelReloadInterval,
    coldRetryInterval: ReadModelColdRetryInterval,
    driftSettle:       ReadModelUpkeep.DriftSettle = ReadModelUpkeep.DefaultDriftSettle,
    // Where each completed backstop and cold-retry tick is reported (`ScheduledTickStale`).
    ticks:             TickRecorder = TickRecorder.Unrecorded,
    threadPools:       ThreadPools,
    // The wait before a drift is confirmed (`driftSettle`): the root's `sleep`; a spec passes one that skips it.
    sleep:             FiniteDuration => Unit) extends Stoppable with Logging {

  /** Periodic backstop tick. While both change streams are live they keep the
   *  model current, so re-reading and re-decoding the whole corpus every tick is
   *  wasted CPU on the single-vCPU serving box — and that decode burst is what
   *  stalls a request that happens to land during it. So skip the reload when the
   *  streams are live *and* the cheap server-side counts still match what we hold;
   *  pay the O(corpus) reload only when a stream has died (full catch-up, the
   *  original backstop behaviour) or a count has drifted (a delivered event we
   *  failed to apply, or one missed by a silently-stalled stream). */
  private[readmodel] def backstopTick(): TickRun =
    if (!streams.streamsLive) model.reloaded()
    else model.drift().fold(TickRun.Completed) { first =>
      // A COUNT TAKEN MID-WRITE IS NOT DRIFT. The count is read straight off the server, the
      // model only once the write's change event has been applied, so a count landing between
      // the two sees the database a row or two ahead. That was every drift reload web-us ever
      // logged (3 in the 7 days to 2026-10-02: isolated, db ahead by 1-2 screenings, each in the
      // same minute as a worker prune burst, never at two consecutive ticks) -- a full corpus
      // decode bought for an event already on its way. A lost event is still missing after
      // the settle; an in-flight one is not.
      sleep(driftSettle.value)
      model.drift() match {
        case Some(confirmed) =>
          logger.info(s"WebReadModel backstop: drift detected — reloading ($confirmed).")
          model.reloaded()
        case None =>
          logger.info(s"WebReadModel backstop: count mismatch settled within ${driftSettle.value} ($first) — no reload.")
          TickRun.Completed
      }
    }

  private val scheduler       = threadPools.scheduler("web-read-model")
  private val BackstopSeconds  = reloadInterval.value.toSeconds
  // Far tighter than the backstop because the state it recovers from is a blank site, not
  // drift. Once hydrated a tick is one field read; until then each is a full reload, which is
  // the point — a pod that has not read the corpus whole is not ready (`hydrated`).
  private val ColdRetrySeconds = coldRetryInterval.value.toSeconds

  def start(): Unit = {
    streams.open()
    ticks.started("read-model-backstop", reloadInterval.value, reloadInterval.value)
    ticks.started("read-model-cold-retry", coldRetryInterval.value, coldRetryInterval.value)
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("read-model-backstop", reloadInterval.value, logger, ticks)(backstopTick()),
      BackstopSeconds, BackstopSeconds, TimeUnit.SECONDS)
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("read-model-cold-retry", coldRetryInterval.value, logger, ticks)(streams.coldRetryTick()),
      ColdRetrySeconds, ColdRetrySeconds, TimeUnit.SECONDS)
    logger.info(s"WebReadModel started; backstop reload every ${BackstopSeconds}s; " +
      s"cold retry every ${ColdRetrySeconds}s; " +
      s"change-stream watches ${if (streams.opened) "active" else "unavailable — reopened on the cold-retry cadence"}.")
  }

  def stop(): Unit = {
    // The scheduler first: a tick in flight may be reopening a stream, which a close before it ends would miss.
    scheduler.shutdown()
    Try(scheduler.awaitTermination(5, TimeUnit.SECONDS))
    streams.close()
  }
}

object ReadModelUpkeep {
  val DefaultReloadInterval: ReadModelReloadInterval       = ReadModelReloadInterval(30.minutes)
  val DefaultColdRetryInterval: ReadModelColdRetryInterval = ReadModelColdRetryInterval(30.seconds)

  /** How long the backstop waits before re-counting a mismatch (see `backstopTick`): long enough
   *  for a write's change event to reach the model, short enough to hold its one thread briefly. */
  final case class DriftSettle(value: FiniteDuration) extends AnyVal
  val DefaultDriftSettle: DriftSettle = DriftSettle(5.seconds)
}
