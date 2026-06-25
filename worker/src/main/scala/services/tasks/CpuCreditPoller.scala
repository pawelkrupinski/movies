package services.tasks

import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import services.Stoppable
import tools.{DaemonExecutors, Env, HttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._
import scala.util.Try

/**
 * The AUTHORITATIVE in-process throttle signal: it polls the worker's REAL
 * shared-CPU credit balance (`fly_instance_cpu_balance`) straight from Fly's
 * hosted Prometheus — the same number the Grafana dashboard and the
 * `kinowo-worker-credit-low` alert read — and trips the reaper backoff when it
 * floors.
 *
 * This replaces the old scrape-duration monitor, which had no way to read the
 * metric and instead INFERRED credit from how slow `ScrapeCinema` handlers ran.
 * Scrape wall-clock is an ambiguous proxy: a brief cluster of network-slow
 * cinema fetches (slow TLS, a hung detail page) looks identical to CPU
 * starvation, so the proxy kept tripping a needless backoff while real credit
 * sat at ~100k (false positives 2026-06-21 and 2026-06-22). Reading the credit
 * balance directly removes the proxy — and the guesswork — entirely.
 *
 * Thresholds mirror the Grafana alert: trip below `enterBelow` (6000), recover
 * above `exitAbove` (12000). The gap is the hysteresis band the reaper won't
 * oscillate within. The external gate ([[ExternalThrottleGate]], driven by that
 * same Grafana alert hitting `/throttle`) stays wired as an INDEPENDENT backstop
 * — so if this poller can't reach `api.fly.io` (or its read-only token lapses)
 * and fails open, the Grafana path still catches a genuine credit crunch.
 *
 * == Proactive / projection throttle ==
 *
 * The reactive floor trigger (balance < 6 000 ms) fires only 1–6 minutes before
 * the actual floor, given observed drain rates of 3 000–7 000 ms/min.  By then
 * up to 8 in-flight tasks keep burning CPU and the floor still hits.
 *
 * The projection trigger catches the drop earlier: consecutive balance readings
 * give a drain rate (ms/s); if the projected time to `enterBelow` is within
 * `lookaheadSeconds` (default 10 min), throttle trips immediately.  On
 * 2026-06-25 this would have tripped 16 min (afternoon) and 26 min (evening)
 * before the floor, preventing both episodes.
 *
 * Fail-open: a failed or unparseable read doesn't flip the decision on a single
 * blip (a transient `api.fly.io` error); only after `maxConsecutiveFailures`
 * reads in a row fail does it force healthy. Under-throttling (the gate covers
 * it) beats pinning the whole pipeline throttled on a dead metric source.
 *
 * The poll runs on a single daemon scheduler thread; the read of `isThrottled`
 * by the reaper threads goes through a lock-free `AtomicReference`.
 */
class CpuCreditPoller(
  http: HttpFetch,
  token: String,
  // Fly org slug the Prometheus endpoint is scoped under (api.fly.io/prometheus/<org>).
  org: String = Env.get("KINOWO_FLY_PROM_ORG").getOrElse("personal"),
  // App whose credit balance to read — this worker. min() across its instances.
  app: String = Env.get("FLY_APP_NAME").getOrElse("kinowo-worker"),
  // Trip throttled once the balance falls BELOW this (matches the Grafana alert).
  enterBelow: Long = Env.positiveLong("KINOWO_CREDIT_THROTTLE_ENTER_BELOW", 6000L),
  // While throttled, HOLD until the balance climbs ABOVE this (hysteresis).
  exitAbove: Long = Env.positiveLong("KINOWO_CREDIT_THROTTLE_EXIT_ABOVE", 12000L),
  pollInterval: FiniteDuration = Env.positiveLong("KINOWO_CREDIT_POLL_SECONDS", 30L).seconds,
  // Consecutive failed reads before we give up and fail open (default healthy).
  maxConsecutiveFailures: Int = Env.positiveInt("KINOWO_CREDIT_MAX_READ_FAILURES", 3),
  // Proactive: trip throttled if (at current drain rate) balance would hit enterBelow
  // within this many seconds.  Default 600 (10 min).  Set to 0 to disable projection.
  lookaheadSeconds: Long = Env.positiveLong("KINOWO_CREDIT_LOOKAHEAD_SECONDS", 600L)
) extends Stoppable with ScrapeThrottleSignal with Logging {

  import CpuCreditPoller.State

  private val query = s"""min(fly_instance_cpu_balance{app="$app"})"""
  private val url =
    s"https://api.fly.io/prometheus/$org/api/v1/query?query=${URLEncoder.encode(query, StandardCharsets.UTF_8)}"

  private val state = new AtomicReference(State(throttled = false, failures = 0, prevBalance = None))
  // The most recent good balance read (None until the first success / after a run
  // of failures). Exposed for the ThrottleStuckWatchdog's trend guard, which needs
  // the raw number — not just the throttled boolean — to tell recovery from a wedge.
  private val lastBalanceRef = new AtomicReference[Option[Double]](None)
  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("cpu-credit-poller")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(pollOnce()), 0L, pollInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"CpuCreditPoller started: $url every ${pollInterval.toSeconds}s " +
      s"(enter<$enterBelow, exit>$exitAbove, lookahead=${lookaheadSeconds}s).")
  }

  /** One poll: read the balance, fold it into the hysteretic state, log on a
   *  throttle transition. Package-private so a test can drive it without a clock. */
  private[tasks] def pollOnce(): Unit = {
    val balance = Try(http.get(url, Map("Authorization" -> token))).toOption.flatMap(CpuCreditPoller.parseBalance)
    val prev    = state.get()
    val next    = CpuCreditPoller.nextState(prev, balance, enterBelow, exitAbove, maxConsecutiveFailures,
                    pollInterval.toSeconds.toDouble, lookaheadSeconds)
    state.set(next)
    balance.foreach(b => lastBalanceRef.set(Some(b)))
    if (next.throttled != prev.throttled) {
      val reason = if (next.throttled) {
        balance match {
          case Some(b) if b >= enterBelow =>
            // Triggered by projection: balance still above the floor threshold.
            val drainPerSec = prev.prevBalance.flatMap(p => Some((p - b) / pollInterval.toSeconds).filter(_ > 0))
            val minsToFloor = drainPerSec.map(r => (b - enterBelow) / r / 60.0)
            (drainPerSec, minsToFloor) match {
              case (Some(d), Some(m)) => f"projected: ${d}%.0f ms/s drain, ${m}%.1f min to floor"
              case _                  => "projected"
            }
          case _ => s"floor"
        }
      } else "RELEASED"
      logger.warn(s"CpuCreditPoller: throttle $reason " +
        s"(credit=${balance.map(_.round).getOrElse("?")}, enter<$enterBelow, exit>$exitAbove).")
    } else if (balance.isEmpty && next.failures >= maxConsecutiveFailures && prev.failures < maxConsecutiveFailures)
      logger.warn(s"CpuCreditPoller: $maxConsecutiveFailures consecutive credit reads failed — failing open (relying on the external gate).")
  }

  def isThrottled: Boolean = state.get().throttled
  // Credit-driven, not duration-derived — no scrape timing to report for the log.
  def slowScrapeMillis: Long = 0L

  /** The most recent good credit balance, or None until one is read. */
  def lastBalance: Option[Double] = lastBalanceRef.get()

  def stop(): Unit = { scheduler.shutdownNow(); () }
}

object CpuCreditPoller {

  // prevBalance is the last good reading, used to compute the drain rate for
  // projection.  Defaults to None so existing call sites (tests, etc.) don't break.
  private[tasks] case class State(throttled: Boolean, failures: Int, prevBalance: Option[Double] = None)

  /** Pure hysteretic state transition with proactive projection.
   *
   *  Entry conditions (either is sufficient):
   *  - balance < enterBelow  (reactive floor trigger)
   *  - projected time to enterBelow < lookaheadSeconds at current drain rate
   *    (proactive trigger — fires 10–30 min before the floor given the observed
   *    drain rates of 3 000–7 000 ms/min on 2026-06-25)
   *
   *  Exit condition: balance > exitAbove AND the projection is no longer firing.
   *  Both conditions must clear: this prevents the projection-triggered throttle
   *  from releasing on the very next poll (balance was already above exitAbove
   *  when projection fired) and then immediately re-engaging, oscillating every
   *  30 s.  The throttle holds until the drain rate itself eases.
   *
   *  `balance = Some(b)` is a good read; `None` is failed/unparseable — hold the
   *  prior decision until `maxConsecutiveFailures` in a row, then fail open.
   *  `prevBalance` is preserved across failed reads so a transient gap doesn't
   *  reset the slope window.
   */
  private[tasks] def nextState(
    prev: State, balance: Option[Double],
    enterBelow: Long, exitAbove: Long, maxConsecutiveFailures: Int,
    pollIntervalSeconds: Double = 30.0,
    lookaheadSeconds: Long = 600L
  ): State = balance match {
    case Some(b) =>
      // Drain rate from consecutive good readings (ms/s, positive = losing credit).
      val drainPerSec: Option[Double] = prev.prevBalance.flatMap { prevB =>
        val rate = (prevB - b) / pollIntervalSeconds
        if (rate > 0) Some(rate) else None
      }
      // Projected seconds until balance reaches enterBelow at the current rate.
      val projectedLow: Boolean = lookaheadSeconds > 0 && drainPerSec.exists { rate =>
        val secsToFloor = (b - enterBelow) / rate
        secsToFloor > 0 && secsToFloor < lookaheadSeconds
      }
      // The floor-hysteresis arm: hold while balance <= exitAbove (same as before),
      // release when it clears.  The projection arm: hold while drain is still
      // threatening, release when the rate eases (secsToFloor >= lookaheadSeconds).
      // OR-ing them means: stay throttled if EITHER condition holds; exit only when
      // BOTH clear.  This prevents the projection-triggered throttle from bouncing
      // every poll when balance is well above exitAbove.
      val floorHysteresis = if (prev.throttled) !(b > exitAbove) else b < enterBelow
      val throttled = floorHysteresis || projectedLow
      State(throttled, failures = 0, prevBalance = Some(b))

    case None =>
      val failures = prev.failures + 1
      // Preserve prevBalance across failed reads — a transient gap must not reset
      // the slope window; the last known value is still useful context.
      State(
        throttled   = if (failures >= maxConsecutiveFailures) false else prev.throttled,
        failures    = failures,
        prevBalance = prev.prevBalance
      )
  }

  /** Extract the scalar credit balance from a Prometheus instant-query response.
   *  Shape: `{"status":"success","data":{"result":[{"value":[<ts>,"<number>"]}]}}`
   *  — Prometheus encodes the sample value as a STRING. `None` when the body
   *  isn't a success vector with a result (e.g. the series momentarily absent). */
  def parseBalance(body: String): Option[Double] =
    Try(Json.parse(body)).toOption.flatMap { json =>
      if (!(json \ "status").asOpt[String].contains("success")) None
      else (json \ "data" \ "result").asOpt[Seq[JsValue]]
        .flatMap(_.headOption)
        .flatMap(r => (r \ "value").asOpt[Seq[JsValue]])
        .flatMap(_.lift(1))
        .flatMap(_.asOpt[String])
        .flatMap(s => Try(s.toDouble).toOption)
    }
}
