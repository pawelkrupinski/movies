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
  maxConsecutiveFailures: Int = Env.positiveInt("KINOWO_CREDIT_MAX_READ_FAILURES", 3)
) extends Stoppable with ScrapeThrottleSignal with Logging {

  import CpuCreditPoller.State

  private val query = s"""min(fly_instance_cpu_balance{app="$app"})"""
  private val url =
    s"https://api.fly.io/prometheus/$org/api/v1/query?query=${URLEncoder.encode(query, StandardCharsets.UTF_8)}"

  private val state = new AtomicReference(State(throttled = false, failures = 0))
  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("cpu-credit-poller")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(pollOnce()), 0L, pollInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"CpuCreditPoller started: $url every ${pollInterval.toSeconds}s (enter<$enterBelow, exit>$exitAbove).")
  }

  /** One poll: read the balance, fold it into the hysteretic state, log on a
   *  throttle transition. Package-private so a test can drive it without a clock. */
  private[tasks] def pollOnce(): Unit = {
    val balance = Try(http.get(url, Map("Authorization" -> token))).toOption.flatMap(CpuCreditPoller.parseBalance)
    val prev = state.get()
    val next = CpuCreditPoller.nextState(prev, balance, enterBelow, exitAbove, maxConsecutiveFailures)
    state.set(next)
    if (next.throttled != prev.throttled)
      logger.warn(s"CpuCreditPoller: throttle ${if (next.throttled) "ENGAGED" else "RELEASED"} " +
        s"(credit=${balance.map(_.round).getOrElse("?")}, enter<$enterBelow, exit>$exitAbove).")
    else if (balance.isEmpty && next.failures >= maxConsecutiveFailures && prev.failures < maxConsecutiveFailures)
      logger.warn(s"CpuCreditPoller: $maxConsecutiveFailures consecutive credit reads failed — failing open (relying on the external gate).")
  }

  def isThrottled: Boolean = state.get().throttled
  // Credit-driven, not duration-derived — no scrape timing to report for the log.
  def slowScrapeMillis: Long = 0L

  def stop(): Unit = { scheduler.shutdownNow(); () }
}

object CpuCreditPoller {

  private[tasks] case class State(throttled: Boolean, failures: Int)

  /** Pure hysteretic state transition. `balance = Some(b)` is a good read (reset
   *  the failure count); `None` is a failed/unparseable read — hold the prior
   *  decision until `maxConsecutiveFailures` in a row, then fail open (healthy). */
  private[tasks] def nextState(
    prev: State, balance: Option[Double],
    enterBelow: Long, exitAbove: Long, maxConsecutiveFailures: Int
  ): State = balance match {
    case Some(b) =>
      // Enter on a floored balance; once throttled, hold until it clears exitAbove.
      val throttled = if (prev.throttled) !(b > exitAbove) else b < enterBelow
      State(throttled, failures = 0)
    case None =>
      val failures = prev.failures + 1
      State(throttled = if (failures >= maxConsecutiveFailures) false else prev.throttled, failures)
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
