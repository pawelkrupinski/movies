package services.tasks

import java.util.concurrent.atomic.AtomicBoolean

/**
 * A throttle signal toggled from OUTSIDE the worker — the credit-balance logic
 * lives there, not here. An external pusher (a Grafana alert on
 * `fly_instance_cpu_balance`, hitting the worker's `/throttle` endpoint) flips
 * this on when credit drops below its threshold and off once it recovers; all
 * the reapers consult it (composed with the in-process scrape-duration backstop)
 * and back off enqueue while it's on, so the pool idles and credit rebuilds.
 *
 * The worker holds no threshold and reads no metric — it only receives the
 * decision through [[setThrottled]]. A lock-free flag: written by the HTTP
 * thread, read by the reaper threads.
 */
class ExternalThrottleGate extends ScrapeThrottleSignal {
  private val throttled = new AtomicBoolean(false)

  /** Set by the external pusher (e.g. the `/throttle` endpoint). */
  def setThrottled(on: Boolean): Unit = throttled.set(on)

  def isThrottled: Boolean = throttled.get()
  // The gate is driven by credit, not scrape durations — no EWMA to report.
  def ewmaMillis: Long = 0L
}

object ExternalThrottleGate {
  /** The desired throttle state from a `/throttle` request — pure so it's
   *  testable apart from the HTTP plumbing. Accepts a `state=on|off` or
   *  `throttled=true|false` query param (manual/curl), or a Grafana webhook body
   *  whose `"status"` is `"firing"` (credit low → on) / `"resolved"` (→ off).
   *  None ⇒ unrecognised (the endpoint answers 400). */
  def parse(query: Option[String], body: => String): Option[Boolean] = {
    val params = query.getOrElse("").split("&")
      .flatMap(_.split("=", 2) match { case Array(k, v) => Some(k.toLowerCase -> v); case _ => None }).toMap
    params.get("state").map(_.equalsIgnoreCase("on"))
      .orElse(params.get("throttled").map(_.equalsIgnoreCase("true")))
      .orElse {
        val raw = body
        if (raw.contains("\"status\"")) Some(raw.contains("firing")) else None
      }
  }
}
