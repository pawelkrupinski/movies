package services.tasks

import play.api.Logging
import tools.ProxyProbe

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap

/**
 * The cinemas parked because the residential proxy is down — parked on that CAUSE, not on their own failure count —
 * and the one probe that decides both ends of it: a venue is parked here only when its scrape failed on the proxy leg
 * AND the probe says the proxy is down, and every venue parked here is released the moment the same probe passes
 * again. The check that parked it is the check that un-parks it.
 *
 * WHY (2026-10-27): Decodo answered every CONNECT with 503 from 17:00Z. Each proxied venue spent its retry budget and
 * was parked on its normal freshness window by [[ScrapeFreshnessPolicy]] — 637 in PL, 189 in the UK — so the proxy
 * coming back would have been noticed one venue at a time, up to a whole window later (UK/ES 7h, DE 10h, US 14h).
 * Parked here, they are all due again within one [[ProxyParking.ProbeInterval]] of the proxy answering.
 *
 * In memory, like the failure streak beside it: a restart forgets who is parked here, and those venues fall back on
 * the normal window they were stamped onto when parked — never worse than before this existed.
 */
final class ProxyParking(probe: ProxyProbe, clock: Clock, interval: FiniteDuration = ProxyParking.ProbeInterval)
    extends Logging {

  private val parked = ConcurrentHashMap.newKeySet[String]()

  // The last verdict and when it was taken: a park asks the probe at most once per `interval`, however many venues
  // fail inside it.
  @volatile private var verdict: Option[(Boolean, Instant)] = None

  /** Park `key` on the proxy being down — if the probe says it is. False (and nothing parked) when it passes: the
   *  venue's failure is then its own. */
  def parkIfProxyDown(key: String): Boolean = {
    val down = !probeAtMost(interval)
    if (down) { parked.add(key); () }
    down
  }

  /** `key` scraped fine: whatever parked it is over. */
  def released(key: String): Unit = { parked.remove(key); () }

  /** How many venues are parked on the proxy right now. */
  def size: Int = parked.size

  def isParked(key: String): Boolean = parked.contains(key)

  /** The recovery check: with anything parked, ask the probe afresh; when it passes, hand back every parked venue
   *  (and forget them). Nothing parked → no probe at all. */
  def releaseIfRecovered(): Seq[String] =
    if (parked.isEmpty) Nil
    else if (!probeAtMost(Duration.Zero)) Nil
    else {
      val released = parked.asScala.toSeq.sorted
      released.foreach(parked.remove)
      released
    }

  private def probeAtMost(maxAge: FiniteDuration): Boolean = {
    val now = clock.instant()
    verdict.collect { case (passes, at) if maxAge > Duration.Zero && at.plusMillis(maxAge.toMillis).isAfter(now) => passes }
      .getOrElse {
        val passes = probe.passes()
        if (verdict.forall(_._1 != passes)) {
          val was = verdict.fold("unknown")(v => if (v._1) "passing" else "failing")
          logger.warn(s"residential proxy probe: ${if (passes) "PASSES" else "FAILS"} (was $was)")
        }
        verdict = Some(passes -> now)
        passes
      }
  }
}

object ProxyParking {

  /** How often the recovery check probes while anything is parked — the most a recovered proxy waits to be noticed. */
  val ProbeInterval: FiniteDuration = 30.seconds

  /** The causes a venue is parked on, as the `kinowo_worker_scrapes_parked` gauge labels them. */
  object Cause {
    val ResidentialProxyDown = "residential_proxy_down"
    val OwnFailures          = "own_failures"
    val all: Seq[String]     = Seq(ResidentialProxyDown, OwnFailures)
  }
}
