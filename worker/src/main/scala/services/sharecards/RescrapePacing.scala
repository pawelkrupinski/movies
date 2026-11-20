package services.sharecards

import scala.concurrent.duration.*

import services.facebook.FacebookUsageBrake

/** The pace of the fleet's Facebook re-scrapes, which every country's [[FacebookRescrapeDrain]] keeps. */
object RescrapePacing {
  /** The fleet's pace: one request every 20 s across every country on average, never two closer
   *  than 15 s (the slot schedule's [[TickEvery]] of slack) — 180 an hour, ABOVE what Facebook
   *  allows: it refused the app (code 4) after 45-105 requests each hour on 2026-10-11/12, and each
   *  refusal held the fleet an hour. So the pace is Facebook's own word on the allowance
   *  ([[FacebookUsageBrake.UsageCeiling]]); demand is kept under it by re-scraping only a card whose picture moved
   *  beyond its rating badges ([[ShareCardService.render]]); the pages Facebook holds a preview of go
   *  first, so a burst of new films' pages waits behind them. */
  val Spacing: FiniteDuration = 20.seconds
  /** How often each country looks for due work. Under [[Spacing]], so one country alone keeps up
   *  with the quota. */
  val TickEvery: FiniteDuration = 5.seconds
  /** A claimed entry is held back this long — so an entry whose worker died mid-request is
   *  claimed again, and never while its request (20 s timeout) may still be running. */
  val Lease: FiniteDuration = 5.minutes
  /** How long the whole fleet waits after Facebook names its rate limit: as long as its refusal. */
  val RateLimitHold: FiniteDuration = FacebookUsageBrake.RefusalHold
  /** How long the whole fleet waits after Facebook could not be reached or answered a 5xx. */
  val UnavailableHold: FiniteDuration = 5.minutes
  /** Refusals (or unreadable pages) before an entry is given up. */
  val MaxAttempts: Int = 5
  /** Films expanded per tick, so a backlog of films cannot hold one tick for long. */
  val FilmsPerTick: Int = 20

  /** A refused page's wait before its next attempt: 1, 2, 4, 8 minutes. */
  def backoff(attempts: Int): FiniteDuration = 1.minute * (1L << (attempts - 1).max(0).min(10))
}
