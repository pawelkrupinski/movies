package services

import services.UptimeMonitor.{BucketListener, BucketSnapshot, RecentTotals}

/** The read side of /uptime: what the pages and the Prometheus exposition query — the retained
 *  buckets per service, their recent statuses and errors, the latency averages, the per-service
 *  tags, and a listener fan-out for the live stream. Only the serving app reads it. */
trait UptimeHistory {

  def addListener(f: BucketListener): Unit
  def removeListener(f: BucketListener): Unit

  /** Every service that has recorded (or been polled) at least one bucket. */
  def services: Set[String]

  /** Every retained bucket of `service`, oldest→newest, error strings included. */
  def history(service: String): Seq[BucketSnapshot]

  /** The status keywords of the most recent `limit` buckets that recorded any
   *  activity, oldest→newest. This is everything a row's TRIAGE classification
   *  needs, and it deliberately does not build the row's bar series: the US
   *  registers one service per venue (5,031), so materialising all 96 slots for
   *  every service just to decide which few are red is ~484k objects — enough to
   *  OOM-kill the web pod, which is what it did on 2026-08-31. A green bucket
   *  whose success was `thin` reads as "thin" here, so the classifier can pull a
   *  run of them into their own section. */
  def recentStatuses(service: String, limit: Int): Seq[String]

  /** The error strings recorded by the most recent `limit` buckets that saw any
   *  activity — the same window [[recentStatuses]] classifies, so the two describe
   *  the same buckets. Cheap for the same reason: it walks the bucket map from the
   *  newest end and never materialises a row's 96 slots.
   *
   *  What it is FOR: telling a venue that is broken apart from one that no longer
   *  exists. Both are red, and only the error text separates them (see
   *  [[services.scrapes.GoneUpstream]]). */
  def recentErrors(service: String, limit: Int): Seq[String]

  /** Every service's counts summed over every bucket holding any moment at or after
   *  `cutoff` — what the Prometheus exposition needs, and nothing else. Why it must
   *  not be spelt `services.map(history)`: [[UptimeMonitor.recentTotals]]. */
  def recentTotals(cutoff: Long): Seq[(String, RecentTotals)]

  /** Average duration (ms) of timed successful calls for `service` within the
   *  last hour, or None when nothing was timed in that window. */
  def averageMs1h(service: String): Option[Long]

  /** Average duration (ms) of timed successful calls for `service` across all
   *  retained buckets (up to the 24h Mongo TTL), or None when none were timed. */
  def averageMsTotal(service: String): Option[Long]

  /** Current per-service tags, for the page render. */
  def serviceTagsSnapshot(): Map[String, Set[String]]
}
