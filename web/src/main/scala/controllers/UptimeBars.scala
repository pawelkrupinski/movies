package controllers

import services.UptimeHistory
import services.UptimeMonitor.{BucketDurationMs, BucketSnapshot, MaxBuckets, bucketTimestamp}

import java.time.format.DateTimeFormatter
import java.time.{Clock, Instant, ZoneId}
import java.util.Locale

/** The uptime grid's bars, every bucket window stamped in the deployment's country's time (the UK
 *  page in London time, not Warsaw's): each service's 96-slot series for the page ([[rowBuilder]])
 *  and each changed bucket the live feed pushes ([[bar]]), so both label a bucket the same. */
final class UptimeBars(monitor: UptimeHistory, zone: ZoneId, clock: Clock) {

  private val formats = UptimeBars.Formats(zone)
  import formats.{dateFmt, timeFmt, tsFmt}

  /** One rendered bar, with its bucket window stamped in the country's time. */
  def bar(service: String, b: BucketSnapshot): BarData =
    bar(service, b, slotLabels(b.timestamp))

  private def bar(service: String, b: BucketSnapshot, labels: SlotLabels): BarData =
    BarData(service, b.timestamp, labels.timeFrom, labels.timeTo, labels.dateLabel,
      b.status, b.successes, b.failures, b.zeroes, b.errors, b.fallback, b.thin)

  private final case class SlotLabels(timeFrom: String, timeTo: String, dateLabel: String)

  private def slotLabels(timestamp: Long): SlotLabels = {
    val from = Instant.ofEpochMilli(timestamp)
    SlotLabels(timeFmt.format(from), timeFmt.format(Instant.ofEpochMilli(timestamp + BucketDurationMs)), dateFmt.format(from))
  }

  /** Builds one service's rendered row — its 96-slot bar series up to the current
   *  bucket, its timings and tags — against slots labelled once per request, not
   *  once per bar: every row shares them. */
  def rowBuilder(): String => ServiceRow = {
    val currentBucket = bucketTimestamp(clock.millis())
    val slots = (0 until MaxBuckets).reverse.map { i =>
      val timestamp = currentBucket - i * BucketDurationMs
      timestamp -> slotLabels(timestamp)
    }
    val tags = monitor.serviceTagsSnapshot()
    name => ServiceRow(name, barsFor(name, slots), monitor.averageMs1h(name), monitor.averageMsTotal(name),
      tags = tags.getOrElse(name, Set.empty))
  }

  private def barsFor(serviceName: String, slots: Seq[(Long, SlotLabels)]): Seq[BarData] = {
    val history = monitor.history(serviceName).map(b => b.timestamp -> b).toMap
    slots.map { case (timestamp, labels) =>
      bar(serviceName, history.getOrElse(timestamp, BucketSnapshot(timestamp, 0, 0, 0, Seq.empty)), labels)
    }
  }

  /** An instant as the page's other sections stamp it (the fallbacks, the dropped venues). */
  def stamp(instant: Instant): String = tsFmt.format(instant)
}

object UptimeBars {
  private[controllers] final case class Formats(zone: ZoneId) {
    val timeFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm", Locale.ENGLISH).withZone(zone)
    val dateFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("d MMM", Locale.ENGLISH).withZone(zone)
    val tsFmt:   DateTimeFormatter = DateTimeFormatter.ofPattern("d MMM HH:mm", Locale.ENGLISH).withZone(zone)
  }
}
