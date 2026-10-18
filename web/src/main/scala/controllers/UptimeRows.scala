package controllers

import play.api.libs.json.{JsString, Json, Writes}
import services.UptimeMonitor

case class BarData(
  service: String,
  bucketTimestamp: Long,
  timeFrom: String,
  timeTo: String,
  dateLabel: String,
  status: String,
  successes: Int,
  failures: Int,
  zeroes: Int,
  errors: Seq[String],
  fallback: Boolean = false,
  // A green scrape with no screening in the near term (the worker's NearTermProgramme).
  thin: Boolean = false
)

object BarData {
  implicit val writes: Writes[BarData] = (b: BarData) => Json.obj(
    "service"   -> b.service,
    "bucketTs"  -> b.bucketTimestamp,
    "timeFrom"  -> b.timeFrom,
    "timeTo"    -> b.timeTo,
    "dateLabel" -> b.dateLabel,
    "status"    -> b.status,
    "successes" -> b.successes,
    "failures"  -> b.failures,
    "zeroes"    -> b.zeroes,
    "errors"    -> b.errors,
    "fallback"  -> b.fallback,
    "thin"      -> b.thin
  )
}

/** The per-bucket detail behind the uptime grid, emitted ONCE for the whole page —
 *  and the ONLY place the grid is described: the page's script builds every row's
 *  bars from it, so the server emits no per-bar markup at all.
 *
 *  History. Each bar used to carry an HTML-escaped JSON blob in `data-info`; on
 *  Germany's ~1550 services × 96 slots that was ~57 MB of HTML, which OOM'd the
 *  384 MB heap and (Pekko exits on a fatal error) took the site down. Moving the
 *  detail into one payload fixed the OOM but still rendered ~70k bar `<div>`s with
 *  their timestamps, and a payload of full JSON objects repeating every key name
 *  and every error string per bucket: ~12–17 MB raw per load, built, serialised
 *  and gzipped on every request — 0.6–1.3 s server-side in prod (Caddy, 2026-10-03).
 *
 *  Compact shape, every repeated value written once and referenced by index:
 *  {{{
 *  { "slots":    [[ts, timeFrom, timeTo, dateLabel], …],   // the grid's columns, oldest first
 *    "statuses": ["green", …],
 *    "errors":   ["connect timeout", …],
 *    "data":     { service: [[slot, status, successes, failures, zeroes, flags, error…], …] } }
 *  }}}
 *  `flags` is a bitmask — 1 fallback, 2 thin. Only buckets that recorded something
 *  get an entry; an absent bucket IS the "no data" case. Safe to drop straight into
 *  a `<script type="application/json">` block: `<` is escaped so an error string
 *  containing `</script>` can't break out. */
object UptimeBarPayload {

  val FallbackFlag = 1
  val ThinFlag     = 2

  def apply(rows: Seq[ServiceRow]): String = {
    val allRows = rows.flatMap(r => r +: r.enrichment.toSeq)

    // Every row normally shares one grid; the union covers any that doesn't.
    val slotBars = scala.collection.mutable.LongMap.empty[BarData]
    allRows.foreach(_.bars.foreach(b => if (!slotBars.contains(b.bucketTimestamp)) slotBars(b.bucketTimestamp) = b))
    val slots     = slotBars.values.toSeq.sortBy(_.bucketTimestamp)
    val slotIndex = slots.iterator.map(_.bucketTimestamp).zipWithIndex.toMap

    val statuses = new Interned
    val errors   = new Interned
    val out      = new StringBuilder(64 * 1024)

    out ++= "{\"slots\":["
    writeSlots(out, slots)
    out ++= "],\"data\":{"
    writeData(out, allRows, slotIndex, statuses, errors)
    out ++= "},\"statuses\":"
    quoteAll(out, statuses.values)
    out ++= ",\"errors\":"
    quoteAll(out, errors.values)
    out += '}'
    ScriptJson.escape(out.toString)
  }

  private def writeSlots(out: StringBuilder, slots: Seq[BarData]): Unit =
    slots.iterator.zipWithIndex.foreach { case (b, i) =>
      if (i > 0) out += ','
      out += '[' ++= b.bucketTimestamp.toString += ','
      quote(out, b.timeFrom) += ','; quote(out, b.timeTo) += ','; quote(out, b.dateLabel) += ']'
    }

  private def writeData(out: StringBuilder, allRows: Seq[ServiceRow], slotIndex: Map[Long, Int],
                        statuses: Interned, errors: Interned): Unit = {
    // A service rendered in two sections (triage AND its city) is written once.
    val written = scala.collection.mutable.HashSet.empty[String]
    allRows.foreach { row =>
      val active = row.bars.filter(_.status != "empty")
      if (active.nonEmpty && written.add(row.name)) {
        if (written.size > 1) out += ','
        quote(out, row.name) ++= ":["
        active.iterator.zipWithIndex.foreach { case (b, i) =>
          if (i > 0) out += ','
          writeBucket(out, b, slotIndex, statuses, errors)
        }
        out += ']'
      }
    }
  }

  private def writeBucket(out: StringBuilder, b: BarData, slotIndex: Map[Long, Int],
                          statuses: Interned, errors: Interned): Unit = {
    val flags = (if (b.fallback) FallbackFlag else 0) | (if (b.thin) ThinFlag else 0)
    out += '[' ++= slotIndex(b.bucketTimestamp).toString += ',' ++= statuses.of(b.status).toString +=
      ',' ++= b.successes.toString += ',' ++= b.failures.toString += ',' ++= b.zeroes.toString += ',' ++= flags.toString
    b.errors.foreach(e => out += ',' ++= errors.of(e).toString)
    out += ']'
  }

  /** Each distinct string once, in first-seen order, referenced by its index. */
  private final class Interned {
    private val index = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    def of(s: String): Int = index.getOrElseUpdate(s, index.size)
    def values: Iterable[String] = index.keys
  }

  private def quote(out: StringBuilder, s: String): StringBuilder = out ++= Json.stringify(JsString(s))

  private def quoteAll(out: StringBuilder, values: Iterable[String]): Unit = {
    out += '['
    values.iterator.zipWithIndex.foreach { case (v, i) => if (i > 0) out += ','; quote(out, v) }
    out += ']'
  }
}

/** A venue every source dropped, as the closure sweep found it (`verdict`), and the evidence line it was raised on. */
case class DroppedRow(cinema: String, verdict: String, since: String, evidence: String)

case class FallbackRow(
  cinema: String, source: String, ref: String, since: String, reason: String,
  fails: Int, nextProbe: String, history: Seq[String]
)

case class ServiceRow(
  name:       String,
  bars:       Seq[BarData],
  avg1hMs:    Option[Long]       = None,
  avgTotalMs: Option[Long]       = None,
  // The cinema's deferred-detail enrichment health, rendered as an indented
  // sub-row beneath the scrape bar. None for non-cinema rows / cinemas with no
  // deferred detail.
  enrichment: Option[ServiceRow] = None,
  // Generic per-row labels (UptimeMonitor service tags) rendered as chips next to
  // the name — e.g. the cinema's scraper-client marker "shared:FilmwebShowtimesClient".
  tags:       Set[String]        = Set.empty
) {
  /** The venue's public source-page URL, parsed out of the `url:` tag — the
   *  href the name links to. Same extractor /debug uses (one source of truth). */
  def url: Option[WebHref] = UptimeMonitor.urlFromTags(tags).flatMap(WebHref.of)
}
