package services.cadence

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
import scala.util.hashing.MurmurHash3

/**
 * Pure shaping of the `rating_cadence` records into the dev cadence page's view
 * model: per (source, film) entries grouped by their current refresh interval,
 * groups ordered by interval DESCENDING (the most backed-off / stable films
 * first). Kept here (not in the web view) so the grouping is unit-testable.
 */
object CadenceReport {

  /** One (source, film) row: its current interval + streak, volatility counts, the
   *  last two displayed-value changes, and when the next refresh is due. */
  case class Entry(
    site:          String,
    title:         String,
    interval:      FiniteDuration,
    nextRefreshAt: Instant,
    streak:        Int,
    windowChecks:  Int,
    windowChanges: Int,
    lastChange:    Option[RatingChange],
    prevChange:    Option[RatingChange]
  )

  /** Films sharing one refresh interval. */
  case class Group(interval: FiniteDuration, entries: Seq[Entry])

  /** Compact label for a refresh interval: `2h`, `16h`, `2d`, `4d`. */
  def intervalLabel(d: FiniteDuration): String = {
    val hours = d.toHours
    if (hours >= 24 && hours % 24 == 0) s"${hours / 24}d"
    else if (hours >= 1)                s"${hours}h"
    else                                s"${d.toMinutes}m"
  }

  /** When this `(source, film)` is next due for a refresh: the next period-boundary
   *  strictly after its last check. MIRRORS [[services.tasks.DueWindow]]'s phase-spread
   *  windowing (boundaries at `phase + n·period`, `phase = hash(key) mod period`),
   *  so the page's "next refresh" matches what the reaper actually does. Always in
   *  `(lastCheckedAt, lastCheckedAt + interval]`. The reaper still ticks on a cadence
   *  and gates on throttle/caps, so it's an estimate, not a guarantee. */
  def nextRefreshAt(key: String, lastCheckedAt: Instant, interval: FiniteDuration): Instant = {
    val period       = interval.toMillis
    val phase        = Math.floorMod(MurmurHash3.stringHash(key).toLong, period)
    val last         = lastCheckedAt.toEpochMilli
    val nextBoundary = phase + (Math.floorDiv(last - phase, period) + 1) * period
    Instant.ofEpochMilli(nextBoundary)
  }

  private val TmdbKey = """(.+)\|tmdb:(\d+)""".r

  /** Split a cadence dedup key into `(site, tmdbId?)`: `imdb|tmdb:42` →
   *  `("imdb", Some(42))`; a legacy title key `imdb|Dune|2024` → `("imdb", None)`. */
  def parseKey(key: String): (String, Option[Int]) = key match {
    case TmdbKey(site, id) => (site, id.toIntOption)
    case other             => (other.takeWhile(_ != '|'), None)
  }

  /** Group the records by current refresh interval, descending. `titleFor` maps a
   *  tmdbId to its film title; an unresolved id (or legacy key) falls back to the
   *  raw key so the row is still identifiable. Entries within a group are sorted by
   *  title then site for a stable render. */
  def build(records: Seq[(String, RatingChangeStats)], titleFor: Int => Option[String]): Seq[Group] = {
    val entries = records.map { case (key, stats) =>
      val (site, tmdbId) = parseKey(key)
      val title    = tmdbId.flatMap(titleFor).orElse(tmdbId.map(id => s"tmdb:$id")).getOrElse(key)
      val interval = RatingCadence.intervalFor(Some(stats))
      Entry(site, title, interval, nextRefreshAt(key, stats.lastCheckedAt, interval), stats.unchangedStreak,
        stats.windowChecks, stats.windowChanges, stats.lastChange, stats.prevChange)
    }
    entries.groupBy(_.interval).toSeq
      .sortBy(-_._1.toMillis)
      .map { case (interval, es) => Group(interval, es.sortBy(e => (e.title.toLowerCase, e.site))) }
  }
}
