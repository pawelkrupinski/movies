package tools

import play.api.Logger

import java.time.{Duration, Instant}

/**
 * Logs a failure once, not on every ask that hits it again.
 *
 * A URL whose fetch keeps failing the same way is asked again and again — the fixture
 * stack remembers a 403 so no network is spent, but every ask still reached the warning.
 * One Record-scrape-fixtures UK leg logged 12,397 nine-line `All N backends failed`
 * warnings (~110k of its 382k lines) for 136 distinct Cineworld URLs, each asked ~1,300
 * times; production workers log the same way.
 *
 * So the FIRST failure for a key is warned in full, and a later failure with the IDENTICAL
 * detail is logged at DEBUG and counted. What is never hidden:
 *  - a new key, or a failure whose detail differs (another status, another backend's
 *    reason) — warned in full, naming how many repeats of the previous one were held back;
 *  - a recovery — [[cleared]] logs at INFO when a remembered failure stops failing;
 *  - a long-running repeat — re-warned in full once per `relogEvery`, with its count, so a
 *    failure that persists for a whole worker lifetime still surfaces periodically.
 *
 * Only the LOG line is deduplicated; callers still get the full exception every time.
 *
 * Memory is bounded: at most `maxEntries` keys (least-recently-asked evicted first), each
 * holding the detail's hash rather than the detail. An evicted key with held-back repeats
 * says so at INFO, and simply warns afresh if it fails again.
 *
 * State is per instance — each `FallbackHttpFetch` owns one.
 */
final class RepeatedFailureLog(logger: Logger, settings: RepeatedFailureLog.Settings) {
  import RepeatedFailureLog.Entry

  private val entries = new java.util.LinkedHashMap[String, Entry](16, 0.75f, true) {
    override def removeEldestEntry(eldest: java.util.Map.Entry[String, Entry]): Boolean = {
      val evict = size() > settings.maxEntries
      if (evict && eldest.getValue.suppressed > 0)
        logger.info(s"${eldest.getKey} — ${eldest.getValue.suppressed} repeat(s) of its failure were not logged " +
                    "(no longer tracked)")
      evict
    }
  }

  /** Records a failure of `key` with `detail`, warning unless it repeats the last one. */
  def failed(key: String, detail: String): Unit = {
    val hash = detail.hashCode
    val at   = settings.now()
    val line: Option[String] = entries.synchronized {
      Option(entries.get(key)) match {
        case Some(e) if e.detailHash == hash && Duration.between(e.loggedAt, at).compareTo(settings.relogEvery) < 0 =>
          entries.put(key, e.copy(suppressed = e.suppressed + 1))
          None
        case Some(e) if e.detailHash == hash =>
          entries.put(key, Entry(hash, at, 0))
          Some(s"$detail\n  (still failing; ${e.suppressed} identical repeat(s) since last logged)")
        case Some(e) =>
          entries.put(key, Entry(hash, at, 0))
          Some(if (e.suppressed > 0) s"$detail\n  (changed after ${e.suppressed} unlogged repeat(s) of the previous failure)"
               else detail)
        case None =>
          entries.put(key, Entry(hash, at, 0))
          Some(detail)
      }
    }
    line match {
      case Some(l) => logger.warn(l)
      case None    => logger.debug(s"$key — same failure as already logged; not repeating it at WARN")
    }
  }

  /** `key` produced an outcome other than a logged failure (an answer, a not-found). A
   *  remembered failure ending is news, so it is logged; otherwise nothing is. */
  def cleared(key: String, outcome: String): Unit = {
    val removed = entries.synchronized(Option(entries.remove(key)))
    removed.foreach(e => logger.info(s"$key — $outcome after its logged failure (${e.suppressed} unlogged repeat(s))"))
  }

  private[tools] def tracked: Int = entries.synchronized(entries.size)
}

object RepeatedFailureLog {
  /** Keys tracked at most: past what one instance's failing URLs number, or a round of asks evicts each before
   *  it comes again and every repeat warns. A convergence leg's detail phase cycles ~4,500 pages its recording
   *  lacks; at 1,024 that was ~235,000 warning lines in one 37 s phase (run 37595419218). An entry is a URL and
   *  a hash — a few hundred bytes — and only failing keys are held. */
  final case class Settings(maxEntries: Int = 16384,
                            relogEvery: Duration = Duration.ofHours(1),
                            now: () => Instant = () => Instant.now()) {
    require(maxEntries > 0, "maxEntries must be positive")
  }

  private final case class Entry(detailHash: Int, loggedAt: Instant, suppressed: Int)
}
