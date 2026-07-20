package services.movies

import play.api.Logger

/**
 * Central audit log for every place a movie row, cinema slot, showtime, or
 * read-model card is REMOVED. It exists so a "a batch of films disappeared off
 * the site, then reappeared over the next hour" incident is diagnosable from ONE
 * log stream instead of being invisible — the exact gap that made the 2026-07-20
 * served-films sawtooth (a partial Multikino scrape whose prune dropped ~50
 * still-playing films) impossible to attribute post-hoc: none of the removal
 * sites logged what they dropped, from which film, or why.
 *
 * Every removal site in `services.movies` (corpus, screenings, cache),
 * `services.readmodel` (web_movies / web_screenings), `services.staging`, and the
 * worker's `UnscreenedCleanup` routes through here so the format, the id sampling,
 * and the level policy live in ONE place.
 *
 * Level policy — an event that takes a WHOLE film / card out of the corpus, read
 * model, or cache, OR a batch slot-prune, OR a prune the partial-scrape guard
 * SKIPPED, is the "something vanished" signal and logs at INFO. Routine
 * single-slot churn on a healthy scrape logs at DEBUG. All of it flows through the
 * dedicated `kinowo.removal-audit` logger, so an operator can grep that one name
 * or dial its level in `logback.xml` without touching the callers.
 */
object RemovalAudit {

  /** The one logger every removal site writes to — a fixed name (not per-class) so
   *  it can be grepped and level-controlled as a unit. */
  val LoggerName = "kinowo.removal-audit"
  private val logger = Logger(LoggerName)

  /** `n` ids at most, with a `(+k more)` tail, so one line can't dump the corpus. */
  private def sample(ids: Iterable[String], cap: Int = 20): String = {
    val all  = ids.toSeq
    val head = all.take(cap).mkString(", ")
    if (all.sizeIs > cap) s"$head, (+${all.size - cap} more)" else head
  }

  /** One whole film row removed from the corpus (`movies`) or evicted from the cache. */
  def filmRemoved(source: String, id: String, reason: String): Unit =
    logger.info(s"[$source] film removed: id=$id reason=$reason")

  /** A batch of whole film rows removed at once (a fold, an orphan sweep, a
   *  daily unscreened cleanup) — the highest-signal "many films vanished" line. */
  def filmsRemoved(source: String, ids: Iterable[String], reason: String): Unit = {
    val all = ids.toSeq
    if (all.nonEmpty) logger.info(s"[$source] ${all.size} film(s) removed: reason=$reason ids=[${sample(all)}]")
  }

  /** A read-model card (`web_movies` doc + its `web_screenings`) removed — the
   *  point at which a film actually leaves the served site. */
  def cardRemoved(filmId: String, screenings: Int, reason: String): Unit =
    logger.info(s"[read-model] card removed: filmId=$filmId screenings=$screenings reason=$reason")

  /** A single cinema's scrape pruned some of its slots off still-known films — the
   *  summary the served-films sawtooth needed: which cinema, how many films/slots. */
  def scrapePruned(cinema: String, films: Int, slots: Int, sampleFilmIds: Iterable[String], reason: String): Unit =
    if (slots > 0)
      logger.info(s"[scrape-prune] cinema='$cinema' pruned $slots slot(s) across $films film(s): " +
        s"reason=$reason films=[${sample(sampleFilmIds)}]")

  /** The partial-scrape guard SKIPPED a prune (a degraded tick returned implausibly
   *  few films) — logs the decision so a "would have dropped N" is on the record
   *  even though nothing was removed. The counterpart signal to [[scrapePruned]]. */
  def scrapePruneSkipped(cinema: String, batchFilms: Int, knownSlots: Int, reason: String): Unit =
    logger.info(s"[scrape-prune] cinema='$cinema' SKIPPED prune (looks partial): " +
      s"batch=$batchFilms known=$knownSlots reason=$reason")

  /** All of a film's screening slots cleared at once (`whole` = the slot map was
   *  empty, so every slot went) — INFO; a partial stale-slot trim on a healthy
   *  write is routine — DEBUG. */
  def screeningsCleared(source: String, filmId: String, slots: Int, whole: Boolean, reason: String): Unit = {
    val line = s"[$source] screenings ${if (whole) "CLEARED" else "trimmed"}: filmId=$filmId slots=$slots reason=$reason"
    if (whole) logger.info(line) else logger.debug(line)
  }

  /** One cinema slot / showtime removed — routine churn, DEBUG. */
  def slotRemoved(source: String, filmId: String, slotKey: String, reason: String): Unit =
    logger.debug(s"[$source] slot removed: filmId=$filmId slot='$slotKey' reason=$reason")
}
