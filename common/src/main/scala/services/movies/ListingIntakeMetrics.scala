package services.movies

/**
 * Observability for a scrape's intake (`services.identity.ListingIntake`): its two health guards'
 * verdicts, and whether a cache write actually reached the store. Before 2026-09-13 both guards only
 * logged their decisions, so "has this country's depth or breadth guard been stuck rejecting for hours"
 * meant grepping logs by hand — which is how long Kino Aurum's breadth-guard deadlock took to notice.
 * The exported names (`kinowo_worker_scrape_guard_verdicts_total`, `kinowo_worker_scrape_write_skipped_total`)
 * predate the rename from `ScrapeLandingMetrics` and are kept: dashboards and alerts read them.
 *
 *  - `recordGuardVerdict(guard, verdict)` — one DEPTH or BREADTH guard decision, `reject` or `accept`.
 *    `Healthy` is not counted: it is the overwhelming default on every tick of every cinema. A `reject`
 *    RATE that never falls is what this exists to alert on; an `accept` is the guard visibly giving up
 *    on a bad-fetch guess — a real schedule cut, or a scraper broken in the same shape for hours.
 *  - `recordWriteSkipped(reason)` — a `MovieCache.putIfPresent` write (a rating refresh, a rekey, …)
 *    did not land, recorded inside `putIfPresent`, where each reason is known:
 *      - `cache-miss-race`: Caffeine's `computeIfPresent` returned null — a concurrent rekey of some
 *        OTHER title invalidated this key between the read and the compute.
 *      - `repository-write-failed`: `MovieRepository.updateIfPresent` returned `false` for a row that
 *        WAS present in the cache — the document didn't match, or the write threw and was caught.
 *        `putIfPresent` used to return `true` regardless (2026-09-13), reporting a persistence
 *        failure as success with the cache already updated to look right.
 *    A skip this shaped is silent by construction (nothing throws), so without the counter a
 *    sustained skip and a once-off race were indistinguishable.
 */
trait ListingIntakeMetrics {
  def recordGuardVerdict(guard: String, verdict: String): Unit
  def recordWriteSkipped(reason: String): Unit
}

object ListingIntakeMetrics {
  object Guard { val Depth = "depth"; val Breadth = "breadth" }
  val Guards: Seq[String] = Seq(Guard.Depth, Guard.Breadth)

  object Verdict { val Reject = "reject"; val Accept = "accept" }
  val Verdicts: Seq[String] = Seq(Verdict.Reject, Verdict.Accept)

  object SkipReason {
    val CacheMissRace         = "cache-miss-race"
    val RepositoryWriteFailed = "repository-write-failed"
  }
  val SkipReasons: Seq[String] = Seq(SkipReason.CacheMissRace, SkipReason.RepositoryWriteFailed)

  val noop: ListingIntakeMetrics = new ListingIntakeMetrics {
    def recordGuardVerdict(guard: String, verdict: String): Unit = ()
    def recordWriteSkipped(reason: String): Unit                 = ()
  }
}
