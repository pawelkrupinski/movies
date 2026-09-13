package services.movies

/**
 * Observability for `ScrapeLanding`'s two health guards and its per-slot write
 * outcome — what a scrape's landing DECIDED, and whether what it decided actually
 * reached the store. Neither existed before 2026-09-13: both guards logged their
 * decisions (`RemovalAudit.scrapeDepthGuarded` / `scrapePruneSkipped` and their
 * `*Accepted` counterparts) but published no counter, so answering "has this
 * country's depth or breadth guard been stuck rejecting for hours" meant grepping
 * logs by hand — which is exactly how long Kino Aurum's breadth-guard deadlock and
 * the still-open "an accepted reduction didn't reach `screenings`" question took to
 * even notice.
 *
 *  - `recordGuardVerdict(guard, verdict)` — one DEPTH or BREADTH guard decision,
 *    `reject` or `accept`. `Healthy` is not counted: it is the overwhelming default
 *    on every tick of every cinema, and a counter for it would answer nothing a
 *    scrape-completed counter elsewhere doesn't already answer better. A `reject`
 *    RATE that never falls is the CpuBusySustained-shaped signal this exists to let
 *    someone alert on; an `accept` is rarer and worth its own line on a dashboard —
 *    it is the guard visibly giving up on a bad-fetch guess, which is either a real
 *    schedule cut or (Kino Aurum) a scraper that has been broken in the same shape
 *    for hours.
 *  - `recordWriteSkipped(reason)` — a scrape OBSERVED a title this tick but its
 *    write did not land, for a reason that produces no exception and therefore no
 *    log line on its own: `cache-miss-race` is `MovieCache.putIfPresent` returning
 *    `false` because a concurrent rekey of some OTHER title invalidated this key
 *    between the read and the compute (`ScrapeLanding`'s own comment on `landed`
 *    names the mechanism); `unreadable-row` is the cache-miss branch finding the
 *    stored row could not be read at all. Both were already reasoned about in code
 *    comments and handled downstream (spared from the prune via
 *    `listedButNotWritten`) but neither had a counter — a skip this shaped is silent
 *    by construction (nothing throws), so a real, sustained skip and a once-off race
 *    are otherwise indistinguishable without reading `screenings`/`movie_slots`
 *    directly, which is what closing the Kino Aurum investigation needed.
 */
trait ScrapeLandingMetrics {
  def recordGuardVerdict(guard: String, verdict: String): Unit
  def recordWriteSkipped(reason: String): Unit
}

object ScrapeLandingMetrics {
  object Guard { val Depth = "depth"; val Breadth = "breadth" }
  val Guards: Seq[String] = Seq(Guard.Depth, Guard.Breadth)

  object Verdict { val Reject = "reject"; val Accept = "accept" }
  val Verdicts: Seq[String] = Seq(Verdict.Reject, Verdict.Accept)

  object SkipReason { val CacheMissRace = "cache-miss-race"; val UnreadableRow = "unreadable-row" }
  val SkipReasons: Seq[String] = Seq(SkipReason.CacheMissRace, SkipReason.UnreadableRow)

  val noop: ScrapeLandingMetrics = new ScrapeLandingMetrics {
    def recordGuardVerdict(guard: String, verdict: String): Unit = ()
    def recordWriteSkipped(reason: String): Unit                 = ()
  }
}
