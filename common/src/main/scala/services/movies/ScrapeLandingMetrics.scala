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
 *  - `recordWriteSkipped(reason)` — a scrape (or a rating refresh, or a rekey —
 *    every `MovieCache.putIfPresent` caller shares the same seam) OBSERVED a title
 *    but its write did not land. Three reasons, recorded where each is actually
 *    known:
 *      - `cache-miss-race`, inside `putIfPresent` itself, when Caffeine's
 *        `computeIfPresent` returns null — a concurrent rekey of some OTHER title
 *        invalidated this key between the read and the compute
 *        (`ScrapeLanding`'s own comment on `landed` names the mechanism).
 *      - `repository-write-failed`, ALSO inside `putIfPresent`, when
 *        `MovieRepository.updateIfPresent` itself returns `false` — the Mongo
 *        document didn't match, or the write threw and was caught — for a row
 *        that WAS present and readable in the cache the whole time. This is
 *        2026-09-13's OTHER find: `putIfPresent` used to call
 *        `repository.updateIfPresent` and then return `true` UNCONDITIONALLY,
 *        discarding whatever it actually answered — so a genuine persistence
 *        failure was reported as success to every caller, with the in-memory
 *        cache (`corpusIndex.put`, a few lines above the discarded call) already
 *        updated to look correct. `putIfPresent`'s OWN caller (here, or a
 *        rating refresh, or a rekey) cannot tell these two apart from the
 *        returned `Boolean` alone, which is why both are recorded at the source
 *        rather than by whichever caller happens to see the `false`.
 *      - `unreadable-row`, in `ScrapeLanding` itself, on the SEPARATE cache-miss
 *        branch: the stored row could not be read at all, so nothing was ever
 *        resident to race over.
 *    All three were already reasoned about (in code comments, or — the first two
 *    — not even correctly returning `false` before this fix) or handled
 *    downstream (spared from the prune via `listedButNotWritten`), but a skip
 *    this shaped is silent by construction (nothing throws) — a real, sustained
 *    skip and a once-off race were otherwise indistinguishable without reading
 *    `screenings`/`movie_slots` directly, which is what closing the Kino Aurum
 *    investigation needed and could not get without interactive Mongo access.
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

  object SkipReason {
    val CacheMissRace         = "cache-miss-race"
    val UnreadableRow         = "unreadable-row"
    val RepositoryWriteFailed = "repository-write-failed"
  }
  val SkipReasons: Seq[String] = Seq(SkipReason.CacheMissRace, SkipReason.UnreadableRow, SkipReason.RepositoryWriteFailed)

  val noop: ScrapeLandingMetrics = new ScrapeLandingMetrics {
    def recordGuardVerdict(guard: String, verdict: String): Unit = ()
    def recordWriteSkipped(reason: String): Unit                 = ()
  }
}
