package tools

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * Fires every candidate's `probe` CONCURRENTLY, but resolves the winner in the
 * candidates' own PRIORITY order — never "whichever answer lands first". Built
 * for a probe ladder whose ordering is load-bearing: `MetacriticClient` and
 * `RottenTomatoesClient` walk a handful of candidate slugs (year-suffixed
 * before bare, primary before a de-articled variant) where a LATER, wrong-film
 * candidate must never win over an EARLIER, correct one just because its
 * response happened to arrive first (see `MetacriticClientSpec`'s "The
 * Odyssey" regression — the bare `/movie/the-odyssey` is a different film that
 * would 200 just as readily as the correct `/movie/the-odyssey-2026`).
 *
 * `probe` follows the same contract candidates already use via
 * [[EnrichmentRead.absentOnNotFound]]: returning `None` means "this candidate
 * isn't a match, try the next"; throwing means the read itself failed (a
 * block, a timeout, a 5xx — not an absence) and must not be swallowed.
 *
 * A throw propagates the moment its candidate's TURN comes — i.e. once every
 * earlier candidate has been found empty — exactly as a sequential probe would
 * have aborted there, even if a LOWER-priority candidate's future had already
 * completed with a match: that keeps a genuinely failing host from being
 * treated as if it had answered. It does NOT stop the fan-out from firing the
 * later candidates before the failure is observed — that's the accepted cost
 * of concurrency, bounded by keeping the candidate list itself small (a
 * same-title slug-variant list, not a whole cross-title ladder).
 */
object ConcurrentCandidateProbe {

  /** `maxConcurrent` bounds how many probes are ever IN FLIGHT at once, batching
   *  the candidates into successive rounds of that size — the default,
   *  unbounded, is exactly the original one-round-fires-everything behaviour.
   *  A probe whose per-candidate cost is more than a cheap network round-trip
   *  (an image download + decode, not a slug lookup) needs this: firing every
   *  candidate at once multiplies PEAK memory by the candidate count, which is
   *  what OOM-killed `web-pl` racing up to 5 poster fallbacks together
   *  (2026-09-17), when the web's share-card poster loader used it. Rounds are evaluated
   *  lazily (later rounds never start once an earlier one has a match or
   *  throws), so this only trades some latency for a memory ceiling; it does
   *  not change the priority-order or failure-propagation guarantees above. */
  def firstMatch[C, A](label: String, candidates: Seq[C], maxConcurrent: Int = Int.MaxValue)
                       (probe: C => Option[A]): Option[A] = {
    if (candidates.isEmpty) return None
    val ec = DaemonExecutors.virtualThreadEC(label)
    try
      candidates.grouped(math.max(1, maxConcurrent)).map { round =>
        round.map(c => Future(probe(c))(using ec)).iterator
          .map(Await.result(_, Duration.Inf))
          .collectFirst { case Some(a) => a }
      }.collectFirst { case Some(a) => a }
    finally ec.shutdown()
  }
}
