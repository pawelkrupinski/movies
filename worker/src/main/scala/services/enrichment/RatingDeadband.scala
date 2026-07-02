package services.enrichment

/**
 * Confirmation deadband for a rating source's DISPLAYED value.
 *
 * A rating we fetch is stored "at the precision the badge shows" (see
 * [[RatingDisplay]]). Upstream scores that sit right on a rounding boundary
 * drift back and forth across it as votes/reviews trickle in — Filmweb 3.6↔3.8,
 * IMDb 7.3↔7.4, an RT Tomatometer 67↔66 — so the displayed value flaps A→B→A
 * across refreshes. Each flip is a real row write → change stream →
 * `ReadModelProjector` re-projection, i.e. visible churn for a score that isn't
 * really moving (measured: 15 such A→B→A reversions live in `rating_cadence`).
 *
 * The deadband requires a NEW displayed value to be reported by `confirmations`
 * consecutive refreshes before it's committed to the row. A single-refresh
 * excursion that reverts on the next fetch is never written, so A→B→A produces
 * zero writes; a genuine move that persists commits one refresh later (the
 * accepted "slightly staler rating" trade). `confirmations <= 1` disables the
 * deadband — the first sighting commits immediately (the historical behaviour,
 * kept as the default so only the composition root opts in).
 */
object RatingDeadband {
  /** Deadband off: commit a new value the first time it's seen. */
  val Off: Int = 1

  /** A displayed value awaiting confirmation, and how many consecutive refreshes
   *  have now reported it (including the one that produced this state). */
  final case class Pending(value: String, seen: Int)

  /** Whether to write `fresh` to the row now, and the pending state to carry
   *  forward. `commit == false` with `pending == None` means "nothing to write,
   *  no candidate outstanding" (unchanged, reverted, or nothing fetched). */
  final case class Decision(commit: Boolean, pending: Option[Pending])

  /** Fold one fetched displayed value into the deadband.
   *
   *  - `fresh` empty (no rating fetched) → no write; leave any pending candidate
   *    intact (a failed fetch is no evidence either way).
   *  - `fresh` equals the stored displayed value → no write; clear the pending
   *    candidate (the value settled back to what's shown — the A→B→A case).
   *  - stored empty (first value ever) → commit immediately, whatever
   *    `confirmations` is (nothing to flap against yet).
   *  - `fresh` differs from stored → it's a candidate: commit once it's been
   *    seen `confirmations` times in a row, else hold and count. */
  def decide(stored: Option[String], fresh: Option[String], pending: Option[Pending],
             confirmations: Int): Decision =
    fresh match {
      case None                          => Decision(commit = false, pending = pending)
      case Some(v) if stored.contains(v) => Decision(commit = false, pending = None)
      case Some(_) if stored.isEmpty     => Decision(commit = true,  pending = None)
      case Some(v)                       =>
        val seen = pending.filter(_.value == v).map(_.seen + 1).getOrElse(1)
        if (seen >= confirmations) Decision(commit = true,  pending = None)
        else                       Decision(commit = false, pending = Some(Pending(v, seen)))
    }
}
