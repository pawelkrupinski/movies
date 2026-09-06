package services.movies

/**
 * Is this tick's listing a believable picture of the venue's board, or a degraded
 * fetch to be ignored? Two pure verdicts, one per axis, with the thresholds beside
 * them. `MovieCache.recordCinemaScrape` asks both before it writes anything and
 * keeps the only state involved — how many ticks in a row the depth guard has
 * rejected a venue — so the decision itself is a function of the counts.
 *
 * Both guards exist because an empty or thin listing is almost always a silent
 * scraper failure (Cloudflare challenge, parser mismatch, proxy 503, a chunked
 * fetch that lost whole dates), not a venue that stopped screening, and acting on
 * it wipes slots that the next healthy tick restores — the visible flicker.
 */
object ScrapeHealth {

  /** The depth guard's answer for one tick. */
  enum Depth {
    /** Fewer showtimes than plausible: keep what is stored, record nothing. */
    case Reject(consecutive: Int)
    /** Still thin, but thin for [[MaxConsecutiveDepthRejections]] ticks running —
     *  the board really has shrunk, so the smaller listing lands. */
    case AcceptDegraded(consecutive: Int)
    case Healthy
  }

  /** Below this many stored showtimes a halving can be real: a two-screen venue
   *  genuinely goes from three showings to one. */
  val MinShowtimesForDepthGuard: Int = 24

  /** A tick returning fewer than this share of what is stored — showtimes for the
   *  depth guard, slots for the breadth guard — is a degraded fetch. Half is a wide
   *  margin: sampling complete chunked runs against the stored count, a healthy
   *  tick reproduces it to within a few percent (PL+DE mean 1.04, sd 0.05, range
   *  0.98–1.10) while the degraded UK ticks of 2026-07-27 sat at 0.09–0.40. */
  val PruneFloorRatio: Double = 0.5

  /** A depth rejection sustained this many ticks stops being a bad fetch. */
  val MaxConsecutiveDepthRejections: Int = 3

  /** Below this many stored slots a venue's ±1 swings never trip the breadth guard. */
  val MinSlotsForShrinkGuard: Int = 8

  /** The DEPTH axis, which neither the empty bail nor the breadth guard can see: a
   *  CHUNKED cinema is fetched one task per date, so a bad fetch window loses whole
   *  dates while every film still comes back on the dates that worked — breadth
   *  intact, depth collapsed, and the thin showtime list simply REPLACES the full
   *  one. That is how the UK lost ~70% of its upcoming showtimes on 2026-07-27
   *  while its film count ROSE.
   *
   *  `consecutiveRejections` is how many ticks running this venue has already been
   *  rejected, BEFORE this one. */
  def depth(knownShowtimes: Int, batchShowtimes: Int, consecutiveRejections: Int): Depth =
    if (knownShowtimes >= MinShowtimesForDepthGuard && batchShowtimes < knownShowtimes * PruneFloorRatio) {
      val consecutive = consecutiveRejections + 1
      if (consecutive <= MaxConsecutiveDepthRejections) Depth.Reject(consecutive) else Depth.AcceptDegraded(consecutive)
    } else Depth.Healthy

  /** The BREADTH axis: is the fresh batch implausibly small against the slots the
   *  venue already holds? If so the end-of-tick prune is skipped — the films this
   *  tick failed to mention keep their slots until a healthy tick.
   *
   *  A caller that KNOWS the listing is short (`listingIsComplete = false`, a
   *  chunked scrape reduced from some of its date-chunks) says so, and that beats
   *  any inference: such a listing returns most of the board, so the ratio never
   *  engages, while silently omitting every film that screens only on a missing
   *  date — the advance-booking titles whose pruning emptied UK venues in July. */
  def looksPartial(knownSlots: Int, batchSlots: Int, listingIsComplete: Boolean): Boolean =
    !listingIsComplete || (knownSlots >= MinSlotsForShrinkGuard && batchSlots < knownSlots * PruneFloorRatio)
}
