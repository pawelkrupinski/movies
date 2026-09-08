package services.movies

import scala.concurrent.duration._

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

  /** Below this many stored slots the RATIO test below is skipped — a venue's ±1/±2
   *  swings would otherwise read as a huge percentage move (2 -> 1 is a "50% drop").
   *  [[MinAbsoluteDropForShrinkGuard]] still applies beneath this floor, so the
   *  exemption protects a small swing without reopening the door to a collapse. */
  val MinSlotsForShrinkGuard: Int = 8

  /** A drop of at least this many slots is not a small venue's day-to-day swing, at
   *  any size — measured against worker-pl history for 68 venues that typically hold
   *  under [[MinSlotsForShrinkGuard]] slots (2026-09-03..09-08, 1858 flat/rising
   *  ticks plus 15 genuine unguarded drops): every one of those drops was 1 or 2
   *  slots, the biggest being Kino Forum's 8 -> 6. CineStars Hood River fell 7 -> 1,
   *  a drop of 6, and was not caught because [[MinSlotsForShrinkGuard]] exempted it
   *  outright (7 < 8) regardless of how big the drop was — the ratio test never ran.
   *  4 sits a full unit above the largest observed legitimate swing and comfortably
   *  below the Hood River collapse; it's also exactly what the ratio test itself
   *  would demand right at the floor (8 slots * 0.5), so a venue one slot short of
   *  the floor is held to the same standard as one just inside it. */
  val MinAbsoluteDropForShrinkGuard: Int = 4

  /** The DEPTH axis, which neither the empty bail nor the breadth guard can see: a
   *  CHUNKED cinema is fetched one task per date, so a bad fetch window loses whole
   *  dates while every film still comes back on the dates that worked — breadth
   *  intact, depth collapsed, and the thin showtime list simply REPLACES the full
   *  one. That is how the UK lost ~70% of its upcoming showtimes on 2026-07-27
   *  while its film count ROSE.
   *
   *  `consecutiveRejections` is how many ticks running this venue has already been
   *  rejected, BEFORE this one. `maxConsecutiveRejections` defaults to the constant
   *  above, but a caller who knows this venue's own scrape cadence should pass
   *  [[maxRejectionsFor]] instead — see that method for why the constant alone isn't
   *  safe everywhere. */
  def depth(knownShowtimes: Int, batchShowtimes: Int, consecutiveRejections: Int,
            maxConsecutiveRejections: Int = MaxConsecutiveDepthRejections): Depth =
    if (knownShowtimes >= MinShowtimesForDepthGuard && batchShowtimes < knownShowtimes * PruneFloorRatio) {
      val consecutive = consecutiveRejections + 1
      if (consecutive <= maxConsecutiveRejections) Depth.Reject(consecutive) else Depth.AcceptDegraded(consecutive)
    } else Depth.Healthy

  /** How long a wall-clock HOLD [[MaxConsecutiveDepthRejections]] actually buys is the
   *  venue's own scrape interval times the tick count — and that interval is not one
   *  constant. `KINOWO_SCRAPE_FRESHNESS_MINUTES` ranges from PL's 60min to ES/UK's
   *  420min, DE's 600min and US's 840min across the fleet
   *  (`infra/kubernetes/worker/overlays/<country>/patch.yaml`), so the SAME "3 rejections"
   *  grace is a 3h hold in Poland and a 42h hold in the US — entirely because of a
   *  config value this guard never saw.
   *
   *  That gap is not theoretical: es/Multicines Zamora — whose upstream (SensaCine)
   *  only ever advertises a 3-day date window for it — was held through three
   *  rejected ticks over 2026-09-07, 21h at its 420min cadence, and its stored
   *  showtimes ran out and served zero films for 7h30m before the fourth tick (a
   *  healthy one) would have landed regardless. The guard's own protection outlasted
   *  the thing it was protecting.
   *
   *  Reduces the tick-count grace so the TOTAL wall-clock hold is capped at
   *  `targetHoldCap`, expressed back in ticks for this venue's own cadence — never
   *  fewer than one tick (a single retry still catches most transient failures: a
   *  Cloudflare challenge, a proxy 503), never more than the tick-count ceiling
   *  above. At PL's 60min cadence this returns 3, unchanged; every slower country
   *  drops to 1. */
  def maxRejectionsFor(scrapeTtl: FiniteDuration, targetHoldCap: FiniteDuration = 3.hours): Int =
    math.max(1, (targetHoldCap / scrapeTtl).toInt) min MaxConsecutiveDepthRejections

  /** The BREADTH axis: is the fresh batch implausibly small against the slots the
   *  venue already holds? If so the end-of-tick prune is skipped — the films this
   *  tick failed to mention keep their slots until a healthy tick.
   *
   *  A caller that KNOWS the listing is short (`listingIsComplete = false`, a
   *  chunked scrape reduced from some of its date-chunks) says so, and that beats
   *  any inference: such a listing returns most of the board, so the ratio never
   *  engages, while silently omitting every film that screens only on a missing
   *  date — the advance-booking titles whose pruning emptied UK venues in July.
   *
   *  Below [[MinSlotsForShrinkGuard]] the ratio test is replaced, not dropped: a
   *  small venue is judged on the ABSOLUTE size of the drop instead
   *  ([[MinAbsoluteDropForShrinkGuard]]), so a ±1/±2 swing still passes but a
   *  near-total collapse — CineStars Hood River, 7 slots to 1 — no longer slips
   *  through purely because the venue started small. */
  def looksPartial(knownSlots: Int, batchSlots: Int, listingIsComplete: Boolean): Boolean =
    !listingIsComplete || {
      if (knownSlots >= MinSlotsForShrinkGuard) batchSlots < knownSlots * PruneFloorRatio
      else knownSlots - batchSlots >= MinAbsoluteDropForShrinkGuard
    }
}
