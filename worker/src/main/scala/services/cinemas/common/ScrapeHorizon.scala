package services.cinemas.common

/**
 * How far ahead a venue's programme is scraped — ONE convention, shared by every client
 * that discovers its own day list.
 *
 * It is a SANITY bound, not a coverage target. Each client asks its source which days the
 * venue actually has a programme on and fetches exactly those; this only stops a stray or
 * garbage far-future date fanning a venue out into hundreds of chunk tasks.
 *
 * WHY IT IS DELIBERATELY GENEROUS. These caps used to be per-client budget decisions —
 * Cineworld 35 days, Odeon 160, Flicks 210, Gatsby 210 — each argued on its own terms:
 * the dense near-term block is most of the programme, and the tail beyond it is a handful
 * of single event-cinema dates. That reasoning is sound for what we FETCH and wrong for
 * what we KEEP, because the two are the same listing. `MovieCache`'s scrape-prune reads a
 * film's absence from a listing as "it stopped screening", so any film living only beyond
 * the cap was deleted on every COMPLETE scrape — no failure required.
 *
 * It cost the UK its whole advance-sale programme on 2026-07-27. Flicks (210 days) had
 * been ingesting the tail; the own-site chain clients then became primary with a 35-day
 * horizon, and zero Cineworld showtimes survived past 36 days. What died is exactly the
 * high-value event stock those tails carry: Met Opera, RBO Cinema Season, NT Live,
 * anniversary re-releases.
 *
 * Two years clears every observed horizon with headroom (measured 2026-07-27: Cineworld
 * Sheffield advertises 55 days ending 2027-04-22, Odeon ~4.5 months, Gatsby to
 * 2027-05-30), and a mismatch between a primary and its fallback cannot reappear while
 * they share this number. The cost is bounded by the source's own day list, not by the
 * bound: a venue with a sparse tail plans a few extra chunks, not two years of them.
 */
object ScrapeHorizon {
  val MaxDays: Int = 730
}
