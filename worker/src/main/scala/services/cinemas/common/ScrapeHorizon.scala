package services.cinemas.common

import java.time.{LocalDate, YearMonth}

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

  /** How many consecutive blank days end a [[liveDays]] walk.
   *
   *  A stop rule, not a horizon — [[MaxDays]] is the bound. A fortnight clears the
   *  gaps these venues actually leave between runs (measured 2026-08-05, the
   *  sparsest of them — Kino Astra, screening four scattered days over a month —
   *  never went more than seven blank days), and a dormant venue costs fourteen
   *  small requests per pass and no more. */
  val MaxEmptyDays: Int = 14

  /** The days a venue actually has a programme on, for a source that will answer
   *  for ANY date but never says which ones it holds.
   *
   *  Walk forward from `from`, keep the days that yield something, and stop after
   *  `maxEmptyDays` consecutive blanks — bounded by [[MaxDays]], so a venue that
   *  keeps publishing keeps being read while a dormant one costs `maxEmptyDays`
   *  requests and no more. This is the alternative to guessing a window: a fixed
   *  one silently hides everything past it, which is what this object exists to
   *  forbid, and what hid Nowe Horyzonty's whole retrospective programme.
   *
   *  A day whose probe THROWS counts as blank, as [[liveMonths]] treats a failed
   *  month: a missing day cannot be told from a quiet one, and treating it as
   *  "keep going" would walk two years on every upstream blip.
   *
   *  Callers group the result into chunks — widening a per-day scrape must not
   *  multiply chunk TASKS day for day (see
   *  `project_scrape_caps_count_venues_not_tasks`). */
  def liveDays(from: LocalDate, maxEmptyDays: Int = MaxEmptyDays)(hasProgramme: LocalDate => Boolean): Seq[LocalDate] = {
    val lastDay = from.plusDays(MaxDays.toLong)
    var day      = from
    var emptyRun = 0
    val live     = Seq.newBuilder[LocalDate]
    while (!day.isAfter(lastDay) && emptyRun < maxEmptyDays) {
      if (scala.util.Try(hasProgramme(day)).getOrElse(false)) { live += day; emptyRun = 0 }
      else emptyRun += 1
      day = day.plusDays(1)
    }
    live.result()
  }

  /** How many consecutive blank months end a [[liveMonths]] walk.
   *
   *  Three, not two. MSI venues were measured publishing one to two months ahead, so
   *  a threshold of two cannot see a programme that resumes after a two-month summer
   *  gap — the situation half those venues were in. A month is a far coarser probe
   *  than a day, so three of them is already a quarter of silence, and a dormant
   *  venue still costs only three requests. */
  val MaxEmptyMonths: Int = 3

  /** The months a venue has a programme in — [[liveDays]] for portals that publish
   *  a page per calendar month rather than per day.
   *
   *  Same contract: walk forward from `from`, keep what yields something, stop after
   *  `maxEmptyMonths` consecutive blanks, bound the whole thing by [[MaxDays]], and
   *  count a month whose probe THROWS as blank. Callers that need to tell "every
   *  month failed" from "the venue is quiet" — a portal that is down must not read as
   *  a dormant venue — keep their own record of the attempts inside `hasProgramme`. */
  def liveMonths(from: YearMonth, maxEmptyMonths: Int = MaxEmptyMonths)(hasProgramme: YearMonth => Boolean): Seq[YearMonth] = {
    val lastMonth = YearMonth.from(from.atDay(1).plusDays(MaxDays.toLong))
    var month     = from
    var emptyRun  = 0
    val live      = Seq.newBuilder[YearMonth]
    while (!month.isAfter(lastMonth) && emptyRun < maxEmptyMonths) {
      if (scala.util.Try(hasProgramme(month)).getOrElse(false)) { live += month; emptyRun = 0 }
      else emptyRun += 1
      month = month.plusMonths(1)
    }
    live.result()
  }
}
