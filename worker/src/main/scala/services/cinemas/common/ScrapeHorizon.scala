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
   *  "keep going" would walk two years on every upstream blip. But when EVERY probe
   *  threw, the walk throws the first failure: a source that answered nothing is
   *  down, not dormant, and must fail the scrape rather than read as an empty one.
   *
   *  Callers group the result into chunks — widening a per-day scrape must not
   *  multiply chunk TASKS day for day (see
   *  `project_scrape_caps_count_venues_not_tasks`). */
  def liveDays(from: LocalDate, maxEmptyDays: Int = MaxEmptyDays)(hasProgramme: LocalDate => Boolean): Seq[LocalDate] = {
    val lastDay = from.plusDays(MaxDays.toLong)
    walk(Iterator.iterate(from)(_.plusDays(1)).takeWhile(!_.isAfter(lastDay)), maxEmptyDays)(hasProgramme)
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
   *  count a month whose probe THROWS as blank — and throw when every probe did. A
   *  caller whose `hasProgramme` catches its own fetch failures (to keep the pages it
   *  read) must still apply [[ListingPages.requireAnyReached]] to what it caught. */
  def liveMonths(from: YearMonth, maxEmptyMonths: Int = MaxEmptyMonths)(hasProgramme: YearMonth => Boolean): Seq[YearMonth] = {
    val lastMonth = YearMonth.from(from.atDay(1).plusDays(MaxDays.toLong))
    walk(Iterator.iterate(from)(_.plusMonths(1)).takeWhile(!_.isAfter(lastMonth)), maxEmptyMonths)(hasProgramme)
  }

  /** The walk both horizons share: keep the steps that yield something, stop after
   *  `maxEmpty` consecutive blanks — a step whose probe threw counting as one — and
   *  throw the first failure when EVERY probe threw ([[ListingPages.requireAnyReached]]).
   *  That last rule is what keeps a dead upstream from reading as a dormant venue: with
   *  every probe blank-by-failure the walk would otherwise end with no live step, and the
   *  scrape report a successful empty listing. */
  /** How many blank WEEKS in a row end a weekly walk: a month of nothing, the
   *  weekly reading of [[MaxEmptyMonths]]'s rule that a venue publishing a week or
   *  two ahead must not be cut off by one quiet week between programmes. */
  val MaxEmptyWeeks: Int = 3

  /** [[liveDays]] a week per step, for a source that serves a 7-day page per request
   *  (kinoprogramm.com's `?datum=`): the week-start dates that had a programme. */
  def liveWeeks(from: LocalDate, maxEmptyWeeks: Int = MaxEmptyWeeks)(hasProgramme: LocalDate => Boolean): Seq[LocalDate] = {
    val lastDay = from.plusDays(MaxDays.toLong)
    walk(Iterator.iterate(from)(_.plusWeeks(1)).takeWhile(!_.isAfter(lastDay)), maxEmptyWeeks)(hasProgramme)
  }

  private def walk[A](steps: Iterator[A], maxEmpty: Int)(hasProgramme: A => Boolean): Seq[A] = {
    val probes   = Seq.newBuilder[scala.util.Try[Boolean]]
    val live     = Seq.newBuilder[A]
    var emptyRun = 0
    while (steps.hasNext && emptyRun < maxEmpty) {
      val step  = steps.next()
      val probe = scala.util.Try(hasProgramme(step))
      probes += probe
      if (probe.getOrElse(false)) { live += step; emptyRun = 0 }
      else emptyRun += 1
    }
    ListingPages.requireAnyReached(probes.result())
    live.result()
  }
}
