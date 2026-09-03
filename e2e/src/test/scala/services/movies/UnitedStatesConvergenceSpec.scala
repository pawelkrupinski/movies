package services.movies

import models.Country

import scala.concurrent.duration._

/** The United States' pipeline reaches a fixpoint.
 *
 *  The widest roster in the suite and the narrowest catalogue behind it: 5,031 venues
 *  across 55 states and territories, all on the one Flicks platform, but 1,312 films
 *  against Germany's 1,783 and the UK's 1,574 (prod, 2026-08-30). That shape matters for
 *  what this leg costs and what it catches. Enrichment — the serial network work that
 *  dominates a leg's wall clock — scales with FILMS, so this leg is not the slowest
 *  despite carrying the most scrapes (2,090) and the most screenings (50k); it fits the
 *  same budget the German and British legs already do.
 *
 *  What the venue count buys instead is fold pressure: one film shown at hundreds of
 *  venues is the shape that finds order-dependence in the merge, and no other country
 *  concentrates it like this one.
 *
 *  It does NOT fit the German and British legs' budget, which this comment used to
 *  claim. Enrichment scaling with films is only half the bill: the scrape walks VENUES,
 *  serially and once per replay, and a cold pass over 4,304 of them is ~147 minutes.
 *  The boot pays it once and each of the three order-independence replays pays it again,
 *  which is 5.5 hours in a job GitHub cancels at 6 — so those replays run in a job of
 *  their own (`convergenceUsOrder`) and this leg excludes them by tag. See
 *  [[OrderIndependence]]. */
@CorpusReplay @CountryScoped
class UnitedStatesConvergenceSpec extends CountryConvergenceBehaviour(
  Country.UnitedStates,
  corpusKey = "us",
  // Just under the 315-minute suite step of the `order-independence` job that runs the
  // replays — see [[OrderIndependence]] for why they need a job of their own. Three
  // concurrent passes over this corpus project to ~4h15m from the UK's measured
  // replays-to-boot ratio; the guard sits above that and below the step, so an overrun
  // fails the step (which reports) instead of cancelling the job (which does not).
  //
  // Inherited by the SAMPLE spec? No — `UnitedStatesSampleConvergenceSpec` constructs the
  // behaviour itself and keeps the default. Its ~100 films replay in seconds, and a
  // five-hour guard on the fast gate would turn a wedge into a cancelled job.
  replayGuard = 290.minutes)
