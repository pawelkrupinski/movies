package services.movies

import models.Country

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
 *  concentrates it like this one. */
@CorpusReplay @CountryScoped
class UnitedStatesConvergenceSpec extends CountryConvergenceBehaviour(Country.UnitedStates, corpusKey = "us")
