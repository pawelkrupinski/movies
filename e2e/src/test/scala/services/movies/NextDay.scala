package services.movies

import org.scalatest.Tag

/**
 * The next-day test of [[CountryConvergenceBehaviour]] (and the city rename inside it), so a leg past
 * the 10-minute target can run it on a boot of its own, beside the rest of the leg rather than after it.
 *
 * Separable because it reads only the booted corpus: the leg's first test asserts that the boot is
 * already a fixpoint, so the corpus it takes the next day's listings over is the one it would have
 * met after the other tests. The United States' identity-lane leg splits it out
 * (`convergenceUsWithoutNextDay` / `convergenceUsNextDay`): its 135 s of re-scrape, re-enrichment
 * and re-projection kept an 11.4-minute row over budget (run 38553692147). Every other leg and lane
 * keeps it inline.
 */
object NextDay extends Tag("services.movies.NextDay")
