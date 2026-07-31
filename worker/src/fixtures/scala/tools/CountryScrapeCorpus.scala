package tools

import models.{Cinema, Country}

/**
 * The cinemas a country actually serves.
 *
 * This used to also GENERATE a synthetic corpus — invented titles across invented
 * venues — for a country with no recorded one. That fallback is gone, and with it 180
 * lines of title-shape machinery (openers, subjects, qualifiers, dubbed/subtitled/
 * yeared/programme/plus-event variants).
 *
 * It was removed because of what it cost when it fired silently. The UK leg had no
 * corpus fixture, so it replayed generated titles — "Long ogród", "Der lange podróż +
 * spotkanie z twórcami" — which TMDB is quite right to match none of: the leg reported
 * `tmdbId 0, tmdbNoMatch 1831` and looked exactly like a broken enrichment pipeline.
 * That cost eleven runs, a wrong diagnosis, and the deletion of 8,306 correctly-cached
 * negatives before anyone read the titles.
 *
 * A convergence leg without its country's real repertoire is not a weaker version of
 * the suite; it is a different experiment wearing its name. So a missing fixture now
 * fails the leg and names the job that records one, rather than quietly substituting
 * films nobody ships.
 */
object CountryScrapeCorpus {

  /** Every cinema this country actually serves, in catalogue order. */
  def cinemasOf(country: Country): Seq[Cinema] = country.cities.flatMap(_.cinemas).distinct
}
