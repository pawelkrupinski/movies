package tools

import models.City

/**
 * Which cities the app actually scrapes, resolved from the
 * `KINOWO_SCRAPE_CITIES` override (comma-separated city slugs, e.g.
 * `poznan,wroclaw`). Pulled out of `Wiring` as a pure function so the parse +
 * default rule is unit-testable without standing up the composition root.
 *
 * History: the catalogue jumped from ~11 (Poznań) to ~48 (Wrocław + Warszawa)
 * and the single-vCPU box couldn't keep up, so the scrape set was gated by city
 * to bring them back one at a time. The box is now 2 vCPU and every modelled
 * city has a wired scraper, so the default is `allCities` — the override now
 * exists only to NARROW the scrape set (e.g. to shed load if the worker
 * throttles or OOMs: `fly secrets set KINOWO_SCRAPE_CITIES=poznan,wroclaw`).
 */
object ScrapeCities {

  /** Every modelled city's slug — the production default scrape set. Derived
   *  from `City.all` so a newly modelled city is scraped automatically, without
   *  re-spelling the list here, in the wiring, or in the test seams. */
  val allCities: Set[String] = City.all.map(_.slug).toSet
}
