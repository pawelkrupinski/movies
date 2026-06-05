package tools

/**
 * Which cities the app actually scrapes, resolved from the
 * `KINOWO_SCRAPE_CITIES` override (comma-separated city slugs, e.g.
 * `poznan,wroclaw`). Pulled out of `Wiring` as a pure function so the parse +
 * default rule is unit-testable without standing up the composition root.
 *
 * Motivation: the cinema catalogue jumped from ~11 (Poznań) to ~48 (adding
 * Wrocław + Warszawa) and the single-vCPU box couldn't keep up — the 30s cache
 * rehydrate + scrapes + first-time enrichment pegged CPU and heavy pages stalled.
 * Gating the scrape set by city lets us bring cities back one at a time
 * (`fly secrets set KINOWO_SCRAPE_CITIES=poznan,wroclaw` + restart) and watch
 * the load, instead of guessing at hardware.
 */
object ScrapeCities {

  /** The set of enabled city slugs (lowercased) parsed from `envValue`, or
   *  `default` when the override is unset, blank, or has no usable tokens.
   *  Tokens are trimmed + lowercased and empties dropped, so
   *  `" Poznan , wroclaw ,"` → `Set("poznan", "wroclaw")`. */
  def enabled(envValue: Option[String], default: Set[String]): Set[String] =
    envValue
      .map(_.split(",").iterator.map(_.trim.toLowerCase).filter(_.nonEmpty).toSet)
      .filter(_.nonEmpty)
      .getOrElse(default)
}
