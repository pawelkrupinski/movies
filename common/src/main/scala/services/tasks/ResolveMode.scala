package services.tasks

/** How far a single-film TMDB resolution may override what the row already concluded.
 *
 *  Carried by the `ResolveTmdb` task payload and the inline dispatcher alike, so both
 *  dispatch seams hand `MovieService.resolveTmdbOnce` the same instruction. */
enum ResolveMode {
  /** The normal flow: a remembered miss on the same inputs stands, a resolved row stays. */
  case Normal

  /** A scheduled or operator re-try of an UNRESOLVED row: search even though a remembered
   *  miss covers the row's inputs. The miss stays stored while the search is in flight, so
   *  the row stays concluded — and served — until a new answer replaces it. Clearing it
   *  first took every re-tried card off the site until the next prune (2026-09-23). */
  case RetryMiss

  /** The operator's re-enrich: reset the row to its scraped data and re-resolve it, even
   *  when it already has a `tmdbId`. */
  case Force
}
