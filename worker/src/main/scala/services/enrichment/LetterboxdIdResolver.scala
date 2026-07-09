package services.enrichment

/**
 * ID-bridge strategy over [[LetterboxdClient]]: given whichever external id a
 * film already has, recover the one it's missing. The cross-check — the fetched
 * page must echo back the id we queried with — guards against a Letterboxd
 * redirect that lands on the wrong (or an empty) film page silently handing us
 * a mismatched id.
 *
 * Intended wiring: a fallback in the tmdbId / imdbId resolution chain, tried
 * only when the primary resolvers (TMDB fuzzy search, TMDB `/find`, Wikidata)
 * leave the id blank — i.e. the arthouse/festival long tail.
 */
class LetterboxdIdResolver(client: LetterboxdClient) {

  /** Recover a missing tmdbId from a known IMDb id (`tt…`), or None. Rejects a
   *  page whose echoed IMDb id doesn't match the one we queried with. */
  def resolveTmdbId(imdbId: String): Option[Int] =
    client.byImdbId(imdbId)
      .filter(_.imdbId.exists(_.equalsIgnoreCase(imdbId.trim)))
      .flatMap(_.tmdbId)

  /** Recover a missing imdbId (`tt…`) from a known TMDB movie id, or None.
   *  Rejects a page whose echoed TMDB id doesn't match the one we queried. */
  def resolveImdbId(tmdbId: Int): Option[String] =
    client.byTmdbId(tmdbId)
      .filter(_.tmdbId.contains(tmdbId))
      .flatMap(_.imdbId)
}
