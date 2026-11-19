package services.enrichment

import services.resolution.{ResolutionCache, ResolutionKeys}
import services.titles.{SearchTitles, TitleNormalizer}

/** IMDb's own rungs of the id ladder ([[ImdbIdLadder]]): its suggestion endpoint, cached, and its director-confirmed
 *  search. */
final class ImdbSuggestions(
  imdb: ImdbClient,
  // Caches the IMDb suggestion lookup keyed by (search title, year), so the same
  // search resolves once for 24h across every path that asks it.
  imdbIdCache: ResolutionCache,
  // The corpus's title rules, which shape the query and its cache key.
  normalizer: TitleNormalizer
) {

  /**
   * Cached IMDb-id lookup shared by both call sites. Hits-only — a no-match
   * re-queries next time.
   *
   * Tries the title AS GIVEN first, then its DE-DECORATED forms, because a cinema's
   * programme banner is part of the row's title and IMDb has never heard of it. TMDB
   * has always been asked both ways — `resolveTmdbId` builds its candidates through
   * `searchTitleCandidates`, which splits a `"X | Y"` pipe and drops a trailing
   * parenthetical — and this path was not, so it went to IMDb with the banner still
   * attached and could only miss: `ImdbIdMissing(search='Ghost in the Shell | Kino
   * Azji')` → no match, for a film IMDb knows perfectly well. The asymmetry cost those
   * rows an imdbId and, through `tmdb.findByImdbId`, the tmdbId that id would have
   * recovered.
   *
   * Costs nothing on an undecorated title: `searchTitleCandidates` returns the one
   * form, so there is exactly one query, as before. Only a decorated row pays a second
   * lookup, and only when the first missed.
   *
   * Deliberately NOT a fold of the decorated row onto the base film — a cycle
   * screening is its own card by design. It just needs the film's id.
   */
  private def cachedFindId(searchTitle: String, year: Option[Int]): Option[String] =
    SearchTitles.candidates(searchTitle, originalTitle = None)
      .map(normalizer.apiQuery).filter(_.nonEmpty).distinct
      .iterator
      .flatMap(query => imdbIdCache.getOrResolve(ResolutionKeys.imdb(query, year, normalizer))(
        imdb.findId(query, year)))
      .nextOption()

  /** The year-anchored cached search, then — when the year-anchored search returns nothing (e.g. IMDb hasn't set a
   *  release year yet for a fresh film) — confirming an exact-deburr-title candidate via director. Not routed through
   *  the cache — the basic path already cached a miss; this live fallback only fires when directors are known and can
   *  disambiguate. */
  def rungs(query: ImdbIdQuery): Seq[() => Option[String]] =
    query.years.map(y => () => cachedFindId(query.searchTitle, y)) ++
      (if (query.directors.nonEmpty) query.years.map(y => () => imdb.findId(query.searchTitle, y, query.directors)) else Nil)

  /** The film's other venue titles, last of all the ladder's rungs: a take that arrived late, TMDB down meanwhile,
   *  searched as the film's other listings call it, and IMDb finds PL "Samson i Dalila" (2026) only as "Opera-samson i
   *  dalila" (Main #2617). After every rung, so a film the search title finds asks nothing more. */
  def otherTitleRungs(query: ImdbIdQuery): Seq[() => Option[String]] =
    query.otherTitles.flatMap(title => query.years.map(y => () => cachedFindId(title, y)))
}
