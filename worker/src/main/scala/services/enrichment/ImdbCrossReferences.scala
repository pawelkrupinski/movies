package services.enrichment

/** The other databases' rungs of the id ladder ([[ImdbIdLadder]]), each cross-referencing the film to its IMDb id. Each
 *  is optional: None disables it (the specs that do not wire it; OMDb with `OMDB_API_KEY` unset). */
final class ImdbCrossReferences(
  // Wikidata cross-reference fallback: when both the IMDb suggestion endpoint and
  // the director-based path return nothing, try Filmweb-ID → Wikidata P5032 →
  // IMDb P345. Covers classic/repertoire films whose titles differ too much
  // between the cinema listing and IMDb for the suggestion endpoint to match.
  wikidata: Option[WikidataClient],
  // When the row already has a tmdbId, Letterboxd's film page echoes the imdbId.
  letterboxdIdResolver: Option[LetterboxdIdResolver],
  // OMDb id backstop — its English DB carries much of the niche/foreign long tail
  // (Indian, Malayalam, festival titles) that IMDb's suggestion endpoint and
  // Letterboxd miss. `findImdbId` is title+year+director corroborated, so a fuzzy hit
  // can't bind an unrelated film.
  omdb: Option[OMDbClient],
  // Cinemeta (Stremio catalogue) — the final rung. IMDb-keyed, indexes a broad
  // foreign/regional long tail; corroborated by title+year. Free, no key.
  cinemeta: Option[CinemetaClient]
) {

  def rungs(query: ImdbIdQuery): Seq[() => Option[String]] = {
    val titles = query.crossReferenceTitles
    Seq(
      // Wikidata fallback: ONE claims call cross-references via the Filmweb
      // entity id (P5032) and yields every film-database id at once. Only fires
      // when the filmwebUrl is a real entity page (not a search redirect) and a
      // WikidataClient has been wired.
      // Only the imdbId is taken. The claims also name RT and Metacritic pages, but those
      // links belong to their rating tasks (`RottenTomatoesRatings`, `MetascoreRatings`):
      // written here they raced a task resolving the same row, and whichever read the row
      // first decided what was fetched — Wikidata's stale "m/1016356-pippi_longstocking"
      // (a 404) in some PL convergence runs and not others.
      () => for {
        client    <- wikidata
        url       <- query.filmwebUrl
        filmwebId <- WikidataClient.filmwebEntityId(url)
        ids       <- client.findIdsByFilmwebId(filmwebId)
        imdbId    <- ids.imdbId
      } yield imdbId,
      // Letterboxd backstop — when the row already has a tmdbId, its Letterboxd
      // film page echoes the imdbId (echo-checked against the queried tmdbId).
      () => for {
        resolver <- letterboxdIdResolver
        tmdbId   <- query.tmdbId
        imdbId   <- resolver.resolveImdbId(tmdbId)
      } yield imdbId,
      // OMDb backstop — the English DB that covers much of the long tail TMDB's
      // IMDb cross-references miss (Indian/Malayalam/festival titles). title+year+director
      // corroborated (see OMDbClient) so a fuzzy hit can't bind a wrong film.
      // This is the id the once-daily OmdbBackfill sweep would have supplied
      // hours later; running it inline lands it now. A lookup OMDb could not answer
      // (down, quota spent) falls through to the next rung; nothing is recorded for it.
      // A new film's ask: OMDb's budget keeps the day's last 10% for these alone (OmdbBudget).
      () => omdb.flatMap(_.findImdbId(titles, query.year, query.directors, OmdbAsk.NewFilm)),
      // Wikidata DIRECT-title — distinct from the Filmweb-id path above: for a
      // film with no Filmweb entity page, search Wikidata's film items
      // by title and bind the first whose label + P577 year corroborate. Catches
      // films with a Wikidata entry (hence RT/MC/Letterboxd slugs too) that the
      // English-DB resolvers miss.
      () => wikidata.flatMap(_.findImdbIdByTitle(query.searchTitle, query.year)),
      // Cinemeta (Stremio) — final rung. IMDb-keyed catalogue covering a broad
      // foreign/regional long tail; corroborated by title+year so a fuzzy hit
      // can't bind a wrong film. Free, no API key.
      () => cinemeta.flatMap(_.findImdbId(titles, query.year))
    )
  }
}
