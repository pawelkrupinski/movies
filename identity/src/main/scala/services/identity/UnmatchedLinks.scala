package services.identity

import models.{Filmweb, FilmwebPages, Imdb, MovieRecord}

/** What an unmatched film of a draft ([[FilmRecordDrafts]]) is linked by. */
private[identity] object UnmatchedLinks {
  /** The ids an unmatched film is linked by, from its cluster's decision alone: a film TMDB has no record of carries the
   *  IMDb id of the fallback film the resolver took for it, else none. The TMDB film its listings' evidence leans to though
   *  no rule took it ([[ResolverDecision.Leaning]]) links nothing: no rule took it — "Łowca androidów: Final Cut" leans to
   *  Ridley Scott's 2026 film — and an id an earlier answer or a title search held for it is no input — PL "Lalka" (2026)
   *  held the 1968 film's, and its 6.9. The rating and IMDb's slot go with an id that goes. A no-match reached before
   *  every question was answered links what it names too: what an earlier decision linked is no input.
   *  One standing on Filmweb's or Wikidata's own id (the agreement's, where no record links an IMDb id) links that
   *  record instead: its Filmweb page — replacing a page a title search guessed, with its rating and slot — or its
   *  Wikidata item, which nothing but such a stand gives a film TMDB has no record of. Without a stand, its Filmweb page
   *  is Filmweb's own search's (`TmdbLessRatingLinks`), which keeps or drops it. */
  def heldLinks(cluster: Cluster, base: MovieRecord, anchor: String): MovieRecord = {
    val imdb = cluster.fallback.filter(_.source == "imdb").map(_.id)
    val linked = (if (imdb == base.imdbId) base else base.copy(imdbId = imdb, imdbRating = None, data = base.data - Imdb))
      .copy(wikidataId = cluster.fallback.collect { case ResolverDecision.Fallback("wikidata", id, _, _, _) => id })
    cluster.fallback match {
      case Some(ResolverDecision.Fallback("filmweb", id, _, title, year))
          if !linked.filmwebUrl.flatMap(FilmwebPages.idOf).map(_.toString).contains(id) =>
        linked.copy(filmwebUrl = id.toIntOption.map(FilmwebPages.url(_, "film", title.getOrElse(anchor), year)), filmwebRating = None,
          data = linked.data - Filmweb)
      case _ => linked
    }
  }
}
