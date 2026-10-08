package services.enrichment

import services.movies.LatestTitleYear

import models.MovieRecord

/**
 * Is a STORED rating-site page (Rotten Tomatoes, Metacritic) another film's?
 *
 * Asked on every score refresh, because re-resolution cannot dislodge a wrong url: it
 * writes only when it finds a page, so a url stored before a guard existed — or by a
 * probe the guard could not see through — is re-scored every tick, forever. The page
 * names its film; this reads that name against the row's.
 *
 * Only a POSITIVE contradiction counts, the rule every same-film check here follows: a
 * page crediting nobody, or a row holding no credit, says nothing. A disagreement on the
 * row's TMDB slot (the localised credit only) is confirmed against TMDB's full crew —
 * localised AND native-script spellings — before it condemns the page, so a film whose
 * director a site romanises differently keeps its url.
 */
object RatingPageIdentity {

  /** The directors to probe a rating site with: TMDB's full crew for the row's film, plus
   *  the row's own TMDB-slot credit when the crew lookup came back empty. */
  def directorsOf(row: MovieRecord, tmdbDirectors: Int => Set[String]): Set[String] =
    row.tmdbId.map(tmdbDirectors).filter(_.nonEmpty).getOrElse(slotDirectors(row))

  /** Does a page crediting `pageDirectors` POSITIVELY agree with the row's film — its TMDB crew or the
   *  cinemas' credit? Then a year decades apart is a retrospective's screening year ("Przekleństwa
   *  niewinności" 2026 is Coppola's 1999 film), not another film's, as Filmweb's own pick reads it. */
  def directorsAgree(row: MovieRecord, pageDirectors: Set[String], tmdbDirectors: Int => Set[String])(using LatestTitleYear): Boolean = {
    val ours = directorsOf(row, tmdbDirectors) ++ TmdbLessRatingLinks.directorsOf(row)
    ours.nonEmpty && pageDirectors.nonEmpty && MetacriticClient.directorsCompatible(ours, pageDirectors)
  }

  /** Does a page crediting `pageDirectors` positively deny the row's film? */
  def directorDenies(row: MovieRecord, pageDirectors: Set[String], tmdbDirectors: Int => Set[String]): Boolean = {
    val slot = slotDirectors(row)
    pageDirectors.nonEmpty && slot.nonEmpty &&
      !MetacriticClient.directorsCompatible(slot, pageDirectors) &&
      !MetacriticClient.directorsCompatible(slot ++ row.tmdbId.toSeq.flatMap(tmdbDirectors), pageDirectors)
  }

  private def slotDirectors(row: MovieRecord): Set[String] =
    row.data.get(models.Tmdb).toSeq.flatMap(_.director).toSet
}
