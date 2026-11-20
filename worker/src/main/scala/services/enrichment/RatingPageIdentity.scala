package services.enrichment

import models.MovieRecord
import services.evidence.CinemaSlotReadings.evidence
import services.evidence.LatestTitleYear

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
    val ours = directorsOf(row, tmdbDirectors) ++ services.evidence.TmdbLessEligibility.directorsOf(row)
    ours.nonEmpty && pageDirectors.nonEmpty && MetacriticClient.directorsCompatible(ours, pageDirectors)
  }

  /** Does a page crediting `pageDirectors` positively deny the row's film? */
  def directorDenies(row: MovieRecord, pageDirectors: Set[String], tmdbDirectors: Int => Set[String]): Boolean = {
    val slot = slotDirectors(row)
    pageDirectors.nonEmpty && slot.nonEmpty &&
      !MetacriticClient.directorsCompatible(slot, pageDirectors) &&
      !MetacriticClient.directorsCompatible(slot ++ row.tmdbId.toSeq.flatMap(tmdbDirectors), pageDirectors)
  }

  /** Is a page dated `pageYear`, crediting `pageDirectors`, another film's BY ITS YEAR -- the row's own year
   *  (`rowYear`) positively contradicts it and the credits do not agree? A year decades apart with the director agreeing
   *  is a retrospective's screening year (see [[directorsAgree]]); an undated page is no evidence. Rotten Tomatoes reads
   *  the page's year, Filmweb its URL's. */
  def otherFilmsYear(rowYear: Option[Int], row: MovieRecord, pageYear: Option[Int], pageDirectors: Set[String],
                     tmdbDirectors: Int => Set[String])(using LatestTitleYear): Boolean =
    !MetacriticClient.yearsCompatible(rowYear, pageYear) && !directorsAgree(row, pageDirectors, tmdbDirectors)

  /** Every director the film is credited with: TMDB's full crew and each cinema's reported director -- what a site
   *  searched one candidate at a time (Filmweb) disambiguates same-titled films across years by. */
  def creditedDirectors(row: MovieRecord, tmdbDirectors: Int => Set[String])(using LatestTitleYear): Set[String] =
    row.tmdbId.map(tmdbDirectors).getOrElse(Set.empty) ++ row.evidence.directors.toSet

  /** TMDB's synopsis of the film, held or retained: a same-year, same-title tie a site's own plot can break (Filmweb's
   *  Polish `plot` is in the language of TMDB's Polish blurb). */
  def referenceSynopsis(row: MovieRecord): Option[String] =
    row.data.get(models.Tmdb).flatMap(_.synopsis).orElse(row.retainedSynopses.get(models.Tmdb))

  private def slotDirectors(row: MovieRecord): Set[String] =
    row.data.get(models.Tmdb).toSeq.flatMap(_.director).toSet
}
