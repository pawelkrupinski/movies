package services.enrichment

import clients.TmdbClient
import models.CacheKey
import models.MovieRecord
import services.evidence.LatestTitleYear
import services.titles.TitleNormalizer

/**
 * A film as every rating site (Metacritic, Rotten Tomatoes, Filmweb) searches for it and judges a page against it: the
 * row, and what TMDB knows of the film -- its details and its full crew, asked only when a site's search or check needs
 * them, never for a row that does not. One per row a site is asked about ([[of]]); the site keeps only what is its own
 * (its search ladder, its page reads, its link cache).
 */
final class RatingSiteFilms(tmdb: TmdbClient, normalizer: TitleNormalizer)(using LatestTitleYear) {
  def of(key: CacheKey, row: MovieRecord): RatingSiteFilms.Film = new RatingSiteFilms.Film(key, row, tmdb, normalizer)
}

object RatingSiteFilms {

  final class Film(val key: CacheKey, val row: MovieRecord, tmdb: TmdbClient, normalizer: TitleNormalizer)(using LatestTitleYear) {

    /** The titles a site is probed under, from the row and TMDB's details of its film (asked here); none for a row TMDB
     *  has not resolved, whose page is found without them. */
    def tmdbTitles: Option[RatingSiteTitles] =
      row.tmdbId.map(tmdbId => RatingSiteTitles.derive(key.cleanTitle, row, tmdb.details(tmdbId), normalizer))

    /** TMDB's English title of the row's film, when TMDB resolved it (its details asked here). */
    def englishTitle: Option[String] = row.tmdbId.flatMap(tmdb.details).flatMap(_.englishTitle)

    /** TMDB's full crew of the row's film: the localised and the native-script spellings; none when TMDB has not
     *  resolved it. */
    def tmdbDirectors: Set[String] = row.tmdbId.fold(Set.empty[String])(tmdb.directorsFor)

    /** The directors to probe a site with ([[RatingPageIdentity.directorsOf]]). */
    def probeDirectors: Set[String] = RatingPageIdentity.directorsOf(row, tmdb.directorsFor)

    /** TMDB's crew and every cinema's reported director ([[RatingPageIdentity.creditedDirectors]]). */
    def creditedDirectors: Set[String] = RatingPageIdentity.creditedDirectors(row, tmdb.directorsFor)

    /** TMDB's synopsis, held or retained ([[RatingPageIdentity.referenceSynopsis]]). */
    def referenceSynopsis: Option[String] = RatingPageIdentity.referenceSynopsis(row)

    /** Does a page crediting `pageDirectors` positively deny the row's film ([[RatingPageIdentity.directorDenies]])? */
    def deniedBy(pageDirectors: Set[String]): Boolean = RatingPageIdentity.directorDenies(row, pageDirectors, tmdb.directorsFor)

    /** Is a page dated `pageYear` another film's by its year ([[RatingPageIdentity.otherFilmsYear]])? */
    def otherFilmsYear(pageYear: Option[Int], pageDirectors: Set[String]): Boolean =
      RatingPageIdentity.otherFilmsYear(key.year, row, pageYear, pageDirectors, tmdb.directorsFor)

    /** The cinema title with its programme decoration stripped ("Kino bez barier: Freak Show (AD)" asks for "Freak
     *  Show"; the key keeps the full form, so this row stays distinct from the regular screening of the same film). */
    def searchQuery: String = normalizer.searchQuery(key.cleanTitle)

    /** The title an IMDb id lookup for the film searches with: TMDB's original title, else the api-query form. */
    def imdbSearchTitle: String = row.originalTitle.getOrElse(normalizer.apiQuery(key.cleanTitle))

    /** The film's year as its key or titles give it ([[TmdbLessRatingLinks.titledYear]]). */
    def titledYear: Option[Int] = TmdbLessRatingLinks.titledYear(key, row)

    /** A film TMDB has no record of, searched across its stripped cinema titles at once ([[TmdbLessRatingLinks.discover]]). */
    def withoutTmdb[A, B](ask: (Seq[String], Option[Int], Set[String]) => Option[A])(confirm: A => Option[B]): Option[B] =
      TmdbLessRatingLinks.discover(key, row, normalizer)(ask)(confirm)

    /** The same, one title at a time ([[TmdbLessRatingLinks.discoverEach]]). */
    def withoutTmdbEach[A](ask: (String, Option[Int], Set[String]) => Option[A])(pageYear: A => Option[Int], pageDirectors: A => Set[String]): Option[A] =
      TmdbLessRatingLinks.discoverEach(key, row, normalizer)(ask)(pageYear, pageDirectors)

    /** Does a page dated `pageYear` crediting `pageDirectors` name this TMDB-less film ([[TmdbLessRatingLinks.corroborated]])? */
    def corroboratedBy(pageYear: Option[Int], pageDirectors: Set[String]): Boolean =
      TmdbLessRatingLinks.corroborated(key, row, pageYear, pageDirectors)
  }
}
