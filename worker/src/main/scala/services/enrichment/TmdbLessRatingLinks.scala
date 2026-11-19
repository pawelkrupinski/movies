package services.enrichment

import java.util.Locale

import models.{MovieRecord, EmbeddedYear}
import services.movies.{CacheKey, LatestTitleYear, TmdbLessEligibility}
import services.titles.TitleNormalizer
import services.titles.SearchTitles

/**
 * Rating-site links for a film TMDB has no record of — asked only once TMDB was asked and found
 * nothing (`MovieRecord.tmdbNoMatch`), and only for a row that publishes a year or a director to
 * check a page against (which rows: [[services.movies.TmdbLessEligibility]]).
 *
 * Measured 2026-09-30 on the 174 TMDB-less cards live in production: every correct Metacritic, RT
 * and Filmweb page found belonged to a row publishing a year or a director, and every wrong one
 * ("A Festival 2026 | Zmiana pierwszeństwa" → Roger Michell's 2002 "Changing Lanes") to a row
 * publishing neither. So a page is taken only when it POSITIVELY agrees ([[corroborated]]) — never
 * because it is merely silent, which is all a TMDB-matched row's page must be.
 */
object TmdbLessRatingLinks {

  /** Can a page be checked against what the cinemas publish of this TMDB-less row: a director, or a year? */
  def checkable(key: CacheKey, row: MovieRecord)(using LatestTitleYear): Boolean = TmdbLessEligibility.directorsOf(row).nonEmpty || yearOf(key, row).isDefined

  /** The film's year as its cinemas publish it: the key's, a year the title itself dates
   *  ("Tabu (1987)"), else one a slot published. */
  def yearOf(key: CacheKey, row: MovieRecord)(using LatestTitleYear): Option[Int] =
    titledYear(key, row).orElse(row.evidence.years.headOption)

  /** The film's year as its key or its titles give it: the key's, else a delimited year the row's own or any cinema's
   *  title brackets ("Konwicki: Lawa (1989)"). */
  def titledYear(key: CacheKey, row: MovieRecord)(using LatestTitleYear): Option[Int] =
    key.year.orElse(EmbeddedYear.ofAll(key.cleanTitle +: row.evidence.titles.toSeq, LatestTitleYear.current))

  /** The titles a site is searched under, STRIPPED: the cinemas' original titles first (the
   *  international title the sites index), then the cinema title without its programme decoration
   *  ("Kino bez barier: …", "… | Kino dyskomfortu") and each piece a banner splits off it. */
  def titlesOf(key: CacheKey, row: MovieRecord, normalizer: TitleNormalizer)(using LatestTitleYear): Seq[String] = {
    val stripped = normalizer.searchQuery(key.cleanTitle)
    (row.evidence.originalTitles ++ Seq(stripped) ++ SearchTitles.candidates(stripped, None).map(normalizer.searchQuery))
      .map(_.trim).filter(_.nonEmpty).distinctBy(_.toLowerCase(Locale.ROOT))
  }

  /** The year a site is searched with: none when the cinemas credit a director, whose agreement
   *  then decides — a retrospective's row carries its SCREENING year ("NADZY", Leigh's 1993 film
   *  shown in 2026), which would turn the right page away. */
  def searchYear(key: CacheKey, row: MovieRecord)(using LatestTitleYear): Option[Int] =
    if (TmdbLessEligibility.directorsOf(row).nonEmpty) None else yearOf(key, row)

  /** Does a page crediting `pageDirectors`, dated `pageYear`, name this row's film? Its director
   *  must agree with the cinemas', or — the page crediting none — its year must equal theirs. A
   *  page contradicting the director is never it, and a page saying nothing is not evidence. */
  def corroborated(key: CacheKey, row: MovieRecord, pageYear: Option[Int], pageDirectors: Set[String])(using LatestTitleYear): Boolean = {
    val ours     = TmdbLessEligibility.directorsOf(row)
    val credited = ours.nonEmpty && pageDirectors.nonEmpty
    if (credited) MetacriticClient.directorsCompatible(ours, pageDirectors)
    else yearOf(key, row).exists(year => pageYear.contains(year))
  }

  /** A rating site's link for a row TMDB never resolved: asked of the site (`ask`) under the row's titles, its
   *  search year and its directors, and kept only as `confirm` makes it — the page it names corroborating the row.
   *  None when the row is not checkable. */
  def discover[A, B](key: CacheKey, row: MovieRecord, normalizer: TitleNormalizer)(ask: (Seq[String], Option[Int], Set[String]) => Option[A])(
      confirm: A => Option[B])(using LatestTitleYear): Option[B] =
    Option.when(checkable(key, row))(()).flatMap(_ =>
      ask(titlesOf(key, row, normalizer), searchYear(key, row), TmdbLessEligibility.directorsOf(row))).flatMap(confirm)

  /** [[discover]] for a site searched one title at a time: each of the row's titles asked of the site in turn (`ask`,
   *  with the row's year and its cinemas' directors), and the first page that corroborates the row taken -- the page's
   *  year and credits read off it by `pageYear` and `pageDirectors`. None when the row is not checkable. */
  def discoverEach[A](key: CacheKey, row: MovieRecord, normalizer: TitleNormalizer)(ask: (String, Option[Int], Set[String]) => Option[A])(
      pageYear: A => Option[Int], pageDirectors: A => Set[String])(using LatestTitleYear): Option[A] =
    Option.when(checkable(key, row))(()).flatMap { _ =>
      val directors = TmdbLessEligibility.directorsOf(row)
      titlesOf(key, row, normalizer).iterator
        .flatMap(title => ask(title, yearOf(key, row), directors))
        .find(page => corroborated(key, row, pageYear(page), pageDirectors(page)))
    }
}
