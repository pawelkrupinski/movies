package services.enrichment

import java.util.Locale

import models.MovieRecord
import services.movies.{CacheKey, EmbeddedYear, TitleNormalizer}
import services.resolution.SearchTitles

/**
 * Rating-site links for a film TMDB has no record of — asked only once TMDB was asked and found
 * nothing (`MovieRecord.tmdbNoMatch`), and only for a row that publishes a year or a director to
 * check a page against.
 *
 * Measured 2026-09-30 on the 174 TMDB-less cards live in production: every correct Metacritic, RT
 * and Filmweb page found belonged to a row publishing a year or a director, and every wrong one
 * ("A Festival 2026 | Zmiana pierwszeństwa" → Roger Michell's 2002 "Changing Lanes") to a row
 * publishing neither. So a page is taken only when it POSITIVELY agrees ([[corroborated]]) — never
 * because it is merely silent, which is all a TMDB-matched row's page must be.
 */
object TmdbLessRatingLinks {

  /** Is `row`, which TMDB could not match, queued for a rating site: TMDB was asked and found
   *  nothing, and the cinemas publish a director or a year (a field's, or one the title dates, "Lawa (1989)")? */
  def eligible(row: MovieRecord): Boolean =
    row.tmdbNoMatch && !row.evidence.titles.exists(notAFilm) &&
      (directorsOf(row).nonEmpty || row.evidence.years.nonEmpty || EmbeddedYear.ofAll(row.evidence.titles).isDefined)

  /** A title that is no single film, whatever its facts: a festival pass, a fan event, a double bill, a
   *  marathon or a secret screening. A rating site has no page for it — "12. SPLAT! FilmFest | karnet"
   *  and "Avengers: Doomsday RealD 3D Fan Event" were searched on Metacritic and RT for nothing. Not
   *  `NonMovieEventClassifier`: a venue-scoped listing filter, whose words ("balet", "na żywo", "koncert")
   *  a FILM's title carries too ("Neneh: Gwiazda baletu", a silent film shown with live music). */
  def notAFilm(title: String): Boolean = NotAFilm.findFirstIn(title).isDefined

  /** A spaced "+" joins a second FILM unless what follows it is an add-on to one film: "Wśród nocnej
   *  ciszy + dyskusja", "… + spotkanie z reżyserem", "… + Q&A". */
  private val NotAFilm =
    """(?iu)\b(?:karnet\w*|festival pass|film pass|fan (?:event|screening)|double bill|triple (?:bill|feature)|marat(?:h)?on\w*|secret screening|seans\s+niespodzian\w*)\b|\s\+\s(?!(?:dyskusj|spotkani|prelekcj|rozmow|wykład|debat|panel|warsztat|konkurs|quiz|q\s*&\s*a|intro|discussion|talk|meet))""".r

  /** Can a page be checked against what the cinemas publish of this TMDB-less row: a director, or a year? */
  def checkable(key: CacheKey, row: MovieRecord): Boolean = directorsOf(row).nonEmpty || yearOf(key, row).isDefined

  /** The film's year as its cinemas publish it: the key's, a year the title itself dates
   *  ("Tabu (1987)"), else one a slot published. */
  def yearOf(key: CacheKey, row: MovieRecord): Option[Int] =
    key.year.orElse(EmbeddedYear.ofAll(key.cleanTitle +: row.evidence.titles.toSeq)).orElse(row.evidence.years.headOption)

  /** Every director the cinemas credit, a comma-packed crew split. */
  def directorsOf(row: MovieRecord): Set[String] =
    row.evidence.directors.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSet

  /** The titles a site is searched under, STRIPPED: the cinemas' original titles first (the
   *  international title the sites index), then the cinema title without its programme decoration
   *  ("Kino bez barier: …", "… | Kino dyskomfortu") and each piece a banner splits off it. */
  def titlesOf(key: CacheKey, row: MovieRecord, normalizer: TitleNormalizer): Seq[String] = {
    val stripped = normalizer.searchQuery(key.cleanTitle)
    (row.evidence.originalTitles ++ Seq(stripped) ++ SearchTitles.candidates(stripped, None).map(normalizer.searchQuery))
      .map(_.trim).filter(_.nonEmpty).distinctBy(_.toLowerCase(Locale.ROOT))
  }

  /** The year a site is searched with: none when the cinemas credit a director, whose agreement
   *  then decides — a retrospective's row carries its SCREENING year ("NADZY", Leigh's 1993 film
   *  shown in 2026), which would turn the right page away. */
  def searchYear(key: CacheKey, row: MovieRecord): Option[Int] =
    if (directorsOf(row).nonEmpty) None else yearOf(key, row)

  /** Does a page crediting `pageDirectors`, dated `pageYear`, name this row's film? Its director
   *  must agree with the cinemas', or — the page crediting none — its year must equal theirs. A
   *  page contradicting the director is never it, and a page saying nothing is not evidence. */
  def corroborated(key: CacheKey, row: MovieRecord, pageYear: Option[Int], pageDirectors: Set[String]): Boolean = {
    val ours     = directorsOf(row)
    val credited = ours.nonEmpty && pageDirectors.nonEmpty
    if (credited) MetacriticClient.directorsCompatible(ours, pageDirectors)
    else yearOf(key, row).exists(year => pageYear.contains(year))
  }
}
