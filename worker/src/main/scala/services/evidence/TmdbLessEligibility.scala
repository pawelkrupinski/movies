package services.evidence

import models.{MovieRecord, EmbeddedYear}
/**
 * Which rows TMDB could not match a rating site is asked about, read off the row alone: the rule
 * [[services.enrichment.TmdbLessRatingLinks]] searches by, and the one a merge re-kicks those ratings on
 * ([[MergeRetrigger]]), so the cache that runs the merge needs no enrichment code to read it.
 */
object TmdbLessEligibility {

  /** Is `row`, which TMDB could not match, queued for a rating site: TMDB was asked and found
   *  nothing, and the cinemas publish a director or a year (a field's, or one the title dates, "Lawa (1989)")? */
  def eligible(row: MovieRecord)(using LatestTitleYear): Boolean =
    row.tmdbNoMatch && !row.evidence.titles.exists(notAFilm) &&
      (directorsOf(row).nonEmpty || row.evidence.years.nonEmpty || EmbeddedYear.ofAll(row.evidence.titles, LatestTitleYear.current).isDefined)

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

  /** Every director the cinemas credit, a comma-packed crew split. */
  def directorsOf(row: MovieRecord)(using LatestTitleYear): Set[String] =
    row.evidence.directors.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSet
}
