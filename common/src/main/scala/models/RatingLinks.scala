package models

/** Where a card's rating badges link, per source — a presentation of a [[MovieRecord]] the
 *  read-model projection writes into `ResolvedRatings` (`ReadModelProjection.ratingsFor`): the
 *  validated stored URL when the record has one, else an on-the-fly search URL (for legacy records
 *  that pre-date URL persistence) keyed off `fallbackTitle`, the display title. */
object RatingLinks {

  def imdb(record: MovieRecord): Option[String] = record.imdbId.map(id => s"https://www.imdb.com/title/$id/")

  def metacritic(record: MovieRecord, fallbackTitle: String): String =
    record.metacriticUrl.getOrElse(RatingSearchUrls.metacritic(englishSiteSearchTitle(record, fallbackTitle)))

  def rottenTomatoes(record: MovieRecord, fallbackTitle: String): String =
    record.rottenTomatoesUrl.getOrElse(RatingSearchUrls.rottenTomatoes(englishSiteSearchTitle(record, fallbackTitle)))

  def filmweb(record: MovieRecord, fallbackTitle: String): String =
    record.filmwebUrl.getOrElse(RatingSearchUrls.filmweb(record.originalTitle.getOrElse(fallbackTitle)))

  /** What the English-language rating sites are searched by: the original title, unless it is
   *  not Latin script, then TMDB's English title, IMDb's or Filmweb's original (usually a
   *  romanisation), or `fallbackTitle` ([[RatingSearchUrls.englishSiteTitle]]). */
  private def englishSiteSearchTitle(record: MovieRecord, fallbackTitle: String): String =
    RatingSearchUrls.englishSiteTitle(
      record.originalTitle.getOrElse(fallbackTitle),
      record.data.get(Tmdb).flatMap(_.englishTitle).toSeq ++
        Seq(Imdb, Filmweb).flatMap(s => record.data.get(s).flatMap(_.originalTitle)) :+ fallbackTitle)
}
