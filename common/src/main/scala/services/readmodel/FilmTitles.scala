package services.readmodel

import models.ResolvedMovie
import services.movies.TitleText

/** The legacy `/{city}/movie?title=…` addressing rule, for the whole corpus at
 *  once.
 *
 *  A title-addressed link carries whatever spelling the page showed when it was
 *  minted, and the display title can have moved under it since — most often a
 *  numeral a cinema wrote in Roman ("Rocky II") that the corpus now shows in
 *  Arabic. So a link matches on `TitleText.normalize`, the numeral fold
 *  the worker keys spellings by, and not on the string.
 *
 *  Memoised per read-model version by `WebReadModel.filmTitles`, the way
 *  [[FilmSlugs]] is: the lookup used to fold every movie in the corpus on every
 *  miss instead, which made an old shared link the most expensive request in
 *  the app.
 */
final class FilmTitles private(private val idsByKey: Map[String, Seq[String]]) {

  /** Every film whose display title folds to the same key as `title`, newest
   *  first — the order [[FilmSlugs]] hands out addresses in, so a same-title
   *  pair resolves to the film that holds the bare slug. Empty for a title the
   *  corpus does not know. */
  def idsFor(title: String): Seq[String] = idsByKey.getOrElse(FilmTitles.key(title), Nil)
}

object FilmTitles {

  def key(title: String): String = TitleText.normalize(title)

  def apply(movies: Seq[ResolvedMovie]): FilmTitles =
    new FilmTitles(movies.sortBy(FilmSlugs.newestFirst).groupMap(m => key(m.title))(_._id))
}
