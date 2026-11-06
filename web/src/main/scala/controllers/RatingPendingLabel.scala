package controllers

/** Message keys of a rating-less pill's accessible label ("IMDb, no rating yet"), and the English
 *  wording the admin pages — which carry no translations — pass to `_ratingBadges`. */
object RatingPendingLabel {
  val Imdb: String    = "rating.imdb.pending"
  val Filmweb: String = "rating.filmweb.pending"

  def english(key: String): String = key match {
    case Imdb    => "IMDb, no rating yet"
    case Filmweb => "Filmweb, no rating yet"
    case other   => other
  }
}
