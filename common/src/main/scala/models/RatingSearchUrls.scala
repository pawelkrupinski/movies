package models

/** Each rating site's SEARCH page for a title: the click-through when no direct page is known,
 *  and what a card whose identity is in doubt links to instead of a page that may be another
 *  film's (`services.readmodel.RatingGate`). */
object RatingSearchUrls {
  private def q(title: String) = java.net.URLEncoder.encode(title, "UTF-8")
  def metacritic(title: String): String     = s"https://www.metacritic.com/search/${q(title)}/?category=2"
  def rottenTomatoes(title: String): String = s"https://www.rottentomatoes.com/search?search=${q(title)}"
  def filmweb(title: String): String        = s"https://www.filmweb.pl/search?query=${q(title)}"
}
