package models

/** Each rating site's SEARCH page for a title: the click-through when no direct page is known,
 *  and what a card whose identity is in doubt links to instead of a page that may be another
 *  film's (`services.readmodel.RatingGate`). */
object RatingSearchUrls {
  private def q(title: String) = java.net.URLEncoder.encode(title, "UTF-8")
  def metacritic(title: String): String     = s"https://www.metacritic.com/search/${q(title)}/?category=2"
  def rottenTomatoes(title: String): String = s"https://www.rottentomatoes.com/search?search=${q(title)}"
  def filmweb(title: String): String        = s"https://www.filmweb.pl/search?query=${q(title)}"

  /** Whether every letter of `title` is Latin script (digits, punctuation and spaces don't count;
   *  a title with no letters, "1917", passes). */
  def isLatinScript(title: String): Boolean =
    title.codePoints().allMatch(cp => !Character.isLetter(cp) || Character.UnicodeScript.of(cp) == Character.UnicodeScript.LATIN)

  /** The title to search the English-language sites (Metacritic, Rotten Tomatoes, IMDb) by:
   *  `original` when it is Latin script, else the first Latin-script `alternative` (best first:
   *  an English title, a romanisation, the shown title), else `original` after all. Those sites
   *  index the English title; a Cyrillic or CJK original ("Кіллхаус") finds nothing there. */
  def englishSiteTitle(original: String, alternatives: Seq[String]): String =
    if (isLatinScript(original)) original
    else alternatives.map(_.trim).find(t => t.nonEmpty && isLatinScript(t)).getOrElse(original)
}
