package services.titles

/**
 * The two forms a title is compared by: its KEY ([[sanitize]], what two spellings of one title share) and its SEARCH
 * form ([[searchQuery]], the title as an external search asks for it). All the identity resolver reads of a country's
 * title rules: it compares listings' titles, their segments and the films' titles by these two forms alone, so it is
 * handed this, never the whole [[TitleNormalizer]] that implements it.
 */
trait TitleForms {
  /** The key form of `title`: case, accents, punctuation and spacing folded, so its spellings share it. */
  def sanitize(title: String): String
  /** `title` as an external search asks for it: the venue's decorations and format tags off, cased for the search. */
  def searchQuery(title: String): String
}
