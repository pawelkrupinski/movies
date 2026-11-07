package tools

/** Is a text written in the Latin script: what a Latin-script search can find, or a name be compared with another by
 *  (TMDB credits "王家衛" where a Polish cinema prints "Wong Kar Wai"). One reading of the script for every caller. */
object LatinScript {
  /** Is `c` a character of the Latin script? */
  def isLatin(c: Char): Boolean = Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN

  /** Does `text` hold a letter in the Latin script? */
  def hasLetter(text: String): Boolean = text.exists(c => Character.isLetter(c) && isLatin(c))

  /** Is every letter of `text` a Latin one — true of a text with no letter at all? */
  def lettersOnly(text: String): Boolean = text.forall(c => !Character.isLetter(c) || isLatin(c))
}
