package services.identity

import services.movies.{FormatTags, TitleContainment, TitleWords}

/** A title's closing VERSION tag — "– Extended", "(ext.)", "EXTENDED VERSION", ": Director's Cut", "(Kinofassung)",
 *  "wersja reżyserska", "versión extendida" ([[TitleWords.Concept.VersionTag]]) — names a cut of the film the rest of the
 *  title names, not another work: TMDB keeps one record for a film and its cuts. The title relations read a billing's
 *  titles and original title without it too ([[versionless]]), so a version-tagged spelling of a film's own title names
 *  that title whole instead of measuring a `segment` or an `overlap` of it — what the learned listing-film cut read as
 *  another film: DE CineStar's "Herr der Ringe - Rückkehr des Königs (ext.)", originally "The Lord of the Rings: The
 *  Return of the King – Extended", was kept from Jackson's film by `originalTitle=segment`, and its `denies-film`
 *  edges vetoed every other venue's Extended billing with it. */
object VersionTags {

  /** The tag's phrases, then format words beside them ("(ext.) (OV)", "Remastered 4K"), over folded words; a tag holds
   *  one phrase at least: a format alone is the print's ([[services.movies.FormatTags]]), not a version's. */
  private val Format  = (FormatTags.FormatToken.keySet ++ Set("dolby", "engl")).toSeq.sortBy(-_.length).mkString("(?:\\b(?:", "|", ")\\b)")
  private val Phrase  = TitleWords.foldedPattern(TitleWords.Concept.VersionTag)
  private val WholeTag = s"(?:$Format\\s)*$Phrase(?:\\s(?:$Phrase|$Format))*".r

  /** Is `piece` a version tag and nothing else? */
  private[identity] def isTag(piece: String): Boolean = {
    val folded = TitleContainment.tokens(piece).mkString(" ")
    folded.nonEmpty && WholeTag.matches(folded)
  }

  private val Bracketed  = """^(.*?\S)\s*[(\[]([^()\[\]]*)[)\]]\s*$""".r
  private val Delimited  = """^(.*\S)(?:\s+[-–—|]\s*|\s*[|:]\s+|\s*\|\s*)(\S[^|:–—]*?)\s*$""".r

  /** `title` without its closing version tag, if it closes on one and something of the film's title is left — more than
   *  an article ("The Final Cut" is a title, not "The" cut finally). Several tags go ("(ext.) (OV)"), each a bracket, a
   *  delimited piece ("… – Extended", "…: Director's Cut") or the title's last words ("… EXTENDED VERSION"). */
  def versionless(title: String): Option[String] = {
    @annotation.tailrec
    def strip(t: String, stripped: Boolean): Option[String] = closingTag(t) match {
      case Some(rest) => strip(rest, stripped = true)
      case None       => Option.when(stripped)(t.trim)
    }
    strip(title.trim, stripped = false).filter(names)
  }

  /** "Pink Floyd: The Final Cut" closes on a title, not a tag after "The". */
  private def endsOnArticle(word: String): Boolean = TitleContainment.tokens(word).lastOption.exists(IdentityMeasures.LeadingArticles)

  private def names(rest: String): Boolean = {
    val words = TitleContainment.tokens(rest)
    words.nonEmpty && !(words.sizeIs == 1 && IdentityMeasures.LeadingArticles(words.head))
  }

  /** The title before its one closing tag, if it closes on one. */
  private def closingTag(t: String): Option[String] =
    t match {
      case Bracketed(rest, inner) if isTag(inner) => Some(rest)
      case Delimited(rest, piece) if isTag(piece) => Some(rest)
      case _ =>
        // the title's last words, undelimited: the longest closing run that is a whole tag, a word of the title left
        val words = t.split("\\s+").toSeq
        (1 until words.size).iterator.map(k => (words.take(k), words.drop(k)))
          .collectFirst { case (rest, tag) if isTag(tag.mkString(" ")) && names(rest.mkString(" ")) && !endsOnArticle(rest.last) => rest.mkString(" ") }
    }
}
