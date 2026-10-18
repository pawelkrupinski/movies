package services.identity

/**
 * The words a text CAPITALISES — the people, places and names a venue's synopsis and a film's description share whatever
 * language each is in — each as its first [[ProseWords.Stem]] letters, folded as [[VenueNames]] folds a name (accents
 * off, `ł` as `l`, lower case) and hashed: inflection and spelling keep the stem, so Polish "Belgii" is English
 * "Belgium", "Eliasa" is "Elias". A word shorter than [[ProseWords.Shortest]] letters says nothing ("I", "Mr"). Held as
 * the sorted hashes, never the text: a corpus holds one per listing and per film read, and a description capitalises a
 * handful of its words.
 */
final class ProseWords private (stems: Array[Int]) extends SortedHashes[ProseWords](stems) {
  protected def holding(sorted: Array[Int]): ProseWords = ProseWords.held(sorted)
  protected def unit: String = "words"
}

object ProseWords {
  val None: ProseWords = new ProseWords(Array.emptyIntArray)

  /** The letters of a word its stem keeps. */
  val Stem = 4
  /** The fewest letters a word read has. */
  val Shortest = 3

  /** The words a venue's prose capitalises, a sentence's first word left out: capitalised whatever it is ("When", "Gdy",
   *  "Die"), it names nothing. */
  def ofProse(texts: Iterable[String]): ProseWords = of(texts, capitalisedOnly = true, sentenceFirst = false)

  /** The words a film's description capitalises, a sentence's first word too: what a venue's synopsis is compared WITH
   *  reads as much of it as there is. */
  def ofDescription(texts: Iterable[String]): ProseWords = of(texts, capitalisedOnly = true, sentenceFirst = true)

  /** Every word of `texts`, whatever its case: a title's, a person's name. */
  def ofWords(texts: Iterable[String]): ProseWords = of(texts, capitalisedOnly = false, sentenceFirst = true)

  /** Words as [[ProseWords.hashes]] gave them. */
  def ofHashes(hashes: Seq[Int]): ProseWords = held(SortedHashes.of(hashes))

  private def held(stems: Array[Int]): ProseWords = if (stems.isEmpty) None else new ProseWords(stems)

  /** A character ending a sentence: the next word is its first. */
  private def endsASentence(c: Char): Boolean = c == '.' || c == '!' || c == '?' || c == ':' || c == ';' || c == '\n' || c == '\r'

  private def of(texts: Iterable[String], capitalisedOnly: Boolean, sentenceFirst: Boolean): ProseWords = {
    var found = new Array[Int](16)
    var count = 0
    texts.foreach { text =>
      var first = true   // the next word opens a sentence
      var i     = 0
      val n     = text.length
      while (i < n) {
        val c = text.charAt(i)
        if (Character.isLetter(c)) {
          var j = i
          while (j < n && Character.isLetter(text.charAt(j))) j += 1
          if (j - i >= Shortest && (!capitalisedOnly || Character.isUpperCase(c)) && (sentenceFirst || !first)) {
            // the folded stem's hash, as its String's would be, built char by char: no word is copied
            var h = 0
            var k = i
            while (k < i + Stem && k < j) { h = 31 * h + VenueNames.fold(text.charAt(k)); k += 1 }
            if (count == found.length) found = java.util.Arrays.copyOf(found, count * 2)
            found(count) = h; count += 1
          }
          first = false
          i = j
        } else {
          if (endsASentence(c)) first = true
          i += 1
        }
      }
    }
    held(SortedHashes.sortedDistinct(found, count))
  }
}
