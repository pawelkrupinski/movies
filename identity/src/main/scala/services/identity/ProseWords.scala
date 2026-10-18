package services.identity

/**
 * The words a text CAPITALISES — the people, places and names a venue's synopsis and a film's description share whatever
 * language each is in — each as its first [[ProseWords.Stem]] letters, folded as [[VenueNames]] folds a name (accents
 * off, `ł` as `l`, lower case) and hashed: inflection and spelling keep the stem, so Polish "Belgii" is English
 * "Belgium", "Eliasa" is "Elias". A word shorter than [[ProseWords.Shortest]] letters says nothing ("I", "Mr"). Held as
 * the sorted hashes, never the text: a corpus holds one per listing and per film read, and a description capitalises a
 * handful of its words.
 */
final class ProseWords private (private val stems: Array[Int]) {
  def isEmpty: Boolean  = stems.length == 0
  def nonEmpty: Boolean = !isEmpty
  /** How many words it holds. */
  def size: Int = stems.length
  /** Its hashes, as a recorded fixture or a stored record keeps them ([[ProseWords.ofHashes]]). */
  def hashes: Seq[Int] = stems.toSeq

  def ++(other: ProseWords): ProseWords =
    if (other.isEmpty) this else if (isEmpty) other else new ProseWords((stems ++ other.stems).distinct.sorted)
  /** The words it holds that `other` does not. */
  def --(other: ProseWords): ProseWords =
    if (isEmpty || other.isEmpty) this else ProseWords.held(stems.filter(stem => java.util.Arrays.binarySearch(other.stems, stem) < 0))
  /** The words both hold. */
  def &(other: ProseWords): ProseWords =
    if (isEmpty || other.isEmpty) ProseWords.None else ProseWords.held(stems.filter(stem => java.util.Arrays.binarySearch(other.stems, stem) >= 0))

  override def equals(other: Any): Boolean = other match {
    case that: ProseWords => java.util.Arrays.equals(stems, that.stems)
    case _                => false
  }
  override def hashCode: Int = java.util.Arrays.hashCode(stems)
  override def toString: String = if (isEmpty) "ProseWords()" else f"ProseWords(${stems.length} words #$hashCode%08x)"
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
  def ofHashes(hashes: Seq[Int]): ProseWords = if (hashes.isEmpty) None else new ProseWords(hashes.toArray.distinct.sorted)

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
    if (count == 0) None else new ProseWords(sortedDistinct(found, count))
  }

  /** The first `count` of `stems`, sorted, each once — in place, no boxing: read at intake off every listing's synopsis. */
  private def sortedDistinct(stems: Array[Int], count: Int): Array[Int] = {
    java.util.Arrays.sort(stems, 0, count)
    var kept = 1
    var i    = 1
    while (i < count) { if (stems(i) != stems(kept - 1)) { stems(kept) = stems(i); kept += 1 }; i += 1 }
    java.util.Arrays.copyOf(stems, kept)
  }
}
