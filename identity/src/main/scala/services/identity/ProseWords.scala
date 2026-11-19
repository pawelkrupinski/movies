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
    val stems = new Stems
    texts.foreach(text => read(text, capitalisedOnly, sentenceFirst, stems))
    held(stems.sortedDistinct)
  }

  /** The stems' hashes as they are found: grown as needed, never boxed. */
  private final class Stems {
    private var found = new Array[Int](16)
    private var count = 0
    def add(hash: Int): Unit = {
      if (count == found.length) found = java.util.Arrays.copyOf(found, count * 2)
      found(count) = hash; count += 1
    }
    def sortedDistinct: Array[Int] = SortedHashes.sortedDistinct(found, count)
  }

  /** Each word of `text` read ([[reads]]) to `stems`, as its stem's hash. */
  private def read(text: String, capitalisedOnly: Boolean, sentenceFirst: Boolean, stems: Stems): Unit = {
    var first = true   // the next word opens a sentence
    var i     = 0
    val n     = text.length
    while (i < n) {
      val c = text.charAt(i)
      if (Character.isLetter(c)) {
        var j = i
        while (j < n && Character.isLetter(text.charAt(j))) j += 1
        if (reads(j - i, c, first, capitalisedOnly, sentenceFirst)) stems.add(stemHash(text, i, j))
        first = false
        i = j
      } else {
        if (endsASentence(c)) first = true
        i += 1
      }
    }
  }

  /** Is a word of `length` letters opening with `initial` read — opening a sentence when `first`? */
  private def reads(length: Int, initial: Char, first: Boolean, capitalisedOnly: Boolean, sentenceFirst: Boolean): Boolean =
    length >= Shortest && (!capitalisedOnly || Character.isUpperCase(initial)) && (sentenceFirst || !first)

  /** The folded stem's hash of the word `text(from until to)`, as its String's would be, built char by char: no word is
   *  copied. */
  private def stemHash(text: String, from: Int, to: Int): Int = {
    var h = 0
    var k = from
    while (k < from + Stem && k < to) { h = 31 * h + VenueNames.fold(text.charAt(k)); k += 1 }
    h
  }
}
