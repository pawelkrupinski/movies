package services.identity.vocabulary

import scala.util.hashing.MurmurHash3

/**
 * The people a venue's OWN text names — its synopsis and its cast field — as [[CastEvidence]] reads them: each pair of
 * neighbouring words of a run of capitalised words ("Marcin Dorociński", "Bolesława Prusa", a hyphened part in lower
 * case among them: "Na Hong-jin", "Bong Joon-ho"), folded (accents off, `ł` as
 * `l`, lower case) and hashed; in a cast field, whose entries are names whatever their case, every pair of an entry's
 * neighbouring words. A comma, bracket, quote or line keeps two runs apart, a space, hyphen, apostrophe or full stop
 * does not ("Richard E. Grant", "Glynn-Carney"). Held as the sorted hashes, never the text: a corpus holds one per
 * listing, and a synopsis names a handful of people among hundreds of words.
 */
final class VenueNames private (pairs: Array[Int]) extends SortedHashes[VenueNames](pairs) {
  protected def holding(sorted: Array[Int]): VenueNames = if (sorted.isEmpty) VenueNames.None else new VenueNames(sorted)
  protected def unit: String = "pairs"

  /** Does the text name `name` WHOLE: two words at least, every pair of its neighbouring words held — never a surname
   *  alone, nor a first name. */
  def names(name: String): Boolean = holdsAll(VenueNames.pairsOf(name))
}

object VenueNames {
  val None: VenueNames = new VenueNames(Array.emptyIntArray)

  /** The names `texts` (a synopsis, a description) and `cast` (a cast field's entries) hold. */
  def of(texts: Iterable[String], cast: Iterable[String] = Nil): VenueNames = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    texts.foreach(text => collect(text, capitalisedOnly = true, found))
    cast.foreach(name => collect(name, capitalisedOnly = false, found))
    val all = found.result()
    if (all.isEmpty) None else new VenueNames(SortedHashes.sortedDistinct(all, all.length))
  }

  /** Names as [[VenueNames.hashes]] gave them. */
  def ofHashes(hashes: Seq[Int]): VenueNames = if (hashes.isEmpty) None else new VenueNames(SortedHashes.of(hashes))

  /** The hashed pairs of `name`'s neighbouring words, case aside: a name of one word has none. */
  private[identity] def pairsOf(name: String): Array[Int] = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    collect(name, capitalisedOnly = false, found)
    found.result()
  }

  /** A letter folded as `TextNormalization.deburr` and lower case fold it — accents off, `ł` as `l` — looked up for the
   *  Latin letters, which is every name a venue in these countries prints; any other only lower-cased. */
  private[identity] def fold(c: Char): Char = if (c < Folded.length) Folded(c) else Character.toLowerCase(c)
  private val Folded: Array[Char] = Array.tabulate(0x0250) { code =>
    val folded = tools.TextNormalization.deburr(code.toChar.toString).toLowerCase(java.util.Locale.ROOT)
    if (folded.length == 1) folded.charAt(0) else Character.toLowerCase(code.toChar)
  }

  private def pairHash(a: Int, b: Int): Int = MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(0x4e616d65, a), b), 2)

  /** Characters a run of words goes on over: a name's spaces, hyphens, apostrophes and an initial's full stop. */
  private def joins(c: Char): Boolean = Character.isWhitespace(c) && c != '\n' && c != '\r' || c == '-' || c == '\'' || c == '’' || c == '.'

  /** Every pair of neighbouring words of each run of `text`, by hash — of capitalised words only when `capitalisedOnly`. */
  private def collect(text: String, capitalisedOnly: Boolean, into: scala.collection.mutable.ArrayBuilder[Int]): Unit =
    new Runs(capitalisedOnly, into).read(text)

  /** The end of the word of letters and digits starting at `from` in `text`. */
  private[identity] def wordEnd(text: String, from: Int): Int = {
    var end = from
    while (end < text.length && Character.isLetterOrDigit(text.charAt(end))) end += 1
    end
  }

  /** The folded word `text(from until to)`'s hash, as its String's would be, built char by char: no word is copied. */
  private[identity] def foldedHash(text: String, from: Int, to: Int): Int = {
    var h = 0
    var i = from
    while (i < to) { h = 31 * h + fold(text.charAt(i)); i += 1 }
    h
  }

  /** A walk along a text's runs of words ([[collect]]), the pairs of neighbouring words put `into` as it goes. */
  private final class Runs(capitalisedOnly: Boolean, into: scala.collection.mutable.ArrayBuilder[Int]) {
    private var previous = 0          // the previous word's hash, while it stands beside the next in one run
    private var inRun    = false
    private var hyphened = false      // the run's last word ended at a hyphen: a name's lower-case part follows it ("Bong Joon-ho")

    def read(text: String): Unit = {
      var i = 0
      while (i < text.length) {
        val c = text.charAt(i)
        if (Character.isLetterOrDigit(c)) i = word(text, i)
        else { between(c); i += 1 }
      }
    }

    /** Reads the word starting at `from`, returning its end. */
    private def word(text: String, from: Int): Int = {
      val end = wordEnd(text, from)
      // prose: skipped unread, and it ends the run
      if (capitalisedOnly && !Character.isUpperCase(text.charAt(from)) && !(inRun && hyphened)) inRun = false
      else {
        val h = foldedHash(text, from, end)
        if (inRun) into += pairHash(previous, h)
        previous = h; inRun = true
      }
      end
    }

    private def between(c: Char): Unit = {
      if (!joins(c)) inRun = false
      hyphened = c == '-'
    }
  }
}
