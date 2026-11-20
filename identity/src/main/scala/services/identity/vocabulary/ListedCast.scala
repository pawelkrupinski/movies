package services.identity.vocabulary

import scala.util.hashing.MurmurHash3

/**
 * A venue's own cast FIELD — never its synopsis — as the cast MISMATCH reads it: each entry's
 * neighbouring words as UNORDERED pairs, folded as [[VenueNames]] folds them, so a name given family name first ("Gong
 * Li", "Li Gong") names the same person. Held only where the field is evidence about the film at all: two entries of two
 * words or more, and no entry of one word — a mononymous star ("Rajinikanth"), a puppet ("Animal") or a role in brackets
 * ("Paul Marque (Romeo)") no pair can name. A part between commas, semicolons, slashes or brackets is an entry.
 */
final class ListedCast private (pairs: Array[Int]) extends SortedHashes[ListedCast](pairs) {
  protected def holding(sorted: Array[Int]): ListedCast = if (sorted.isEmpty) ListedCast.None else new ListedCast(sorted)
  protected def unit: String = "pairs"

  /** Does the field name a name given as its unordered pairs ([[FilmCast]]'s)? */
  def namesPairs(pairs: Array[Int]): Boolean = holdsAll(pairs)
}

object ListedCast {
  val None: ListedCast = new ListedCast(Array.emptyIntArray)
  /** The field as its [[SortedHashes.hashes]] stored it ([[ListingBson]]). */
  def ofHashes(hashes: Seq[Int]): ListedCast = if (hashes.isEmpty) None else new ListedCast(SortedHashes.of(hashes))
  /** The names a field must list for its silence about a film's cast to count. */
  val Names = 2

  /** The field `cast` lists, where it is evidence ([[ListedCast]]); none otherwise. Read char by char, as [[VenueNames]]
   *  is: a corpus reads one per listing. */
  def of(cast: Seq[String]): ListedCast = {
    val field = new Field
    val it = cast.iterator
    while (it.hasNext && !field.oneWord) field.entries(it.next())
    if (field.oneWord || field.names < Names) None else new ListedCast(SortedHashes.sortedDistinct(field.found, field.count))
  }
  private val EntryEnds = ",;/|()[]\r\n"

  /** A walk along a cast field's texts: the unordered pairs of each entry's neighbouring words (`found`, `count` of them),
   *  how many entries name two words or more (`names`), and whether one names a single word (`oneWord`, which ends it). */
  private final class Field {
    var found: Array[Int] = new Array[Int](16)
    var count   = 0
    var names   = 0
    var oneWord = false
    private var words    = 0
    private var previous = 0

    /** Reads one text of the field, its end ending its last entry. */
    def entries(text: String): Unit = {
      words = 0; previous = 0
      var i = 0
      val n = text.length
      while (i <= n && !oneWord) i = step(text, i, n)
    }

    /** Reads what starts at `i` -- a word, an entry's end or a separator -- and returns where the next read starts. */
    private def step(text: String, i: Int, n: Int): Int = {
      val c = if (i < n) text.charAt(i) else '\n'
      if (Character.isLetterOrDigit(c)) word(text, i)
      else {
        if (EntryEnds.indexOf(c) >= 0) entryEnds()
        i + 1
      }
    }

    private def word(text: String, from: Int): Int = {
      val end = VenueNames.wordEnd(text, from)
      val h   = VenueNames.foldedHash(text, from, end)
      if (words > 0) add(pair(previous, h))
      previous = h; words += 1
      end
    }

    private def add(hash: Int): Unit = {
      if (count == found.length) found = java.util.Arrays.copyOf(found, count * 2)
      found(count) = hash; count += 1
    }

    private def entryEnds(): Unit = {
      if (words == 1) oneWord = true else if (words >= 2) names += 1
      words = 0
    }
  }

  /** The folded words of `name`, each as its hash. */
  private[identity] def wordsOf(name: String): Array[Int] = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    var i = 0
    while (i < name.length) {
      if (Character.isLetterOrDigit(name.charAt(i))) {
        val end = VenueNames.wordEnd(name, i)
        found += VenueNames.foldedHash(name, i, end)
        i = end
      } else i += 1
    }
    found.result()
  }

  /** The unordered pairs of neighbouring words, by hash. */
  private[identity] def pairs(words: Array[Int]): Array[Int] = words.indices.drop(1).map(i => pair(words(i - 1), words(i))).toArray

  /** One unordered pair of words, by hash: either order hashes alike. */
  private def pair(a: Int, b: Int): Int =
    MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(0x4c697374, math.min(a, b)), math.max(a, b)), 2)

  /** `name`'s unordered pairs: none for a name of one word. */
  private[identity] def pairsOf(name: String): Array[Int] = pairs(wordsOf(name)).distinct.sorted
}
