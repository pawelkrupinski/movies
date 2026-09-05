package services.resolution

/**
 * Evidence that two spellings of a title name the SAME film, for the resolvers
 * that have to accept a candidate whose title does not match the query verbatim.
 *
 * Both TMDB's director-walk and Filmweb's director+year override face the same
 * problem: a film is filed under a title the cinemas never print — a translation
 * ("Il Maestro" for "Trener Tenisa"), a transliteration ("Mavka. Spravzhnij mif"
 * for "Mawka. Prawdziwy mit"), or the original alphabet ("Мавка. Справжній міф").
 * Pinning on director+year ALONE asserts nothing about the film, and a director
 * with two releases in one year then resolves confidently to the wrong one —
 * Jan Sobierajski's "Mistyczka" took his other 2026 title, "Maryja. Matka
 * Papieża", and served its original title, ratings and Filmweb URL.
 *
 * What survives translation is the proper noun: "Munch", "Mavka", "Giulietta".
 * So the check is a SHARED DISTINCTIVE WORD — long enough not to be an article
 * or a particle, compared after transliteration so another alphabet still
 * counts, and optionally within a character or two so a romanisation that
 * disagrees on one letter ("Mawka" / "Mavka") still ties.
 *
 * Folding is the caller's, passed in as `sanitize`: the movie pipeline hands its
 * `TitleNormalizer.sanitize`, Filmweb its own diacritic-stripping fold. Nothing
 * here reaches for either, so this stays a pure function of its arguments.
 */
object TitleCorroboration {

  /** Shortest word length that counts as EVIDENCE two titles are the same film.
   *  Four keeps the proper nouns a translation preserves ("Munch", "Mavka",
   *  "Giulietta") while dropping the articles and particles that coincide between
   *  unrelated titles in every language ("i", "de", "la", "the", "und"). */
  val DistinctiveToken = 4

  /** Cyrillic → Latin, for comparing a title against one written in another
   *  alphabet. Scoped deliberately to corroboration — cache keys and display
   *  titles are NOT run through it, so nothing about how a film is stored or
   *  shown changes. Ukrainian and Russian letters only, which is what the Polish
   *  corpus actually carries (Ukrainian releases and dubs).
   *
   *  Digraphs first, so `щ`→"shch" isn't clipped by the `ш`→"sh" rule. Soft and
   *  hard signs vanish, as they do in every romanisation. */
  private val CyrillicToLatin: Seq[(String, String)] = Seq(
    "щ" -> "shch", "ж" -> "zh", "ч" -> "ch", "ш" -> "sh", "ц" -> "ts", "х" -> "kh",
    "ю" -> "iu", "я" -> "ia", "є" -> "ie", "ї" -> "i", "й" -> "i",
    "а" -> "a", "б" -> "b", "в" -> "v", "г" -> "h", "ґ" -> "g", "д" -> "d",
    "е" -> "e", "з" -> "z", "и" -> "y", "і" -> "i", "к" -> "k", "л" -> "l",
    "м" -> "m", "н" -> "n", "о" -> "o", "п" -> "p", "р" -> "r", "с" -> "s",
    "т" -> "t", "у" -> "u", "ф" -> "f", "ы" -> "y", "э" -> "e", "ё" -> "e",
    "ь" -> "", "ъ" -> ""
  )

  /** Rewrite any Cyrillic in `s` as Latin, leaving everything else untouched. */
  def latinise(s: String): String =
    if (!s.exists(c => Character.UnicodeBlock.of(c) == Character.UnicodeBlock.CYRILLIC)) s
    else CyrillicToLatin.foldLeft(s.toLowerCase) { case (acc, (from, to)) => acc.replace(from, to) }

  /** Levenshtein edit distance — used to fuzzy-match a cinema's spelling of a
   *  foreign title against a director's filmography ("guru" ↔ "gourou"). Plain
   *  two-row DP, O(a·b); titles are short so it's cheap. A pure function. */
  def editDistance(a: String, b: String): Int = {
    if (a.isEmpty) b.length
    else if (b.isEmpty) a.length
    else {
      var prev = (0 to b.length).toArray
      for (i <- 1 to a.length) {
        val curr = new Array[Int](b.length + 1)
        curr(0) = i
        for (j <- 1 to b.length) {
          val cost = if (a(i - 1) == b(j - 1)) 0 else 1
          curr(j) = math.min(math.min(prev(j) + 1, curr(j - 1) + 1), prev(j - 1) + cost)
        }
        prev = curr
      }
      prev(b.length)
    }
  }

  /** The words in `s` long enough to identify a film, transliterated and folded. */
  def distinctiveTokens(s: String, sanitize: String => String): Set[String] =
    latinise(s).split("[^\\p{L}\\p{N}]+").iterator
      .map(sanitize).filter(_.length >= DistinctiveToken).toSet

  /** Do these two sides of a title comparison share a distinctive word?
   *
   *  `maxTokenEdits` is how far a romanisation may disagree and still tie. Zero
   *  — the default — demands the same word, which is what latinisation already
   *  delivers for the Cyrillic case. One covers a Latin-alphabet romanisation
   *  that picks a different letter for the same sound ("Mawka" / "Mavka") while
   *  still keeping unrelated four-letter words apart. */
  def sharesDistinctiveToken(
    left:          Iterable[String],
    right:         Iterable[String],
    sanitize:      String => String,
    maxTokenEdits: Int = 0
  ): Boolean = {
    val rightTokens = right.iterator.flatMap(distinctiveTokens(_, sanitize)).toSet
    left.iterator.flatMap(distinctiveTokens(_, sanitize)).exists { l =>
      rightTokens.contains(l) ||
        (maxTokenEdits > 0 && rightTokens.exists(editDistance(l, _) <= maxTokenEdits))
    }
  }
}
