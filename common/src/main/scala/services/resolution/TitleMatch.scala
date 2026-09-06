package services.resolution

import tools.{EditDistance, TextNormalization}

/**
 * The title tests the external-site matchers share — Metacritic, Rotten Tomatoes,
 * Filmweb, IMDb, OMDb, Cinemeta and TMDB's director-walk each decide for
 * themselves WHICH of these to apply and in what order, but the tests themselves
 * have one definition each, so a dash glyph or a leading article means the same
 * thing on every site.
 *
 * Three families:
 *
 *  - FOLDS and the comparisons on top of them ([[fold]], [[deburredFold]],
 *    [[exact]], [[isModifierSuffix]], [[oneStartsWithTheOther]], [[close]]).
 *    OMDb and Cinemeta keep their own alphanumeric-only folds — each differs
 *    from these and from the other — and bring only the comparison here.
 *  - SLUG LADDER spellings ([[dropLeadingArticle]], [[yearSuffixedFirst]]) for
 *    the sites reached by probing a URL rather than searching.
 *  - CORROBORATION across a translation ([[sharesDistinctiveToken]]) for the
 *    resolvers that must accept a candidate whose title does not match verbatim.
 *
 * Nothing here reaches for a pipeline-wide normaliser: where a fold is the
 * caller's (`TitleNormalizer.sanitize`, Filmweb's diacritic strip) it is passed
 * in, so every function is pure in its arguments.
 */
object TitleMatch {

  // ── Folds ──────────────────────────────────────────────────────────────────

  // Unicode dash variants (hyphen-minus aside): hyphen, non-breaking hyphen,
  // figure dash, en dash, em dash, horizontal bar, minus sign. Cinemas and the
  // rating sources disagree on which one a title uses ("Chainsaw Man – The
  // Movie" vs "Chainsaw Man - The Movie"), so fold them all to ASCII '-' before
  // comparing titles.
  private val DashVariants: Set[Char] = Set('‐', '‑', '‒', '–', '—', '―', '−')

  /** Fold every Unicode dash variant in `s` to ASCII '-'. Case- and
   *  diacritic-preserving — [[fold]] and [[deburredFold]] layer those on. */
  def foldDashes(s: String): String =
    if (s.exists(DashVariants)) s.map(c => if (DashVariants(c)) '-' else c) else s

  /** Lower-case + trim + every dash variant to '-', for title equality. Keeps
   *  diacritics — Metacritic, Rotten Tomatoes and Filmweb titles carry them and
   *  so do the titles they are compared against. */
  def fold(s: String): String = foldDashes(s.toLowerCase.trim)

  /** [[fold]] after stripping diacritics — IMDb stores titles in ASCII (ł→l,
   *  ą→a, ś→s) while query titles keep theirs, so both sides are deburred
   *  before they meet. `TextNormalization.deburr` is NFD stripping PLUS the
   *  explicit ł→l that NFD alone misses. */
  def deburredFold(s: String): String =
    foldDashes(TextNormalization.deburr(s).toLowerCase.trim)

  // ── Comparisons ────────────────────────────────────────────────────────────

  /** The same title under [[fold]]: case, surrounding whitespace and the dash
   *  glyph are not differences; diacritics and punctuation are. */
  def exact(a: String, b: String): Boolean = fold(a) == fold(b)

  /** True when `title` starts with `query` and the *next* non-space character
   *  is punctuation — indicating a modifier suffix like " - Re-Release",
   *  ": Restored", " (Anniversary Edition)". False for "Deaf President Now!"
   *  vs "Deaf" (next char "P" is alphanumeric → different film), and for
   *  exact equals (caller treats those separately).
   *
   *  `query` is expected pre-lowercased + trimmed. Both sides are dash-folded
   *  so an en-dash title still prefix-matches a hyphen query (and vice versa).
   */
  def isModifierSuffix(title: String, query: String): Boolean = {
    val normalizedQuery = foldDashes(query)
    val normalizedTitle = foldDashes(title.toLowerCase.trim)
    normalizedTitle.startsWith(normalizedQuery) && normalizedTitle != normalizedQuery && {
      val rest = normalizedTitle.drop(normalizedQuery.length).dropWhile(_.isWhitespace)
      rest.headOption.exists(c => !c.isLetterOrDigit)
    }
  }

  /** Either already-folded title is a prefix of the other, neither being empty —
   *  the "containing title" half of OMDb's and Cinemeta's corroboration, which
   *  only ever counts alongside an agreeing year. */
  def oneStartsWithTheOther(a: String, b: String): Boolean =
    a.nonEmpty && b.nonEmpty && (a.startsWith(b) || b.startsWith(a))

  /** Two (already-sanitised) titles within a tight edit distance: at most 2
   *  edits AND at most a third of the longer title. Scoped to a small, trusted
   *  candidate set — a director's filmography — where a cinema's spelling of a
   *  foreign title drifts from TMDB's ("guru" → "gourou") but "guru" must never
   *  reach "dalloway". */
  def close(a: String, b: String): Boolean = {
    val d = EditDistance.between(a, b)
    d <= 2 && d * 3 <= math.max(a.length, b.length)
  }

  // ── Slug ladder ────────────────────────────────────────────────────────────

  /** Some films index without their leading "the"/"a"/"an" (more common on
   *  RT, but happens on Metacritic too). Returns the de-articled slug only
   *  when the leading article is present, so callers can decide whether to
   *  also probe the variant.
   */
  def dropLeadingArticle(slug: String, sep: Char): Option[String] = {
    val prefixes = Seq(s"the$sep", s"a$sep", s"an$sep")
    prefixes.collectFirst { case p if slug.startsWith(p) => slug.drop(p.length) }
  }

  /** Interleave `base` slug forms with their `<slug><sep><year>` variants, each
   *  year-suffixed form immediately BEFORE its bare form. No year → unchanged.
   *
   *  Both Metacritic and Rotten Tomatoes disambiguate same-titled films with a
   *  year suffix, and for a NEW film the bare slug is routinely the older
   *  namesake: `/movie/the-odyssey` is Jerome Salle's Cousteau biopic while
   *  Nolan's 2026 film is `/movie/the-odyssey-2026`. Probing bare-first stored
   *  the wrong film — and the year guard could not catch it, because that page
   *  serves `datePublished: "0000-00-00"`, which parses to no year at all and so
   *  is "compatible" with everything. Trying the year-suffixed form first is what
   *  actually separates them; the guard only rejects what it can disprove.
   *
   *  Shared by both clients (RT with '_', MC with '-') so the ordering rule has
   *  one definition. */
  def yearSuffixedFirst(base: Seq[String], year: Option[Int], separator: Char): Seq[String] =
    year.fold(base)(y => base.flatMap(s => Seq(s"$s$separator$y", s)).distinct)

  // ── Corroboration across a translation ─────────────────────────────────────
  //
  // Both TMDB's director-walk and Filmweb's director+year override face the same
  // problem: a film is filed under a title the cinemas never print — a translation
  // ("Il Maestro" for "Trener Tenisa"), a transliteration ("Mavka. Spravzhnij mif"
  // for "Mawka. Prawdziwy mit"), or the original alphabet ("Мавка. Справжній міф").
  // Pinning on director+year ALONE asserts nothing about the film, and a director
  // with two releases in one year then resolves confidently to the wrong one —
  // Jan Sobierajski's "Mistyczka" took his other 2026 title, "Maryja. Matka
  // Papieża", and served its original title, ratings and Filmweb URL.
  //
  // What survives translation is the proper noun: "Munch", "Mavka", "Giulietta".
  // So the check is a SHARED DISTINCTIVE WORD — long enough not to be an article
  // or a particle, compared after transliteration so another alphabet still
  // counts, and optionally within a character or two so a romanisation that
  // disagrees on one letter ("Mawka" / "Mavka") still ties.

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
        (maxTokenEdits > 0 && rightTokens.exists(EditDistance.between(l, _) <= maxTokenEdits))
    }
  }
}
