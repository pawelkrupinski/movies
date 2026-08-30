package tools

/**
 * Display casing for a PERSON's name (cast and crew), for sources that ship the
 * name in a case they invented rather than the case it is written in.
 *
 * WHY: cast names reach the corpus from a dozen places at once — TMDB and IMDb
 * send properly-cased credits, but several scraped listing sites shout or
 * whisper them. Flicks is the clearest: its session buttons carry a
 * `data-eventjson` blob whose `content_cast` is emitted entirely lowercase
 * ("christoph waltz, sandra bullock"), and that string was stored and rendered
 * verbatim. The corpus is therefore MIXED, and any repair has to run over the
 * good names as well as the bad ones without damaging the good ones.
 *
 * ==The gate: only an ALL-LOWERCASE name is touched==
 *
 * [[capitalized]] returns its input byte-identical unless the string contains
 * NO uppercase letter at all. This is the single most important property here,
 * for two reasons: the helper runs at a parse boundary shared with
 * already-correct sources, and the backfill runs it over an entire corpus of
 * mostly-correct rows. A name that already carries any uppercase letter is
 * evidence that its source cased it deliberately, and we do not second-guess
 * that — "Ke Huy Quan", "RZA", "Daniel Day-Lewis", "Ludwig van Beethoven" and
 * even the deliberately odd "Sirbossman" spellings a source chose all pass
 * through untouched. (The cost of that gate: a genuinely all-lowercase stage
 * name such as "k.d. lang" or "will.i.am" IS recased, because nothing
 * distinguishes it from a shouted-down credit. Two names against a corpus-wide
 * repair is the right side of the trade.)
 *
 * '''ALL-CAPS ("SMITH") is deliberately left ALONE.''' An all-caps token cannot
 * be told apart from a stage name or acronym that is genuinely upper — RZA, MGK,
 * T.I., ODB, JAY-Z — and no cast source we ingest has been observed shouting,
 * so there is no bug to fix and only damage to do. Callers that KNOW their
 * source shouts everything should use [[TextNormalization.titleCaseIfAllCaps]],
 * which is exactly that narrower tool.
 *
 * ==The rules applied to an all-lowercase name==
 *
 *  - '''Word start''' — the first letter of each whitespace-separated word is
 *    uppercased: `christoph waltz` → `Christoph Waltz`.
 *  - '''Hyphens and dots''' — both start a fresh sub-word, so
 *    `joseph gordon-levitt` → `Joseph Gordon-Levitt` and the initials
 *    `samuel l. jackson` / `j.k. simmons` → `Samuel L. Jackson` /
 *    `J.K. Simmons`.
 *  - '''Apostrophes''' — the letter after `'` is uppercased ONLY when the
 *    letter-run before the apostrophe is at most two letters long, which is what
 *    an elided-particle prefix looks like: `peter o'toole` → `Peter O'Toole`,
 *    `vincent d'onofrio` → `Vincent D'Onofrio`, `n'dour` → `N'Dour`. Names whose
 *    apostrophe sits INSIDE the word instead — Hawaiian `keali'i`, Ivorian
 *    `n'guessan` is covered by the prefix rule, `dell'orto` is not — keep their
 *    lowercase letter (`Keali'i`, `Dell'orto`), which is the safe direction: a
 *    missed capital reads as a spelling, a spurious one reads as a typo.
 *  - '''Mc''' — a word beginning `mc` with at least two more letters gets the
 *    third letter uppercased: `mcconaughey` → `McConaughey`, `mcavoy` →
 *    `McAvoy`, `mcdormand` → `McDormand`.
 *  - '''Mac is NOT special-cased.''' `mac` is also just three ordinary letters —
 *    `macy`, `machado`, `macario`, `mackenzie`, `macht` — and there is no
 *    reliable way to tell `MacDowell` from `Macy` without a name list. `Mac…`
 *    words are therefore title-cased plainly, so `macy` → `Macy` (never
 *    `MacY`) and `macdonald` → `Macdonald`. Under-capitalising a genuine
 *    `MacDowell` is the acceptable half of that trade; mangling `Macy` is not.
 *  - '''Nobiliary particles''' — `de del della delle den der des di du da das dos
 *    van von vom der zu zur ter ten la le les el al bin ibn bint af av y e`
 *    stay lowercase in the MIDDLE of a name — `ludwig van beethoven` → `Ludwig
 *    van Beethoven`, `robert de niro` → `Robert de Niro`, `alexander von
 *    humboldt` → `Alexander von Humboldt` — but are
 *    capitalised when they LEAD it, because a leading `al`/`van`/`di` is far
 *    more often a given name or an Anglicised surname than a particle:
 *    `al pacino` → `Al Pacino`, `van damme` → `Van Damme`.
 *
 * ==Unicode==
 *
 * Casing goes through `Character.toUpperCase(char)`, which is locale-INDEPENDENT
 * — `String.toUpperCase` without an explicit locale turns `i` into `İ` under a
 * Turkish default locale, the exact shape of a flake this repo has already been
 * bitten by. Polish `ł` → `Ł` and every other single-char mapping works;
 * `józef piłsudski` → `Józef Piłsudski` and `renée zellweger` → `Renée
 * Zellweger`.
 */
object PersonName {

  /** Lowercase words that stay lowercase inside a name but are capitalised when
   *  they lead it. See the class comment for why the leading case flips. */
  private val Particles: Set[String] = Set(
    "de", "del", "della", "delle", "den", "der", "des", "di", "du", "da", "das", "dos",
    "van", "von", "vom", "zu", "zur", "ter", "ten",
    "la", "le", "les", "el", "al", "bin", "ibn", "bint",
    "af", "av", "y", "e"
  )

  /** Characters that start a fresh sub-word inside a token. */
  private def isSubWordBreak(c: Char): Boolean = c == '-' || c == '.' || c == '–' || c == '—' || c == '/'

  private def isApostrophe(c: Char): Boolean = c == '\'' || c == '’' || c == 'ʼ'

  /**
   * The display casing of one person's name — unchanged unless the name carries
   * no uppercase letter at all. See the class comment for the full rule set.
   */
  def capitalized(name: String): String = {
    if (name.isEmpty || name.exists(Character.isUpperCase)) return name
    val chars = name.toCharArray
    var index = 0
    var wordOrdinal = 0
    while (index < chars.length) {
      if (Character.isWhitespace(chars(index))) index += 1
      else {
        var end = index
        while (end < chars.length && !Character.isWhitespace(chars(end))) end += 1
        capitalizeWord(chars, index, end, leading = wordOrdinal == 0)
        wordOrdinal += 1
        index = end
      }
    }
    new String(chars)
  }

  /** [[capitalized]] over a cast/crew list. */
  def capitalizedAll(names: Seq[String]): Seq[String] = names.map(capitalized)

  /** Case one whitespace-delimited word of an all-lowercase name, in place. */
  private def capitalizeWord(chars: Array[Char], start: Int, end: Int, leading: Boolean): Unit = {
    if (!leading && Particles.contains(letterCore(chars, start, end))) return

    var index = start
    var atSubWordStart = true
    // Letters seen since the last sub-word break — the apostrophe rule's "how
    // long is the prefix" measure.
    var lettersInSegment = 0
    while (index < end) {
      val character = chars(index)
      if (Character.isLetter(character)) {
        if (atSubWordStart) chars(index) = Character.toUpperCase(character)
        atSubWordStart = false
        lettersInSegment += 1
      } else if (isSubWordBreak(character)) {
        atSubWordStart = true
        lettersInSegment = 0
      } else if (isApostrophe(character)) {
        // `o'toole` / `d'onofrio`: a one- or two-letter prefix is an elided
        // particle, so the next letter starts the real name. A longer prefix
        // (`keali'i`) is a word with an apostrophe in it — leave it alone.
        atSubWordStart = lettersInSegment <= 2
        lettersInSegment = 0
      } else {
        // Digits and other punctuation neither break nor extend the segment.
        atSubWordStart = false
      }
      index += 1
    }
    applyMcPrefix(chars, start, end)
  }

  /** `mcconaughey` → `McConaughey`. Runs after the word has been title-cased, so
   *  the word already reads `Mcconaughey` and only the third letter moves. */
  private def applyMcPrefix(chars: Array[Char], start: Int, end: Int): Unit =
    if (end - start >= 4 && chars(start) == 'M' && chars(start + 1) == 'c' &&
        Character.isLetter(chars(start + 2)) && Character.isLetter(chars(start + 3)))
      chars(start + 2) = Character.toUpperCase(chars(start + 2))

  /** The word's letters only, for the particle lookup — so a trailing comma or a
   *  wrapping paren can't hide `van` from the table. */
  private def letterCore(chars: Array[Char], start: Int, end: Int): String = {
    val builder = new StringBuilder(end - start)
    var index = start
    while (index < end) {
      if (Character.isLetter(chars(index))) builder.append(chars(index))
      index += 1
    }
    builder.toString
  }
}
