package services.movies

object TitleNormalizer {
  // "Mortal Kombat 2" and "Mortal Kombat II" should collapse.
  private val ArabicToRoman = Map(
    "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
    "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
    "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
    "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX"
  )

  // Always-applied transformation: standalone Arabic numerals → Roman.
  def normalize(title: String): String =
    title.split(" ").map(word => ArabicToRoman.getOrElse(word, word)).mkString(" ")

  // ── Cinema-decoration stripping ────────────────────────────────────────────
  //
  // Single set of patterns reused by:
  //   - mergeKey / preferredDisplay  — so "Top Gun 40th Anniversary" and a
  //     plain "Top Gun" listing collapse into one card.
  //   - MovieService.searchTitle — so the TMDB/OMDb query is "Top Gun",
  //     not "Top Gun 40th Anniversary" (the latter doesn't match anything).
  //
  // Patterns target decoration that cinemas apply to a base film title —
  // anniversary rereleases, remasters, cycle screenings, bilingual postfixes.
  // Each is anchored so that a sequel ("Top Gun: Maverick", "Mortal Kombat II"),
  // a real-titled film ("Rocznica"), or a synopsis-style line can't be hit.
  private val CyklPrefix        = """^Cykl\s+[„"][^„""]*[„""]?\s+[-–—]\s+""".r
  private val SlashSuffix       = """\s+/\s+.+$""".r
  // Require the suffix to start with a letter so that mathematical titles
  // like "Orwell: 2 + 2 = 5" aren't truncated to "Orwell: 2".
  private val PlusSuffix        = """\s+\+\s+\p{L}.+$""".r
  private val AnniversarySuffix = """(?i)\s*[-–—|.]?\s*\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""".r
  private val RestoredSuffix    = """(?i)\s*[-–—|.]?\s*\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""".r
  private val WersjaSuffix      = """(?i)\s*[-–—.]\s+wersja\s+\p{L}+\s*$""".r

  /** Strip cinema decoration (anniversary, restored, Cykl prefix, bilingual
   *  postfix, remaster suffix). Display titles intentionally keep these — only
   *  used for grouping and external-API lookups.
   */
  def searchTitle(display: String): String = {
    val a = CyklPrefix.replaceFirstIn(display, "")
    val b = SlashSuffix.replaceFirstIn(a, "")
    val c = PlusSuffix.replaceFirstIn(b, "")
    val d = AnniversarySuffix.replaceFirstIn(c, "")
    val e = RestoredSuffix.replaceFirstIn(d, "")
    WersjaSuffix.replaceFirstIn(e, "").trim
  }

  // Conditional cleanups for merging — applied only when another title in
  // `allTitles` reduces to the same canonical form. Builds on `searchTitle` so
  // anniversary/wersja variants merge with their base film, then adds the
  // cross-cinema spelling unifications (Gwiezdne Wojny prefix, " & " → " i ")
  // that aren't needed for enrichment lookups.
  private def canonical(t: String): String =
    searchTitle(t).stripPrefix("Gwiezdne Wojny: ").replace(" & ", " i ")

  // Last-resort collapse for titles that share words + order but differ only
  // in punctuation/whitespace ("Top Gun Maverick" vs "Top Gun: Maverick").
  // Lowercased, accents stripped, every non-alphanumeric char dropped. Used
  // by `mergeKeyLookup` ONLY when at least two distinct corpus titles reduce
  // to the same form — so it never collapses a standalone film into siblings
  // that merely share a prefix.
  private def stripPunct(t: String): String =
    java.text.Normalizer.normalize(t, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
      .toLowerCase
      .replaceAll("[^a-z0-9]+", "")

  /** Corpus-independent stable key — the same collapse as `mergeKeyLookup`'s
   *  most-aggressive tier (`stripPunct` of `canonical`), applied
   *  unconditionally rather than gated on a sibling reducing to the same
   *  form. Used as the persistent docId in `MovieRepo`/`MovieCache`
   *  so the cache key is stable across refresh ticks and write sites: every
   *  cinema-reported variant of the same film (Arabic/Roman, colon-or-not,
   *  &/i, anniversary suffix, "Gwiezdne Wojny:" prefix) lands on the same
   *  key without needing to see its sibling in the current corpus.
   *
   *  Unicode-aware on the strip step — preserves Cyrillic / Greek / CJK
   *  letters so non-Latin titles keep a non-empty key. Polish `ł` is folded
   *  to `l` so "Diabeł" and "Diabel" share a key (NFD doesn't decompose `ł`).
   *
   *  Per-script titles still get distinct keys (Latin vs Cyrillic translations
   *  of the same film stay as separate records). The imdbId re-merge step
   *  (later phase) folds those across scripts. */
  def sanitize(title: String): String =
    tools.TextNormalization.deburr(canonical(normalize(title)))
      .toLowerCase
      .replaceAll("[^\\p{L}\\p{N}]+", "")

  // Group key for merging. Falls back to the plain Roman-numeral form when no
  // sibling title reduces to the same canonical.
  def mergeKey(title: String, allTitles: Iterable[String]): String =
    mergeKeyLookup(allTitles)(title)

  // Faster batch entry point: when caller has many titles to key, pre-compute
  // the canonical→count index once (O(N)) and then look up each title in O(1).
  // Caller iterates with `index(title)`. Equivalent semantics to `mergeKey`.
  //
  // Counts are keyed by *lower-cased* canonical so cross-cinema casing diffs
  // (e.g. Rialto's sentence-case "Top gun | 40 rocznica" alongside Helios's
  // "Top Gun 40th Anniversary") don't prevent a merge.
  def mergeKeyLookup(allTitles: Iterable[String]): String => String = {
    val romanized = allTitles.iterator.map(normalize).toSet
    val canonicalCounts: Map[String, Int] =
      romanized.iterator.map(t => canonical(t).toLowerCase).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    // Punctuation-stripped counts — for cases where two titles share words +
    // word order but differ only in : / - / whitespace. Built on top of
    // canonical so this also catches "Mandalorian & Grogu" ≡ "Mandalorian i
    // Grogu" when they additionally lose their colon.
    val puncStripCounts: Map[String, Int] =
      romanized.iterator.map(t => stripPunct(canonical(t))).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    title => {
      val r       = normalize(title)
      val cLower  = canonical(r).toLowerCase
      val rLower  = r.toLowerCase
      val p       = stripPunct(cLower)
      // Punctuation-strip is the widest collapse — check first. Only fires
      // when ≥2 distinct corpus titles reduce to the same form, so a lone
      // film never gets a key derived from punctuation it didn't share.
      if (p.nonEmpty && puncStripCounts.getOrElse(p, 0) > 1) p
      else if (cLower != rLower && canonicalCounts.getOrElse(cLower, 0) > 1) cLower
      else rLower
    }
  }

  // Among a group of titles that merge to one schedule, pick the display form.
  // When a merge happens (two or more distinct titles), always show the
  // canonical form — " & " replaced with " i " and the "Gwiezdne Wojny: "
  // prefix stripped — even when the canonical form isn't literally present in
  // the input. That way "Mandalorian i Grogu" wins over both
  // "Mandalorian & Grogu" and "Gwiezdne Wojny: Mandalorian i Grogu", regardless
  // of which spellings the cinemas happened to ship.
  // A single-title group is returned untouched to avoid mutating standalone
  // titles like "Gwiezdne Wojny: A New Hope" or "Pizza & Pasta" that did not
  // actually trigger a merge.
  def preferredDisplay(titles: Iterable[String]): Option[String] = {
    val seq = titles.iterator.toSeq.distinct
    if (seq.size <= 1) seq.headOption
    else {
      // After canonical (decoration stripping, & → i, Gwiezdne Wojny: removed),
      // a merged group typically reduces to a single canonical form — return
      // it. If canonicals still differ, score each by:
      //   1. richest punctuation — "Top Gun: Maverick" over "Top Gun Maverick"
      //   2. Latin script         — Polish "Diabeł ubiera się u Prady 2" over
      //                             Cyrillic "ДИЯВОЛ НОСИТЬ ПРАДА 2" when the
      //                             same record collected both during a
      //                             cross-script merge that pre-dated the
      //                             cross-script-block rule.
      //   3. mixed case           — proper "Top Gun: Maverick" beats all-caps
      //                             "TOP GUN: MAVERICK" and a Rialto-style
      //                             sentence-cased duplicate. (All-caps
      //                             Cyrillic also loses on this axis since
      //                             Cyrillic ALL CAPS scores no `isLower`.)
      //   4. earliest input       — deterministic tiebreaker.
      val canonicals = seq.map(canonical).distinct
      if (canonicals.size == 1) canonicals.headOption
      else canonicals.zipWithIndex.maxByOption { case (c, i) =>
        val punct      = c.count(ch => !ch.isLetterOrDigit && !ch.isWhitespace)
        val latinScore = if (isLatinDominant(c)) 1 else 0
        val mixedCase  = if (c.exists(_.isUpper) && c.exists(_.isLower)) 1 else 0
        (punct, latinScore, mixedCase, -i)
      }.map(_._1)
    }
  }

  /** True when most of `s`'s letters are in the Latin Unicode script.
   *  Polish diacritics (`ł`, `ś`, `ą`, …) count as Latin; Cyrillic and CJK
   *  do not. Used to favour the Polish/Latin variant of a film over the
   *  Ukrainian/Cyrillic one, and to filter cross-script entries out of
   *  `cinemaTitles` so a single row never accumulates spellings in two
   *  scripts. */
  def isLatinDominant(s: String): Boolean = {
    val letters = s.filter(_.isLetter)
    if (letters.isEmpty) false
    else letters.count(c =>
      Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN
    ) * 2 >= letters.length
  }

  /** Two titles share a "primary script" when both are Latin-dominant or
   *  both are not. We treat scripts as a binary distinction (Latin /
   *  non-Latin) because the only cross-script collisions we actually see
   *  in cinema data are Polish-vs-Ukrainian — finer-grained script splits
   *  would just create unnecessary rows. */
  def sameScript(a: String, b: String): Boolean =
    isLatinDominant(a) == isLatinDominant(b)
}
