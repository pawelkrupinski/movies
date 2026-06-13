package services.movies

import services.titlerules.{TitleRuleSet, TitleRuleDefaults}

import java.util.Locale

object TitleNormalizer {
  // The active rule set drives every prefix/suffix/canonical strip below. It's
  // swapped wholesale by the change-stream consumer when an admin edits a rule;
  // seeded with the migrated defaults so behaviour is correct before Mongo loads
  // and in tests that don't wire a rules store. `@volatile` makes the swap
  // visible to scrape/enrich threads without locking the hot path.
  @volatile private var active: TitleRuleSet = TitleRuleDefaults.ruleSet

  /** Install a new rule set (called by the `titleRules` change-stream consumer). */
  def installRules(rs: TitleRuleSet): Unit = active = rs

  /** The currently-active rule set — read by the admin preview + backfill. */
  def currentRules: TitleRuleSet = active

  /** Restore the in-code defaults. Tests that mutate rules MUST call this in
   *  teardown so they don't leak a rule set into the next spec. */
  def resetToDefaults(): Unit = active = TitleRuleDefaults.ruleSet

  /** Apply a cinema's per-cinema cleanup rules to a raw scraped title. */
  def cinemaClean(cinemaId: String, raw: String): String = active.perCinema(cinemaId, raw)

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
  // The patterns formerly hardcoded here now live in the active `TitleRuleSet`
  // (seeded from `TitleRuleDefaults`, editable in Mongo via the admin page). The
  // tiers:
  //   - `searchTitle` (GlobalStructural) — decoration stripping for EXTERNAL
  //     LOOKUPS ONLY: "Top Gun 40th Anniversary" → "Top Gun" for the TMDB search.
  //     It does NOT feed the merge key (see `sanitize` / `canonical`), so a
  //     decoration edition keys by its own form and stays a separate row.
  //   - `apiQuery` (Search tier on top of structural) — also strips programme
  //     prefixes / accessibility tags / "+ <event>" suffixes for upstream
  //     lookups, while the display row keeps them (a programme screening is its
  //     own row).
  //   - `canonical` (Canonical tier — NO structural) — cross-cinema spelling
  //     unifications (Gwiezdne Wojny / & → i) folded into the stable docId.

  /** Strip cinema decoration (anniversary, restored, Cykl prefix, slash
   *  postfix, language-version suffix) via the GlobalStructural rules — for
   *  external-API lookups only. Identity (`sanitize`) does NOT apply these, so a
   *  decoration edition keys by its own form and stays its own card; this just
   *  finds the base film upstream. */
  def searchTitle(display: String): String = active.structural(display)

  /** Everything `searchTitle` strips PLUS the programme prefix, trailing
   *  accessibility tag, and "+ <event>" suffix (the Search-tier rules). Used by
   *  every external-API resolver so "Kino bez barier: Freak Show (AD + CC + PJM)"
   *  queries upstream as just "Freak Show". */
  def apiQuery(display: String): String = active.search(display)

  /** When `title` opens with a recognised programme prefix (Kino bez barier,
   *  Filmowy Klub Seniora, …), return the matched prefix INCLUDING the trailing
   *  ": " delimiter, so a caller can split the prefix from the film title and
   *  case each half on its own. None when no programme prefix is present. */
  def programmePrefix(title: String): Option[String] = active.programmePrefix(title)

  // Cross-cinema spelling unifications (Gwiezdne Wojny prefix, " & " → " i ")
  // over the trimmed title. Does NOT apply `searchTitle`/structural: decoration
  // (anniversary / wersja / slash / Cykl / restored) is NOT part of identity, so
  // a decoration edition keys by its own form and is NOT merged with the base
  // film. Used by `sanitize` (the docId) and `preferredDisplay`.
  private def canonical(t: String): String = active.canonical(t)

  // Last-resort collapse for titles that share words + order but differ only
  // in punctuation/whitespace ("Top Gun Maverick" vs "Top Gun: Maverick").
  // Lowercased, accents stripped, every non-alphanumeric char dropped. Used
  // by `mergeKeyLookup` ONLY when at least two distinct corpus titles reduce
  // to the same form — so it never collapses a standalone film into siblings
  // that merely share a prefix.
  private def stripPunct(t: String): String =
    java.text.Normalizer.normalize(t, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^a-z0-9]+", "")

  /** Corpus-independent stable key — the same collapse as `mergeKeyLookup`'s
   *  most-aggressive tier (`stripPunct` of `canonical`), applied
   *  unconditionally rather than gated on a sibling reducing to the same
   *  form. Used as the persistent docId in `MovieRepo`/`MovieCache`
   *  so the cache key is stable across refresh ticks and write sites: every
   *  cinema-reported variant of the same film (Arabic/Roman, colon-or-not,
   *  &/i, "Gwiezdne Wojny:" prefix) lands on the same key without needing to
   *  see its sibling in the current corpus. Decoration (anniversary / wersja /
   *  slash / Cykl / restored) is deliberately NOT collapsed here — a decoration
   *  edition is a distinct identity and keeps its own key + card.
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
      .toLowerCase(Locale.ROOT)
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
      romanized.iterator.map(t => canonical(t).toLowerCase(Locale.ROOT)).toSeq
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
      val cLower  = canonical(r).toLowerCase(Locale.ROOT)
      val rLower  = r.toLowerCase(Locale.ROOT)
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
      // it. If canonicals still differ, pick via `displayLadderKey` — a total,
      // CONTENT-deterministic ordering (no input-index), so the displayed title
      // never depends on the order the cinema spellings arrived in (the
      // whole-corpus snapshot flake). Cross-script identity is already settled
      // by the caller (`MovieRecord.displayTitle` picks the dominant `sanitize`
      // key before calling here), so this ladder only ranks same-identity
      // spellings of one film.
      val canonicals = seq.map(canonical).distinct
      if (canonicals.size == 1) canonicals.headOption
      else canonicals.sortBy(displayLadderKey).headOption
    }
  }

  // Deterministic preference ladder for same-identity title spellings. Pure
  // function of the string — the pick never depends on scrape/merge order.
  // Axes, best-first (the `-` makes "more is better" sort first under ascending
  // `sortBy`):
  //   1. richer punctuation — "Top Gun: Maverick" over "Top Gun Maverick"
  //   2. diacritics present — "Diabeł" over a scraper-flattened "Diabel"
  //   3. mixed case, not ALL-CAPS — "Top Gun" over "TOP GUN"
  //   4. least leading/trailing junk — "Werdykt" over "Werdykt." / "„Arco”"
  //   5. shorter — demoted below the quality axes so it can't strip the colon
  //   6. the string itself — total, order-independent final fallback
  private def displayLadderKey(c: String): (Int, Int, Int, Int, Int, String) = {
    // Strip leading/trailing non-alphanumerics so a stray trailing "." or
    // wrapping „quotes" count as junk (axis 4), NOT as richer interior
    // punctuation (axis 1) — otherwise "Werdykt." would outrank "Werdykt".
    val trimmed   = c.dropWhile(!_.isLetterOrDigit)
                     .reverse.dropWhile(!_.isLetterOrDigit).reverse
    val punct     = trimmed.count(ch => !ch.isLetterOrDigit && !ch.isWhitespace)
    val diacritic = if (c.exists(ch => ch.isLetter && ch.toInt > 127)) 1 else 0
    val mixedCase = if (c.exists(_.isUpper) && c.exists(_.isLower)) 1 else 0
    val junk      = c.length - trimmed.length
    (-punct, -diacritic, -mixedCase, junk, c.length, c)
  }

  /** Whether a title is clean enough to display verbatim. Used to gate the
   *  TMDB-Polish-title preference in `MovieRecord.displayTitle`: TMDB's
   *  crowd-sourced titles are usually the canonical form, but a minority are
   *  malformed — ALL-CAPS ("ALL YOU NEED IS KILL"), double-spaced ("Super
   *  Mario  Galaxy Film"), or carrying edge junk ("Zaproszenie."). When TMDB's
   *  title fails this check we fall back to the cinema spelling ladder, which
   *  has the well-formed form the cinemas advertise. */
  def wellFormedTitle(t: String): Boolean = {
    val letters       = t.filter(_.isLetter)
    val notAllCaps    = letters.isEmpty || letters.exists(_.isLower)
    val noDoubleSpace = !t.contains("  ")
    val noEdgeJunk    = t.headOption.exists(_.isLetterOrDigit) &&
                        t.lastOption.exists(_.isLetterOrDigit)
    notAllCaps && noDoubleSpace && noEdgeJunk
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
