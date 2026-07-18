package services.movies

import services.titlerules.TitleRuleSet

import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern

object TitleNormalizer {
  // The active rule set drives every prefix/suffix/canonical strip below.
  // Rules live in code (TitleRules + ExtraTitleRules) and are loaded once
  // at class-load. `@volatile` makes the reference visible to scrape/enrich
  // threads; tests can swap it thread-locally via `withRules`.
  // Scoped to the country THIS process serves: a language-specific rule (the
  // Polish " & " → " i " unification) must not rewrite a German or British title.
  // `soleFromEnv`, NOT `fromEnv`: the worker names its country through
  // `KINOWO_COUNTRIES`, so reading only `KINOWO_COUNTRY` gives it the Poland
  // default and the scoping does nothing on the process that writes the corpus.
  // A multi-country worker gets `None` — no global set can be right for it, so it
  // falls back to the default and must scope per country via `withRules`.
  @volatile private var active: TitleRuleSet =
    TitleRuleSet.forCountry(models.Country.soleFromEnv.getOrElse(models.Country.default))

  // A THREAD-SCOPED override that, when set, shadows `active` for the current
  // thread only. Tests that need a custom rule set install it here via
  // `withRules` instead of swapping the global `active`: ScalaTest runs suites in
  // PARALLEL (`Test / parallelExecution` defaults to true), so a global swap by
  // one suite leaked its rules into every OTHER suite's `sanitize` for the
  // duration of its `try` block — a rare, order-dependent flake that no
  // `finally` restore could prevent (the race is during the install, not after).
  // Scoping the override to the installing thread keeps a test's rules invisible
  // to the suites running beside it. Production never sets this; the change-stream
  // consumer still swaps `active` globally via `installRules`.
  private val scopedOverride = new ThreadLocal[TitleRuleSet]()
  private def effective: TitleRuleSet = scopedOverride.get() match {
    case null => active
    case rs   => rs
  }

  /** Install a rule set — used by tests that want to exercise a custom set globally. */
  def installRules(rs: TitleRuleSet): Unit = { active = rs; sanitizeCache.clear() }

  /** Run `body` with `rs` as the active rule set for the CURRENT THREAD only,
   *  restoring the prior state afterwards. The scope is thread-local so a test
   *  installing custom rules can't leak them into a suite running in parallel —
   *  see `scopedOverride`. The body must drive its title normalisation on the
   *  calling thread (the common case for unit specs). */
  def withRules[A](rs: TitleRuleSet)(body: => A): A = {
    val prev = scopedOverride.get()
    scopedOverride.set(rs)
    try body
    finally if (prev == null) scopedOverride.remove() else scopedOverride.set(prev)
  }

  /** The currently-active GLOBAL rule set — read by the admin preview + backfill
   *  (which run without a thread scope, so they see what `installRules` set). */
  def currentRules: TitleRuleSet = active

  /** Restore the full in-code rule set on the GLOBAL slot — used by tests after a global swap. */
  def resetToDefaults(): Unit = { active = TitleRuleSet.forCountry(models.Country.soleFromEnv.getOrElse(models.Country.default)); sanitizeCache.clear() }

  /** Apply a cinema's per-cinema cleanup rules to a raw scraped title. */
  def cinemaClean(cinemaId: String, raw: String): String = effective.perCinema(cinemaId, raw)

  // Precompiled hot-path patterns. `sanitize` / `stripPunct` run per movie ×
  // per cinema × per tick (plus every staging row and read-model projection);
  // `String.replaceAll` recompiles its `Pattern` on every call, so we compile
  // these once. `CombiningMarks` mirrors the NFD combining-mark strip; the
  // `NonAlnum*` pair drops the residual punctuation/whitespace, one Unicode-aware
  // (keeps Cyrillic/Greek/CJK letters) and one ASCII-only.
  private val CombiningMarks  = Pattern.compile("\\p{M}")
  private val NonAlnumUnicode = Pattern.compile("[^\\p{L}\\p{N}]+")
  private val NonAlnumAscii   = Pattern.compile("[^a-z0-9]+")

  // "Mortal Kombat 2" and "Mortal Kombat II" should collapse — onto the ARABIC
  // form (the spelling cinemas + TMDB actually use), so keys read `mortalkombat2`,
  // not `mortalkombatii`. Only MULTI-letter Roman numerals are converted: the
  // single letters I, V, X collide with real title words ("I Am Legend",
  // "Malcolm X", "V for Vendetta", Polish "i" = and), so converting them would
  // corrupt those titles. The cost is not unifying a bare Roman single-digit
  // ("Rocky V") with its Arabic form ("Rocky 5"), which cinema listings
  // effectively never use.
  private val RomanToArabic = Map(
    "II" -> "2", "III" -> "3", "IV" -> "4", "VI" -> "6", "VII" -> "7",
    "VIII" -> "8", "IX" -> "9", "XI" -> "11", "XII" -> "12", "XIII" -> "13",
    "XIV" -> "14", "XV" -> "15", "XVI" -> "16", "XVII" -> "17", "XVIII" -> "18",
    "XIX" -> "19", "XX" -> "20"
  )

  // Always-applied transformation: standalone (space-delimited) multi-letter Roman
  // numerals → Arabic, CASE-INSENSITIVELY so "Mortal Kombat II" (chains) and
  // "Mortal kombat ii" (a lower-casing cinema) fold to the same `mortalkombat2`
  // rather than splitting. `sanitize` runs this AFTER `canonical` (not before): a
  // decoration glued to a numeral with no separating space ("Mortal Kombat II-
  // dubbing" → token "II-") hides the numeral until canonical strips the
  // decoration, so normalising first stranded it as Roman while the stripped
  // display form ("Mortal Kombat II") deromanised it — the two then sanitized to
  // different keys and the film never settled (the staging re-divert loop).
  def normalize(title: String): String =
    title.split(" ").map(word => RomanToArabic.getOrElse(word.toUpperCase(Locale.ROOT), word)).mkString(" ")

  // ── Cinema-decoration stripping ────────────────────────────────────────────
  //
  // The patterns formerly hardcoded here now live in the active `TitleRuleSet`
  // (seeded from `TitleRules`, editable in Mongo via the admin page). The
  // tiers:
  //   - `apiQuery` (GlobalStructural) — decoration strips (anniversary, restored,
  //     Cykl prefix, slash, language-version) PLUS programme prefixes /
  //     accessibility tags / "+ <event>" suffixes, for EXTERNAL LOOKUPS ONLY:
  //     "Kino bez barier: Freak Show (AD + CC + PJM)" → "Freak Show". It does NOT
  //     feed the merge key (see `sanitize` / `canonical`), so a decoration /
  //     programme edition keys by its own form and stays a separate row.
  //   - `canonical` (Canonical tier — NO structural) — cross-cinema spelling
  //     unifications (Gwiezdne Wojny / & → i) folded into the stable documentId.
  //
  // Display casing (`recase`) reuses the same tier's `^`-anchored rules to find
  // banner boundaries, but only re-cases — it never strips.

  /** The aggressive strip used by every external-API resolver: decoration plus
   *  programme prefix / accessibility tag / "+ <event>" suffix, so
   *  "Kino bez barier: Freak Show (AD + CC + PJM)" queries upstream as just
   *  "Freak Show". Identity (`sanitize`) does NOT apply these, so the decorated
   *  row stays its own card; this just finds the base film upstream.
   *
   *  NOTE: this is the literal query sent to TMDB / Filmweb / etc., and it stays
   *  in the ORIGINAL script — TMDB resolves Cyrillic titles fine via its
   *  alternative-title index, so romanizing here would replace an exact
   *  Ukrainian alt-title match with a transliteration upstream doesn't know.
   *  Cross-script folding of an UNresolved orphan onto its Latin sibling is done
   *  separately, on the canonicalizer's union key — see
   *  `FilmCanonicalizer.groupByFilm`'s search-title edge. */
  def apiQuery(display: String): String = effective.search(display)

  /** Display-side casing applied to EVERY scraper's title at the scrape choke
   *  point (`MovieCache.recordCinemaScrape`). Banner-aware: when a leading
   *  banner rule matches (any programme prefix, the Cykl prefix, …), split at
   *  its boundary and case the banner and the film independently so the film
   *  keeps its own capital ("FILMOWY KLUB SENIORA: OJCZYZNA" → "Filmowy klub
   *  seniora: Ojczyzna"). A fully all-UPPERCASE or all-lowercase segment is
   *  sentence-cased; a partly-shouted segment has only its run(s) of 2+
   *  consecutive all-caps words down-cased ("FEDERICO FELLINI: Ciao a tutti!" →
   *  "Federico Fellini: Ciao a tutti!"), leaving lone acronyms ("UEFA") and the
   *  already-cased words alone (see `recaseShoutedRuns`). "Paris Saint-Germain",
   *  "Moulin Rouge!" are untouched.
   *
   *  IDENTITY-INVARIANT BY CONSTRUCTION: casing must never re-key a row, but
   *  `sanitize` is NOT perfectly casing-blind — the canonical strips include
   *  case-sensitive prefixes (e.g. "Gwiezdne Wojny: " matches that exact casing
   *  but not "GWIEZDNE WOJNY: "), so down-casing a shout COULD make it sanitize to
   *  a different key, scattering the row's merge and spinning the staging fold. So
   *  the re-cased form is only adopted when it sanitizes to the SAME key; otherwise
   *  the original casing is kept (the franchise-prefixed shout stays as scraped). */
  def recase(title: String): String = {
    val recased = effective.leadingBannerBoundary(title) match {
      case Some(n) => caseSegment(title.substring(0, n)) + caseSegment(title.substring(n))
      case None    => caseSegment(title)
    }
    // Fast path: the overwhelming majority of titles are already well-cased, so
    // recasing is a no-op — skip the (relatively costly) identity check entirely.
    // Only a title we actually re-cased pays for the `sanitize` round-trip guard.
    if (recased == title) title
    else if (sanitize(recased) == sanitize(title)) recased
    else title
  }

  private def caseSegment(s: String): String = {
    val letters = s.filter(_.isLetter)
    if (letters.isEmpty) s
    else if (letters.forall(_.isUpper) || letters.forall(_.isLower)) tools.TextNormalization.sentenceCase(s)
    else recaseShoutedRuns(s) // partly-shouted → down-case the shouted run(s)
  }

  // A token made only of roman-numeral letters — kept in caps when a shout is
  // down-cased so "Rocky BALBOA II" cases the name but leaves the sequel ("II").
  private val RomanNumeral = "^[IVXLCDM]+$".r

  private def isAllCapsWord(token: String): Boolean = {
    val ls = token.filter(_.isLetter)
    ls.nonEmpty && ls.forall(_.isUpper)
  }

  /** Display-casing for a MIXED-case segment: when a scraper SHOUTS part of an
   *  otherwise properly-cased title ("FEDERICO FELLINI: Ciao a tutti!"), down-case
   *  the shouted words while leaving the already-cased words byte-identical.
   *
   *  The trigger is a RUN of two or more *consecutive* all-caps words — that's
   *  what tells a shout ("FEDERICO FELLINI", "GWIEZDNE WOJNY: MANDALORIAN") apart
   *  from a lone acronym/initialism that must stay ("Liga Mistrzów UEFA",
   *  "NT Live"). Once a segment is found to be shouting, EVERY all-caps word in it
   *  is down-cased — including ones a lowercase connective stranded out of the run
   *  ("…MANDALORIAN i GROGU" → "…Mandalorian i Grogu", not a half-shouted
   *  "…Mandalorian i GROGU" that would also key as a brand-new spelling and
   *  churn the staging fold). Multi-letter roman numerals keep their caps
   *  ("BALBOA II" → "Balboa II"). */
  private def recaseShoutedRuns(s: String): String = {
    // Alternating whitespace / non-whitespace tokens, preserved exactly so an
    // untouched input round-trips byte-identical.
    val tokens    = "\\s+|\\S+".r.findAllIn(s).toVector
    val capsWords = tokens.indices.filter(i => isAllCapsWord(tokens(i)))
    // A shout = at least one ADJACENT pair of all-caps words. Tokens strictly
    // alternate whitespace/non-whitespace, so two consecutive caps words sit
    // exactly two indices apart (one whitespace token between them).
    val shouting  = capsWords.sliding(2).exists { case Seq(a, b) => b - a == 2; case _ => false }
    if (!shouting) s
    else tokens.zipWithIndex.map {
      case (t, i) if isAllCapsWord(t) && RomanNumeral.findFirstIn(t.filter(_.isLetter)).isEmpty =>
        tools.TextNormalization.titleCaseIfAllCaps(t)
      case (t, _) => t
    }.mkString
  }

  /** When `title` opens with a recognised programme prefix (Kino bez barier,
   *  Filmowy Klub Seniora, …), return the matched prefix INCLUDING the trailing
   *  ": " delimiter, so a caller can split the prefix from the film title and
   *  case each half on its own. None when no programme prefix is present. */
  def programmePrefix(title: String): Option[String] = effective.programmePrefix(title)

  // Cross-cinema spelling unifications (Gwiezdne Wojny prefix, " & " → " i ")
  // over the trimmed title. Does NOT apply `searchTitle`/structural: decoration
  // (anniversary / wersja / slash / Cykl / restored) is NOT part of identity, so
  // a decoration edition keys by its own form and is NOT merged with the base
  // film. Used by `sanitize` (the documentId) and `preferredDisplay`.
  private def canonical(t: String): String = effective.canonical(t)

  // Last-resort collapse for titles that share words + order but differ only
  // in punctuation/whitespace ("Top Gun Maverick" vs "Top Gun: Maverick").
  // Lowercased, accents stripped, every non-alphanumeric char dropped. Used
  // by `mergeKeyLookup` ONLY when at least two distinct corpus titles reduce
  // to the same form — so it never collapses a standalone film into siblings
  // that merely share a prefix.
  private def stripPunct(t: String): String = {
    val deburred = CombiningMarks.matcher(
      java.text.Normalizer.normalize(t, java.text.Normalizer.Form.NFD)
    ).replaceAll("").toLowerCase(Locale.ROOT)
    NonAlnumAscii.matcher(deburred).replaceAll("")
  }

  /** Corpus-independent stable key — the same collapse as `mergeKeyLookup`'s
   *  most-aggressive tier (`stripPunct` of `canonical`), applied
   *  unconditionally rather than gated on a sibling reducing to the same
   *  form. Used as the persistent documentId in `MovieRepository`/`MovieCache`
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
    // Memoised because `sanitize` is the hottest normaliser — called per movie ×
    // per corpus row inside `MovieCache`'s scrape scans (`concludedKeyFor`,
    // `redirectToExistingVariant`, the per-tick index rebuilds) and every staging /
    // projection key. The inner `canonical` fold is already cached per-`TitleRuleSet`,
    // but the outer NFD-normalise + deburr + Unicode `replaceAll` ran uncached on
    // every call. The cache is keyed on the raw title and scoped to the GLOBAL
    // `active` rule set: cleared whenever `active` swaps (`installRules` /
    // `resetToDefaults`), and BYPASSED under a thread-local `withRules` override
    // (transient + rare — tests / admin preview) so a scoped set never poisons the
    // shared cache nor reads a globally-cached value.
    if (scopedOverride.get() != null) computeSanitize(title)
    else sanitizeCache.computeIfAbsent(title, computeSanitize)

  private val sanitizeCache = new ConcurrentHashMap[String, String]()

  private val computeSanitize: java.util.function.Function[String, String] = title =>
    NonAlnumUnicode.matcher(
      tools.TextNormalization.deburr(normalize(canonical(title))).toLowerCase(Locale.ROOT)
    ).replaceAll("")

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

  /** The deterministic display-title ladder used by the live merge
   *  (`MovieRecord.displayTitle`): from the per-cinema cleaned spellings of one
   *  merged row, pick the form to show (no scrape-order dependence).
   *
   *   A. **Dominant identity** — group the spellings by `sanitize` key and take
   *      the key the most cinemas agree on (ties → lexicographically-smallest),
   *      dropping minority misspellings + cross-script variants.
   *   B. **TMDB Polish title** — when supplied and it shares that key and is
   *      `wellFormedTitle`, prefer it (canonical casing / diacritics / punct).
   *   C. **Cinema ladder** — otherwise pick among the dominant-identity
   *      spellings via `preferredDisplay`.
   *
   *  The winner is finally `recase`d. `fallback` is the anchor used when there
   *  are no spellings (a TMDB-only row in the live merge) and the last resort if
   *  the ladder empties; callers pass the row's clean key / search title. */
  def chooseDisplay(perCinemaTitles: Seq[String], fallback: String,
                    tmdbTitle: Option[String] = None): String = {
    val votePool    = if (perCinemaTitles.nonEmpty) perCinemaTitles else Seq(fallback)
    val dominantKey = votePool.groupBy(sanitize).toSeq.sortBy { case (k, ts) => (-ts.size, k) }.head._1
    val chosen = tmdbTitle
      .filter(t => sanitize(t) == dominantKey && wellFormedTitle(t))
      .getOrElse {
        val variants = (perCinemaTitles :+ fallback).filter(t => sanitize(t) == dominantKey)
        preferredDisplay(variants).getOrElse(fallback)
      }
    recase(chosen)
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
