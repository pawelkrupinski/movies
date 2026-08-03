package services.movies

import models.Country
import services.titlerules.TitleRuleSet

import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern

/**
 * Title normalisation under ONE country's rule set.
 *
 * An INSTANCE, not a process-global, because the rule set is country-specific:
 * the canonical " & " → " i " unification is Polish ("i" = "and"), and applying
 * it to a German listing stored CinemaxX Würzburg's "Minions & Monster" as
 * "Minions i Monster" — a key no German cinema slot can ever produce. A process
 * serving several countries cannot pick one global set, and the thread-scoped
 * override that used to stand in for per-country scoping could never reach the
 * hot paths: a [[CacheKey]] normalises inside its own constructor, Mongo
 * change-stream callbacks run on driver threads, and the rating enrichers fan
 * out through `BoundedParallel`'s shared executor. Passing the normalizer as a
 * dependency is the only scoping that survives all three.
 *
 * The memo cache lives HERE rather than on the companion for the same reason:
 * keyed on the raw title alone, one shared cache would hand a German title the
 * key Poland's rules computed for it.
 *
 * Rule-INDEPENDENT helpers (Roman-numeral folding, script detection, the
 * well-formedness check) stay on the companion — they are pure string functions
 * that no country can disagree about.
 */
class TitleNormalizer(val rules: TitleRuleSet) {

  /** Apply a cinema's per-cinema cleanup rules to a raw scraped title. */
  def cinemaClean(cinemaId: String, raw: String): String = rules.perCinema(cinemaId, raw)

  // ── Cinema-decoration stripping ────────────────────────────────────────────
  //
  // The patterns live in the `TitleRuleSet` (seeded from `TitleRules`, editable
  // in Mongo via the admin page). The tiers:
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
  def apiQuery(display: String): String = rules.search(display)

  /** The external-search form of a title: `apiQuery` (decoration strip) over the
   *  banner-aware re-cased title, so the query a resolver sends is normalised
   *  regardless of how a cinema spelled it (ALL-CAPS, all-lower, mixed). A pure
   *  function of its input — no scrape-order dependence — and, applied to a row's
   *  canonical `cleanTitle`, it reproduces the cased query the per-client casing
   *  used to produce (now that cinema slots keep their raw spelling).
   *
   *  Lives here rather than on `MovieService` because it composes two of THIS
   *  instance's rules; a caller holding the normalizer should not have to route
   *  through the enrichment service to combine them. */
  def searchQuery(title: String): String = apiQuery(recase(title))

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
    val recased = rules.leadingBannerBoundary(title) match {
      case Some(n) => TitleNormalizer.caseSegment(title.substring(0, n)) +
                      TitleNormalizer.caseSegment(title.substring(n))
      case None    => TitleNormalizer.caseSegment(title)
    }
    // Fast path: the overwhelming majority of titles are already well-cased, so
    // recasing is a no-op — skip the (relatively costly) identity check entirely.
    // Only a title we actually re-cased pays for the `sanitize` round-trip guard.
    if (recased == title) title
    else if (sanitize(recased) == sanitize(title)) recased
    else title
  }

  /** When `title` opens with a recognised programme prefix (Kino bez barier,
   *  Filmowy Klub Seniora, …), return the matched prefix INCLUDING the trailing
   *  ": " delimiter, so a caller can split the prefix from the film title and
   *  case each half on its own. None when no programme prefix is present. */
  def programmePrefix(title: String): Option[String] = rules.programmePrefix(title)

  // Cross-cinema spelling unifications (Gwiezdne Wojny prefix, " & " → " i ")
  // over the trimmed title. Does NOT apply `searchTitle`/structural: decoration
  // (anniversary / wersja / slash / Cykl / restored) is NOT part of identity, so
  // a decoration edition keys by its own form and is NOT merged with the base
  // film. Used by `sanitize` (the documentId) and `preferredDisplay`.
  private def canonical(t: String): String = rules.canonical(t)

  // Memoised because `sanitize` is the hottest normaliser — called per movie ×
  // per corpus row inside `MovieCache`'s scrape scans (`concludedKeyFor`,
  // `redirectToExistingVariant`, the per-tick index rebuilds) and every staging /
  // projection key. The inner `canonical` fold is already cached per-`TitleRuleSet`,
  // but the outer NFD-normalise + deburr + Unicode `replaceAll` ran uncached on
  // every call. Keyed on the raw title alone, which is only safe because the cache
  // belongs to ONE rule set: the instance owns it, so two countries can never read
  // each other's keys and no swap has to invalidate anything.
  private val sanitizeCache = new ConcurrentHashMap[String, String]()

  // Canonicalise, but never all the way to NOTHING. The Canonical tier is a set of
  // `^`-anchored banner rules, and a cinema can list a film whose title is nothing BUT
  // the banner — Kino Muza's "Federico Fellini: ciao a tutti!" is a programme name that
  // is also the whole listing. That sanitized to "", so the film's identity became the
  // bare year: `_id = "|1957"`, with its screening row keyed `"|1957|krakow|Kino Agrafka"`
  // behind it. Identity is the one thing that cannot be empty — every wholly-banner film
  // in a given year lands on that same `_id` and the later write replaces the earlier one.
  // Falling back to the RAW title's key keeps the identity a pure function of what the
  // cinemas reported, and still deburrs/lower-cases/strips, so the case and punctuation
  // variants that should be one film remain one film.
  private val computeSanitize: java.util.function.Function[String, String] = title => {
    val canonicalised = TitleNormalizer.strippedKey(canonical(title))
    if (canonicalised.nonEmpty) canonicalised else TitleNormalizer.strippedKey(title)
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
  def sanitize(title: String): String = sanitizeCache.computeIfAbsent(title, computeSanitize)

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
    val romanized = allTitles.iterator.map(TitleNormalizer.normalize).toSet
    val canonicalCounts: Map[String, Int] =
      romanized.iterator.map(t => canonical(t).toLowerCase(Locale.ROOT)).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    // Punctuation-stripped counts — for cases where two titles share words +
    // word order but differ only in : / - / whitespace. Built on top of
    // canonical so this also catches "Mandalorian & Grogu" ≡ "Mandalorian i
    // Grogu" when they additionally lose their colon.
    val puncStripCounts: Map[String, Int] =
      romanized.iterator.map(t => TitleNormalizer.stripPunct(canonical(t))).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    title => {
      val r       = TitleNormalizer.normalize(title)
      val cLower  = canonical(r).toLowerCase(Locale.ROOT)
      val rLower  = r.toLowerCase(Locale.ROOT)
      val p       = TitleNormalizer.stripPunct(cLower)
      // Punctuation-strip is the widest collapse — check first. Only fires
      // when ≥2 distinct corpus titles reduce to the same form, so a lone
      // film never gets a key derived from punctuation it didn't share.
      if (p.nonEmpty && puncStripCounts.getOrElse(p, 0) > 1) p
      else if (cLower != rLower && canonicalCounts.getOrElse(cLower, 0) > 1) cLower
      else rLower
    }
  }

  // Among a group of titles that merge to one schedule, pick the display form —
  // spelling-unified, so " & " shows as " i " however the cinemas spelled it. That way
  // "Mandalorian i Grogu" wins over "Mandalorian & Grogu" even when no cinema shipped the
  // "i" form.
  //
  // The unification applies to a group of ONE as well. It used to be skipped there, on
  // the reasoning that a standalone name had not triggered a merge — but the pool size is
  // a property of who is asking, not of the film. The settle offers one variant (the
  // cinema's spelling) and got "Arnie & barney"; a hydrate offers two and got
  // "Arnie i barney". Neither is persisted, so the settle rewrote those rows after every
  // boot until the two agreed.
  //
  // The two halves of the canonical tier are applied on different terms, because they do
  // different things. A REWRITE (" & " → " i ") swaps one spelling of a film for another
  // and is safe on any group, merge or not. A STRIP (a franchise or banner prefix, a year
  // suffix) DELETES information, and only earns that when a merge actually happened — a
  // standalone "Gwiezdne Wojny: A New Hope" that no other spelling joined would otherwise
  // display as "A New Hope", losing the only name a cinema ever gave it.
  //
  // So: rewrites always, strips only for a genuine merge (the ladder branch below, which
  // is what a merged group has always done). Neither may empty a title outright, which the
  // banner strips do to a listing that is nothing BUT a banner.
  def preferredDisplay(titles: Iterable[String]): Option[String] = {
    val seq = titles.iterator.toSeq.distinct
    if (seq.sizeIs <= 1) seq.headOption.map(unifySpelling)
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
      val canonicals = seq.map(canonicalForDisplay).distinct
      if (canonicals.sizeIs == 1) canonicals.headOption
      else canonicals.sortBy(TitleNormalizer.displayLadderKey).headOption
    }
  }

  /** Full canonical fold for a MERGED group's display, never to nothing: a listing that is
   *  nothing but a banner ("Federico Fellini: ciao a tutti!") reduces to "" under the
   *  strips, which would leave a film with no name at all. Raw wins that argument. */
  private def canonicalForDisplay(t: String): String = {
    val folded = canonical(t)
    if (folded.trim.nonEmpty) folded else t
  }

  /** The REWRITING half only — safe for a group of one, because it swaps a spelling rather
   *  than deleting a decoration. See [[services.titlerules.TitleRuleSet.spellingUnified]]. */
  private def unifySpelling(t: String): String = {
    val unified = rules.spellingUnified(t)
    if (unified.trim.nonEmpty) unified else t
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
      .filter(t => sanitize(t) == dominantKey && TitleNormalizer.wellFormedTitle(t))
      .getOrElse {
        // The fallback joins the pool only when NO cinema spelling survives the dominant-key
        // filter. It used to be appended unconditionally, and that made the answer depend on
        // WHO ASKED rather than on the film: `preferredDisplay` leaves a one-title group
        // alone but canonicalises a group of two, so a row with a single cinema spelling got
        // the raw title from the settle (pool = that spelling) and the canonicalised title
        // from a hydrate (pool = that spelling + the sanitized `_id` prefix). "Arnie &
        // barney" one way, "Arnie i barney" the other, neither persisted, so the settle
        // rewrote those rows after every boot. Dropping the synthetic member gives both
        // callers the same pool — the cinema spellings — and the same answer.
        val fromCinemas = perCinemaTitles.filter(t => sanitize(t) == dominantKey)
        val variants    = if (fromCinemas.nonEmpty) fromCinemas
                          else Seq(fallback).filter(t => sanitize(t) == dominantKey)
        preferredDisplay(variants).getOrElse(fallback)
      }
    recase(chosen)
  }
}

/**
 * Per-country instances, plus the rule-INDEPENDENT half of normalisation: pure
 * string functions (Roman-numeral folding, script detection, casing mechanics,
 * the well-formedness check) that no country's rule set can disagree about, so
 * they stay static rather than being duplicated per instance.
 */
object TitleNormalizer {

  /** The normalizer for `country`, memoised — a [[TitleRuleSet]] compiles ~180
   *  regexes and builds its tier maps at construction, so it is worth holding
   *  one per country rather than one per call site. */
  def forCountry(country: Country): TitleNormalizer =
    byCountry.computeIfAbsent(country, c => new TitleNormalizer(TitleRuleSet.forCountry(c)))

  private val byCountry = new ConcurrentHashMap[Country, TitleNormalizer]()

  /** TRANSITIONAL: the normalizer for the country THIS process serves, resolved
   *  from the environment exactly as the old process-global did.
   *
   *  It exists so the constructor defaults on `MovieRepository`, `MovieCache`,
   *  `StagingRepository` and friends stay behaviour-identical while their
   *  composition roots are migrated to pass an instance explicitly. Defaulting
   *  them to `forCountry(Country.default)` instead would have been a live
   *  regression: `showtimes-de` and `showtimes-uk` resolve `KINOWO_COUNTRY`, so a
   *  Poland default would have started keying their corpora with Polish rules —
   *  the precise fault this refactor removes.
   *
   *  A multi-country worker still gets Poland here, which is why
   *  `WorkerMain.unsupportedCountries` keeps refusing to boot one until the
   *  remaining call sites are injected and this default can be deleted. */
  def deployment: TitleNormalizer =
    forCountry(Country.soleFromEnv.getOrElse(Country.default))

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

  // A token made only of roman-numeral letters — kept in caps when a shout is
  // down-cased so "Rocky BALBOA II" cases the name but leaves the sequel ("II").
  private val RomanNumeral = "^[IVXLCDM]+$".r

  private def isAllCapsWord(token: String): Boolean = {
    val ls = token.filter(_.isLetter)
    ls.nonEmpty && ls.forall(_.isUpper)
  }

  private[movies] def caseSegment(s: String): String = {
    val letters = s.filter(_.isLetter)
    if (letters.isEmpty) s
    else if (letters.forall(_.isUpper) || letters.forall(_.isLower)) tools.TextNormalization.sentenceCase(s)
    else recaseShoutedRuns(s) // partly-shouted → down-case the shouted run(s)
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

  // Last-resort collapse for titles that share words + order but differ only
  // in punctuation/whitespace ("Top Gun Maverick" vs "Top Gun: Maverick").
  // Lowercased, accents stripped, every non-alphanumeric char dropped. Used
  // by `mergeKeyLookup` ONLY when at least two distinct corpus titles reduce
  // to the same form — so it never collapses a standalone film into siblings
  // that merely share a prefix.
  private[movies] def stripPunct(t: String): String = {
    val deburred = CombiningMarks.matcher(
      java.text.Normalizer.normalize(t, java.text.Normalizer.Form.NFD)
    ).replaceAll("").toLowerCase(Locale.ROOT)
    NonAlnumAscii.matcher(deburred).replaceAll("")
  }

  private[movies] def strippedKey(t: String): String =
    NonAlnumUnicode.matcher(
      tools.TextNormalization.deburr(normalize(t)).toLowerCase(Locale.ROOT)
    ).replaceAll("")

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
  private[movies] def displayLadderKey(c: String): (Int, Int, Int, Int, Int, String) = {
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

  // ── Transitional process-global facade ─────────────────────────────────────
  //
  // TEMPORARY. Every delegate below resolves the rule set from the environment
  // instead of from the caller, which is exactly the coupling this class exists
  // to remove — a multi-country process has no correct answer here, which is why
  // `WorkerMain.unsupportedCountries` still refuses to boot one. The delegates
  // exist only so the ~65 call sites can migrate to an injected instance in
  // separate commits rather than one unreviewable diff; each one deleted is a
  // call site that now says whose rules it means. Do not add callers.

  private def defaultRules: TitleRuleSet =
    TitleRuleSet.forCountry(Country.soleFromEnv.getOrElse(Country.default))

  @volatile private var active: TitleNormalizer = new TitleNormalizer(defaultRules)

  /** Swap the rule set `deployment` hands out. Sole caller: the country
   *  convergence e2e, which installs one country's rules per run so the
   *  components still defaulting to `deployment` key that country's way. Not
   *  thread-safe by design — it is a whole-run switch, not a scope. */
  def installRules(rs: TitleRuleSet): Unit = active = new TitleNormalizer(rs)

}
