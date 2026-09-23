package services.movies

import models.Country
import services.titlerules.TitleRuleSet

import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

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
 * well-formedness check) live on [[TitleText]] — they are pure string functions
 * that no country can disagree about.
 */
class TitleNormalizer(val rules: TitleRuleSet) {

  /** Apply a cinema's per-cinema cleanup rules to a raw scraped title, after the
   *  shared tidy-up every scraped title needs (see [[TitleText.tidy]]). */
  def cinemaClean(cinemaId: String, raw: String): String =
    rules.perCinema(cinemaId, TitleText.tidy(raw))

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
      case Some(n) => TitleText.caseSegment(title.substring(0, n)) +
                      TitleText.caseSegment(title.substring(n))
      case None    => TitleText.caseSegment(title)
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
  // per corpus row inside `ScrapeLanding`'s scrape scans (`concludedKeyFor`,
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
    // Tidy FIRST: `strippedKey` drops punctuation, so an undecoded `&quot;` would
    // otherwise leave the bare letters "quot" welded into the key — which is how
    // one André Rieu broadcast ended up keyed `andrerieuquotniechzyjemaastrichtquot`
    // beside the four other rows for the same film.
    val tidied        = TitleText.tidy(title)
    val canonicalised = TitleText.strippedKey(canonical(tidied))
    if (canonicalised.nonEmpty) canonicalised else TitleText.strippedKey(tidied)
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
    val romanized = allTitles.iterator.map(TitleText.normalize).toSet
    val canonicalCounts: Map[String, Int] =
      romanized.iterator.map(t => canonical(t).toLowerCase(Locale.ROOT)).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    // Punctuation-stripped counts — for cases where two titles share words +
    // word order but differ only in : / - / whitespace. Built on top of
    // canonical so this also catches "Mandalorian & Grogu" ≡ "Mandalorian i
    // Grogu" when they additionally lose their colon.
    val puncStripCounts: Map[String, Int] =
      romanized.iterator.map(t => TitleText.stripPunct(canonical(t))).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    title => {
      val r       = TitleText.normalize(title)
      val cLower  = canonical(r).toLowerCase(Locale.ROOT)
      val rLower  = r.toLowerCase(Locale.ROOT)
      val p       = TitleText.stripPunct(cLower)
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
      else canonicals.sortBy(TitleText.displayLadderKey).headOption
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
      .filter(t => sanitize(t) == dominantKey && TitleText.wellFormedTitle(t))
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
 * Per-country instances. The rule-INDEPENDENT half of normalisation — the pure
 * string functions no country's rule set can disagree about — is [[TitleText]].
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
  def deployment: TitleNormalizer = rulesFor(Country.ambiguousFromEnv, Country.soleFromEnv)

  /** Pure core of [[deployment]] — the choice, testable without touching process
   *  state. A process configured for SEVERAL countries has no one rule set, so it
   *  is refused rather than silently given Poland's: that silent fallback is what
   *  stored CinemaxX Würzburg's "Minions & Monster" as "Minions i Monster" and
   *  served it to German users under a key no German cinema slot can produce.
   *  Nothing configured is NOT ambiguous — a dev box or a spec gets Poland. */
  private[movies] def rulesFor(ambiguous: List[Country], sole: Option[Country]): TitleNormalizer =
    if (ambiguous.isEmpty) forCountry(sole.getOrElse(Country.default))
    else sys.error(
      s"No sole country: KINOWO_COUNTRIES names ${ambiguous.map(_.code).mkString(", ")} and " +
      "KINOWO_COUNTRY does not disambiguate, so there is no one rule set this process " +
      "can normalise titles under. Pass a TitleNormalizer explicitly instead of relying " +
      "on TitleNormalizer.deployment.")

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
