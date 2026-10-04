package services.titlerules

import models.Country

import java.util.concurrent.ConcurrentHashMap

/** An immutable, compiled snapshot of all title rules, grouped by tier and
 *  pre-sorted so the hot normalisation path is a fold over a small list of
 *  pre-compiled regexes. `TitleNormalizer` holds one of these in a swappable
 *  `@volatile` slot, replaced wholesale when the change stream reports an edit.
 *
 *  Tier composition:
 *  {{{
 *    apiQuery(t)    = structural(t)           // decoration + programme/access/event strips — EXTERNAL LOOKUPS ONLY
 *    canonical(t)   = canonicalRules(t.trim)  // Gwiezdne Wojny / & → i — IDENTITY + display
 *  }}}
 *  `canonical` deliberately does NOT apply `structural`: identity (`sanitize`)
 *  and display key a title by its OWN form, so a decoration/programme edition
 *  ("Top Gun / 40th Anniversary", "Kino bez barier: …") stays a separate row
 *  from the base film rather than folding into it. The structural strip survives
 *  only for the upstream-lookup tier (`apiQuery`); the `programmePrefix` rules in
 *  it additionally locate banner boundaries for display casing
 *  (`leadingBannerBoundary`).
 */
case class TitleRuleSet(rules: Seq[TitleRule], placeholders: Map[String, String] = TitleRulePlaceholders.all) {
  import RuleScope._

  // Each raw rule paired with its placeholder-expanded form and whether a
  // `{{NAME}}` token survived expansion (an unknown placeholder or a cycle). An
  // unresolved rule is forced disabled so it's a genuine no-op in the fold — we
  // do NOT let a literal `{{NAME}}` reach the regex engine — and is reported by
  // `invalidRules` so the typo shows up in the editor.
  private val expansions: Seq[(TitleRule, TitleRule)] =
    if (placeholders.isEmpty) rules.map(r => (r, r))
    else rules.map { raw =>
      val expandedPattern = PlaceholderExpander.expand(raw.pattern, placeholders)
      val unresolved      = PlaceholderExpander.containsToken(expandedPattern)
      (raw, raw.copy(pattern = expandedPattern, enabled = raw.enabled && !unresolved))
    }

  /** Rules with their `{{NAME}}` placeholder tokens expanded — what actually
   *  compiles and matches. Identity when there are no placeholders, so a set
   *  built without them behaves exactly as before. The raw `rules` (carrying the
   *  tokens) are preserved for storage and the editor; only the regex sees the
   *  expansion. */
  val effectiveRules: Seq[TitleRule] = expansions.map(_._2)

  // `last` rules fold after the non-last ones of the same scope/cinema:
  // `false < true`, so the tuple sort puts them at the end of the tier.
  private def ruleOrder(r: TitleRule): (Boolean, Int, String) = (r.last, r.order, r.id)

  private def tier(scope: RuleScope): Seq[TitleRule] =
    effectiveRules.iterator.filter(_.scope == scope).toSeq.sortBy(ruleOrder)

  private val structuralRules = tier(GlobalStructural)
  private val canonicalRules  = tier(Canonical)
  private val spellingRules   = canonicalRules.filter(_.replacement.nonEmpty)
  private val perCinemaRules: Map[String, Seq[TitleRule]] =
    effectiveRules.iterator.filter(_.scope == PerCinema).toSeq
      .groupBy(_.cinemaId.getOrElse(""))
      .view.mapValues(_.sortBy(ruleOrder)).toMap

  private def fold(rs: Seq[TitleRule], in: String): String = rs.foldLeft(in)((s, r) => r(s))

  /** The ids of `rs` that CHANGE `in` as the tier folds it, in fold order — what the identity trace records a
   *  title took. Uncached: asked only when a trace is written, never on the pipeline's path. */
  private def fired(rs: Seq[TitleRule], in: String): Seq[String] =
    rs.foldLeft((in, Vector.empty[String])) { case ((acc, ids), r) => val next = r(acc); (next, if (next != acc) ids :+ r.id else ids) }._2
  /** The [[perCinema]] rules that change `raw`. */
  def firedPerCinema(cinemaId: String, raw: String): Seq[String] = perCinemaRules.get(cinemaId).fold(Seq.empty[String])(fired(_, raw))
  /** The [[canonical]] rules that change `t`. */
  def firedCanonical(t: String): Seq[String] = fired(canonicalRules, t.trim)
  /** The [[structural]] (search) rules that change `t`. */
  def firedSearch(t: String): Seq[String] = fired(structuralRules, t)

  // Per-title memo caches. These tier folds are pure functions over an IMMUTABLE
  // rule set, but the pipeline normalises the same ~1k corpus titles millions of
  // times (hydrate → merge → settle → display), and since ExtraTitleRules merged
  // into production the structural tier grew ~5× (≈36 → ≈180 rules), so each
  // uncached fold got proportionally heavier. Memoising collapses the work to one
  // fold per distinct (title) — caching keeps title normalisation off the
  // worker's CPU-credit budget and roughly halved the e2e corpus pipeline.
  //
  // Lifecycle is automatic: the caches live and die with this set, which is
  // immutable, so they always yield stable results, never staleness.
  // ConcurrentHashMap for thread-safety (a TitleNormalizer is shared across a
  // wiring's threads, lock-free).
  private val structuralCache      = new ConcurrentHashMap[String, String]()
  private val canonicalCache       = new ConcurrentHashMap[String, String]()
  private val spellingUnifiedCache = new ConcurrentHashMap[String, String]()
  private val perCinemaCache       = new ConcurrentHashMap[(String, String), String]()
  private val programmePrefixCache = new ConcurrentHashMap[String, Option[String]]()
  private val bannerBoundaryCache  = new ConcurrentHashMap[String, Option[Int]]()

  /** `apiQuery` tier — the full decoration + programme/access/event strip, then
   *  trim. Folded in rule order (former Search strips are numbered to run before
   *  the former structural strips, preserving the legacy composition). */
  def structural(t: String): String =
    structuralCache.computeIfAbsent(t, k => fold(structuralRules, k).trim)

  /** Alias of [[structural]] for `apiQuery`/`search` call sites after the tier merge. */
  def search(t: String): String = structural(t)

  /** `canonical` fold used by `sanitize` / `preferredDisplay` — the
   *  cross-cinema spelling unifications (Gwiezdne Wojny prefix, & → i) over the
   *  trimmed title. Does NOT apply `structural`: decoration (anniversary /
   *  "- wersja X" / slash / Cykl / restored) is NOT part of a film's identity or
   *  displayed title — it only matters for external lookups (`searchTitle` /
   *  `apiQuery`). So two listings merge only when they resolve to the same key
   *  on their own. */
  def canonical(t: String): String =
    canonicalCache.computeIfAbsent(t, k => fold(canonicalRules, k.trim))

  /** The REWRITING half of the canonical tier — the rules that replace one spelling of a
   *  film with another (" & " → " i ", a lower-cased franchise prefix) rather than
   *  deleting a decoration. That distinction is exactly `replacement.nonEmpty`, and it is
   *  the half a DISPLAY title may safely take.
   *
   *  The stripping half must not reach a display title. Those rules exist so a decorated
   *  scrape keys like the bare film — the Fellini retrospective's "Federico Fellini:" /
   *  "ciao a tutti!" family is the reason the tier is load-bearing at all — and a title
   *  made ENTIRELY of such a banner reduces to nothing: applying the whole tier to
   *  display renders "Federico Fellini: ciao a tutti!" as "" and renames its sibling
   *  "Federico Fellini: Ciao a tutti! - Wałkonie …" to plain "Wałkonie …", collapsing a
   *  deliberately separate programme row onto the base film's name. Measured, not
   *  supposed. Identity keeps the full fold; only display takes this subset. */
  def spellingUnified(t: String): String =
    spellingUnifiedCache.computeIfAbsent(t, k => fold(spellingRules, k.trim))

  /** Per-cinema raw → clean cleanup (the old per-client `cleanTitle`). Unknown
   *  cinema → identity. NO implicit trim — clients that trimmed carry an explicit
   *  trim rule, since some legacy clients (Helios, Cinema City, …) deliberately
   *  did NOT trim and preserved trailing whitespace. */
  def perCinema(cinemaId: String, raw: String): String =
    perCinemaRules.get(cinemaId) match {
      // Most venues have no rules of their own, and folding none is the title itself: memoising
      // that kept one (venue, title) → title entry per listing for the process's life — 104,711
      // of them, ~8 MB, on the US worker's live heap (dump 2026-09-29).
      case None        => raw
      case Some(rules) => perCinemaCache.computeIfAbsent((cinemaId, raw), k => fold(rules, k._2))
    }

  /** How many per-cinema folds are memoised — only venues WITH rules of their own may add one. */
  private[titlerules] def perCinemaCached: Int = perCinemaCache.size

  /** The programme-prefix banner at the start of `title`, including the trailing
   *  ": " delimiter, when one of the `tag = "programmePrefix"` rules matches at
   *  the start. None otherwise. The tagged subset of [[leadingBannerBoundary]]. */
  def programmePrefix(title: String): Option[String] =
    programmePrefixCache.computeIfAbsent(title, k =>
      structuralRules.iterator
        .filter(_.tag.contains("programmePrefix"))
        .flatMap(r => r.compiled.flatMap(_.findPrefixMatchOf(k)).map(_.matched))
        .find(_.nonEmpty))

  /** The end offset of the longest leading banner on `title` matched by ANY
   *  enabled `^`-anchored rule in the lookup tier (programme prefixes, the Cykl
   *  banner, …). Drives [[services.movies.TitleNormalizer.recase]], which splits
   *  there and cases the banner and the film independently — generalising the
   *  old Rialto-only, single-prefix casing to every prefix rule. `None` when no
   *  prefix rule matches at the start. */
  def leadingBannerBoundary(title: String): Option[Int] =
    bannerBoundaryCache.computeIfAbsent(title, k =>
      structuralRules.iterator
        .filter(r => r.enabled && r.isPrefixAnchored)
        .flatMap(r => r.compiled.flatMap(_.findPrefixMatchOf(k)))
        .map(_.end)
        .filter(_ > 0)
        .maxOption)

  /** Cinema ids that have at least one per-cinema rule — used by the backfill to
   *  scope which records to re-key after a per-cinema edit. */
  def cinemasWithRules: Set[String] = perCinemaRules.keySet

  /** Patterns that failed to compile OR carry an unresolved `{{NAME}}` token —
   *  surfaced to the editor so a typo can't silently no-op. Validity is judged on
   *  the EXPANDED pattern (a raw `{{SEP}}…` doesn't compile until its placeholder
   *  is substituted), but the RAW rule is returned so the editor shows the
   *  `{{SEP}}` the author typed. */
  def invalidRules: Seq[TitleRule] =
    expansions.collect {
      case (raw, effective)
        if !effective.patternValid || PlaceholderExpander.containsToken(effective.pattern) => raw
    }
}

object TitleRuleSet {

  /** The in-code rule set as it applies to ONE country — the full seed minus
   *  every rule that declares a different language's countries. Each process
   *  serves a single country (`KINOWO_COUNTRY`), so the filter happens once at
   *  load rather than per-call on the hot normalisation path. */
  def forCountry(country: Country): TitleRuleSet =
    TitleRuleSet((TitleRules.all ++ ExtraTitleRules.all).filter(_.appliesTo(country)))

  val empty: TitleRuleSet = TitleRuleSet(Nil)
}
