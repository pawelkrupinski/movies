package services.titlerules

/** An immutable, compiled snapshot of all title rules, grouped by tier and
 *  pre-sorted so the hot normalisation path is a fold over a small list of
 *  pre-compiled regexes. `TitleNormalizer` holds one of these in a swappable
 *  `@volatile` slot, replaced wholesale when the change stream reports an edit.
 *
 *  Tier composition:
 *  {{{
 *    searchTitle(t) = structural(t)              // global decoration strip + trim — EXTERNAL LOOKUPS ONLY
 *    apiQuery(t)    = structural(searchRules(t)) // + programme/access/event strips — external lookups
 *    canonical(t)   = canonicalRules(t.trim)     // Gwiezdne Wojny / & → i — IDENTITY + display
 *  }}}
 *  `canonical` deliberately does NOT apply `structural`: identity (`sanitize`)
 *  and display key a title by its OWN form, so a decoration edition
 *  ("Top Gun / 40th Anniversary") stays a separate row from the base film
 *  rather than folding into it. The structural decoration strip survives only
 *  for the upstream-lookup tiers (`searchTitle` / `apiQuery`).
 */
case class TitleRuleSet(rules: Seq[TitleRule]) {
  import RuleScope._

  // `last` rules fold after the non-last ones of the same scope/cinema:
  // `false < true`, so the tuple sort puts them at the end of the tier.
  private def ruleOrder(r: TitleRule): (Boolean, Int, String) = (r.last, r.order, r.id)

  private def tier(scope: RuleScope): Seq[TitleRule] =
    rules.iterator.filter(_.scope == scope).toSeq.sortBy(ruleOrder)

  private val structuralRules = tier(GlobalStructural)
  private val searchRules     = tier(Search)
  private val canonicalRules  = tier(Canonical)
  private val perCinemaRules: Map[String, Seq[TitleRule]] =
    rules.iterator.filter(_.scope == PerCinema).toSeq
      .groupBy(_.cinemaId.getOrElse(""))
      .view.mapValues(_.sortBy(ruleOrder)).toMap

  private def fold(rs: Seq[TitleRule], in: String): String = rs.foldLeft(in)((s, r) => r(s))

  /** `searchTitle` tier — global decoration stripping, then trim. */
  def structural(t: String): String = fold(structuralRules, t).trim

  /** `apiQuery` tier — the search-only strips, then the structural chain. */
  def search(t: String): String = structural(fold(searchRules, t))

  /** `canonical` fold used by `sanitize` / `preferredDisplay` — the
   *  cross-cinema spelling unifications (Gwiezdne Wojny prefix, & → i) over the
   *  trimmed title. Does NOT apply `structural`: decoration (anniversary /
   *  "- wersja X" / slash / Cykl / restored) is NOT part of a film's identity or
   *  displayed title — it only matters for external lookups (`searchTitle` /
   *  `apiQuery`). So two listings merge only when they resolve to the same key
   *  on their own. */
  def canonical(t: String): String = fold(canonicalRules, t.trim)

  /** Per-cinema raw → clean cleanup (the old per-client `cleanTitle`). Unknown
   *  cinema → identity. NO implicit trim — clients that trimmed carry an explicit
   *  trim rule, since some legacy clients (Helios, Cinema City, …) deliberately
   *  did NOT trim and preserved trailing whitespace. */
  def perCinema(cinemaId: String, raw: String): String =
    fold(perCinemaRules.getOrElse(cinemaId, Nil), raw)

  /** The programme-prefix banner at the start of `title`, including the trailing
   *  ": " delimiter, when one of the `tag = "programmePrefix"` Search rules
   *  matches at the start. None otherwise. Used by the separate-row casing
   *  logic, which needs to split the banner off rather than strip it. */
  def programmePrefix(title: String): Option[String] =
    searchRules.iterator
      .filter(_.tag.contains("programmePrefix"))
      .flatMap(r => r.compiled.flatMap(_.findPrefixMatchOf(title)).map(_.matched))
      .find(_.nonEmpty)

  /** Cinema ids that have at least one per-cinema rule — used by the backfill to
   *  scope which records to re-key after a per-cinema edit. */
  def cinemasWithRules: Set[String] = perCinemaRules.keySet

  /** For every rule in the tiers that DON'T rewrite the stored record
   *  (`!scope.changesRecord` — `GlobalStructural`, `Search`), the corpus titles
   *  that rule rewrites and what it rewrites them to. Used by the admin editor
   *  to show, per transient rule, an unfoldable list of affected films.
   *
   *  Attribution is positional: each tier is folded in its sorted order and a
   *  change is credited to the rule that produced it — `(original, after)` where
   *  `after` is the title once this rule (and the rules before it in the tier)
   *  have applied. A title the rule leaves untouched is omitted; a rule that
   *  changes nothing in the corpus yields an empty `changes`. The `Search` tier
   *  is folded on its own (the structural pass that follows in `apiQuery` is the
   *  `GlobalStructural` tier's concern), so each rule is credited only its own
   *  effect. Pure — the caller supplies the corpus display titles. */
  def transientAffected(titles: Seq[String]): Seq[TitleRuleSet.RuleAffected] = {
    val distinct = titles.distinct
    RuleScope.all.filterNot(_.changesRecord).flatMap { scope =>
      val tierRules = tier(scope)
      // title → the changes each rule made to it, in fold order (ruleId-keyed).
      val changesByRule: Map[String, Seq[TitleRuleSet.Change]] =
        distinct.flatMap { title =>
          tierRules.foldLeft((title, List.empty[(String, TitleRuleSet.Change)])) {
            case ((acc, out), r) =>
              val next = r(acc)
              (next, if (next != acc) (r.id -> TitleRuleSet.Change(title, next)) :: out else out)
          }._2
        }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
      tierRules.map(r => TitleRuleSet.RuleAffected(r.id, scope, changesByRule.getOrElse(r.id, Nil)))
    }
  }

  /** Patterns that failed to compile — surfaced to the editor so a typo can't
   *  silently no-op. */
  def invalidRules: Seq[TitleRule] = rules.filterNot(_.patternValid)
}

object TitleRuleSet {
  val empty: TitleRuleSet = TitleRuleSet(Nil)

  /** One title a transient rule rewrites: the `original` corpus title and the
   *  `result` once the rule applies. */
  final case class Change(original: String, result: String)

  /** A transient rule's effect on the corpus — the (original → result) pairs it
   *  rewrites, in the corpus order they were folded. Empty when the rule touches
   *  nothing currently in the corpus. */
  final case class RuleAffected(ruleId: String, scope: RuleScope, changes: Seq[Change])
}
