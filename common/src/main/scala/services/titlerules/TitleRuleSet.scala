package services.titlerules

/** An immutable, compiled snapshot of all title rules, grouped by tier and
 *  pre-sorted so the hot normalisation path is a fold over a small list of
 *  pre-compiled regexes. `TitleNormalizer` holds one of these in a swappable
 *  `@volatile` slot, replaced wholesale when the change stream reports an edit.
 *
 *  The tier composition mirrors the legacy `TitleNormalizer` exactly:
 *  {{{
 *    searchTitle(t) = structural(t)                  // global decoration + trim
 *    apiQuery(t)    = structural(searchRules(t))      // + programme/access/event strips
 *    canonical(t)   = canonicalRules(structural(t))   // + Gwiezdne Wojny / & → i
 *  }}}
 */
case class TitleRuleSet(rules: Seq[TitleRule]) {
  import RuleScope._

  private def tier(scope: RuleScope): Seq[TitleRule] =
    rules.iterator.filter(_.scope == scope).toSeq.sortBy(r => (r.order, r.id))

  private val structuralRules = tier(GlobalStructural)
  private val searchRules     = tier(Search)
  private val canonicalRules  = tier(Canonical)
  private val perCinemaRules: Map[String, Seq[TitleRule]] =
    rules.iterator.filter(_.scope == PerCinema).toSeq
      .groupBy(_.cinemaId.getOrElse(""))
      .view.mapValues(_.sortBy(r => (r.order, r.id))).toMap

  private def fold(rs: Seq[TitleRule], in: String): String = rs.foldLeft(in)((s, r) => r(s))

  /** `searchTitle` tier — global decoration stripping, then trim. */
  def structural(t: String): String = fold(structuralRules, t).trim

  /** `apiQuery` tier — the search-only strips, then the structural chain. */
  def search(t: String): String = structural(fold(searchRules, t))

  /** `canonical` fold used by `sanitize` / `preferredDisplay` — structural,
   *  then the cross-cinema spelling unifications. */
  def canonical(t: String): String = fold(canonicalRules, structural(t))

  /** Per-cinema raw → clean cleanup (the old per-client `cleanTitle`). Unknown
   *  cinema → identity. Trailing trim matches the legacy clients. */
  def perCinema(cinemaId: String, raw: String): String =
    fold(perCinemaRules.getOrElse(cinemaId, Nil), raw).trim

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

  /** Patterns that failed to compile — surfaced to the editor so a typo can't
   *  silently no-op. */
  def invalidRules: Seq[TitleRule] = rules.filterNot(_.patternValid)
}

object TitleRuleSet {
  val empty: TitleRuleSet = TitleRuleSet(Nil)
}
