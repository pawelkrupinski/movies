package services.titlerules

/** The tier of the title-normalisation pipeline a [[TitleRule]] participates in.
 *  Each scope maps to one of the transformations `TitleNormalizer` exposes, so
 *  the formerly-hardcoded regexes can be expressed as editable rules without
 *  changing where they apply.
 *
 *  `changesRecord` splits the scopes by whether they rewrite the STORED row.
 *  `PerCinema` (display title + merge key) and `Canonical` (the documentId) both
 *  reshape what's persisted, so an edit there re-keys/merges/splits the corpus.
 *  `GlobalStructural` only rewrites the title used for EXTERNAL LOOKUPS
 *  (`apiQuery`) — the stored row is untouched, so its effect can be previewed
 *  non-destructively (see `TitleRuleSet.transientAffected`). */
sealed trait RuleScope { def name: String; def changesRecord: Boolean }

object RuleScope {
  /** Per-cinema cleanup applied to the raw scraped title before it becomes a
   *  display / merge title (the old per-client `cleanTitle`). Carries a
   *  `cinemaId`; affects both the stored display title and the merge key. */
  case object PerCinema extends RuleScope { val name = "PerCinema"; val changesRecord = true }

  /** The single external-lookup tier (formerly `GlobalStructural` + `Search`):
   *  decoration strips (Cykl prefix, slash postfix, anniversary / restored /
   *  wersja suffixes) AND the programme prefixes, accessibility tags and
   *  "+ <event>" suffixes. Feeds `apiQuery` only. Does NOT affect the merge key
   *  or display: a decoration/programme edition keys by its own form and stays a
   *  separate row (so the base film's TMDB id is still found, but the edition
   *  isn't folded into it). The `tag = "programmePrefix"` rules also drive the
   *  shared banner-aware display casing (`TitleNormalizer.recase`). */
  case object GlobalStructural extends RuleScope { val name = "GlobalStructural"; val changesRecord = false }

  /** Cross-cinema spelling unifications folded into the stable documentId key
   *  (`sanitize` / `preferredDisplay`): "Gwiezdne Wojny:" strip, " & " → " i ". */
  case object Canonical extends RuleScope { val name = "Canonical"; val changesRecord = true }

  val all: Seq[RuleScope] = Seq(PerCinema, GlobalStructural, Canonical)

  /** `"Search"` is the legacy name of rules now folded into `GlobalStructural`;
   *  alias it so the ~30 prod Mongo `scope:"Search"` docs still load. */
  def byName(n: String): Option[RuleScope] = n match {
    case "Search" => Some(GlobalStructural)
    case _        => all.find(_.name == n)
  }
}
