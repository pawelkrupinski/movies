package services.titlerules

/** The tier of the title-normalisation pipeline a [[TitleRule]] participates in.
 *  Each scope maps to one of the transformations `TitleNormalizer` exposes, so
 *  the formerly-hardcoded regexes can be expressed as editable rules without
 *  changing where they apply. */
sealed trait RuleScope { def name: String }

object RuleScope {
  /** Per-cinema cleanup applied to the raw scraped title before it becomes a
   *  display / merge title (the old per-client `cleanTitle`). Carries a
   *  `cinemaId`; affects both the stored display title and the merge key. */
  case object PerCinema extends RuleScope { val name = "PerCinema" }

  /** Global decoration stripping in `searchTitle` (Cykl prefix, slash postfix,
   *  anniversary / restored / wersja suffixes) — feeds the EXTERNAL-LOOKUP tiers
   *  (`searchTitle` / `apiQuery`) only. Does NOT affect the merge key or display:
   *  a decoration edition keys by its own form and stays a separate row (so the
   *  base film's TMDB id is still found, but the edition isn't folded into it). */
  case object GlobalStructural extends RuleScope { val name = "GlobalStructural" }

  /** Extra stripping applied only for external-API queries (`apiQuery`):
   *  programme prefixes, accessibility tags, "+ <event>" suffixes. Does NOT
   *  affect the merge key — the programme/event screening keeps its own row. */
  case object Search extends RuleScope { val name = "Search" }

  /** Cross-cinema spelling unifications folded into the stable docId key
   *  (`sanitize` / `preferredDisplay`): "Gwiezdne Wojny:" strip, " & " → " i ". */
  case object Canonical extends RuleScope { val name = "Canonical" }

  val all: Seq[RuleScope] = Seq(PerCinema, GlobalStructural, Search, Canonical)
  def byName(n: String): Option[RuleScope] = all.find(_.name == n)
}
