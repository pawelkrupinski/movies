package scripts

import services.titlerules.RuleScope._
import services.titlerules.TitleRule

/** Post-baseline title-rule ADDITIONS, curated from a 2026-06 audit of the prod
 *  `movies` corpus (the senior / DKF / Konesera / przedpremiera long tail that
 *  fragments one film across 15–25 rows, none of which enrich because the banner
 *  isn't stripped for the external-API query).
 *
 *  These are deliberately NOT in [[services.titlerules.TitleRuleDefaults]]: that
 *  set is the FROZEN migration baseline `TitleRuleMigrationSpec` /
 *  `ProdTitlesNormalizationSpec` pin byte-for-byte against the pre-rules code, so
 *  a behaviour-changing rule can't live there. New normalisation is operational
 *  data in the `titleRules` collection. This object is the version-controlled,
 *  regression-tested (`ExtraTitleRulesSpec`) source for those records;
 *  [[ApplyExtraTitleRules]] merges them into prod, where the worker's change-stream
 *  backfill (`NormalizationRebuilder` + `reEnrichSearchChanges`) re-keys and
 *  re-enriches the affected rows.
 *
 *  Three tiers, matching the existing pipeline:
 *   - `programmePrefixes` — Search scope, `tag = programmePrefix`: a banner the
 *     display EXTRACTS to its own row (the screening stays distinct) but the
 *     external-API query STRIPS, so "Klub Konesera: Ojczyzna" finally resolves
 *     ratings/poster as "Ojczyzna".
 *   - `searchStrips` — Search scope, no tag: stripped for enrichment only; the
 *     row is kept (przedpremiera screenings, DKF-suffix forms, the `*AD` tag).
 *
 *  Decoration MERGES (Tani wtorek: / `/Kino Cafe` collapsing into the base film)
 *  were intentionally left out. They'd be GlobalStructural rules, and a
 *  GlobalStructural (merge-key-changing) rule added AFTER the docs exist can't be
 *  back-filled by the change-stream rebuilder: `CacheKey` equality is by
 *  `sanitize` (which runs the GlobalStructural tier), so the stale "X/Kino Cafe"
 *  and the base "X" collide on cache hydration — Caffeine last-write-wins drops
 *  one before `NormalizationRebuilder.rebuild` ever sees two entries to union.
 *  PerCinema rules don't hit this (sanitize
 *  ignores that tier), which is why the seed's per-cinema migrations back-fill
 *  fine. Decoration merges wait on a rebuild union-on-collision fix (or
 *  per-cinema scoping), tracked with the language-version format extraction. */
object ExtraTitleRules {

  private def prog(id: String, pattern: String, note: String): TitleRule =
    TitleRule(id, Search, None, pattern, "", applyAll = false, order = 0,
      tag = Some("programmePrefix"), note = Some(note))

  private def searchStrip(id: String, pattern: String, note: String): TitleRule =
    TitleRule(id, Search, None, pattern, "", applyAll = false, order = 0, note = Some(note))

  /** Programme banners not in the seed alternation. Each anchored at `^` and
   *  ending in its delimiter (`: `, ` | `) so it's a true prefix the extractor
   *  can split off. `[^:]+` variants absorb the cycle's sub-name (DKF Kropka,
   *  Klub Filmowy Urania, …) in one rule. */
  val programmePrefixes: Seq[TitleRule] = Seq(
    prog("xtra-pp-konesera",          """(?i)^(?:Klub|Kino)\s+Konesera:\s+""",            "Klub/Kino Konesera cycle"),
    prog("xtra-pp-kino-seniora-pipe", """(?i)^KINO\s+SENIORA\s*\|\s*""",                  "KINO SENIORA | senior banner"),
    prog("xtra-pp-pora-dla-seniora",  """(?i)^Pora\s+dla\s+Seniora:\s+""",                "Pora dla Seniora cycle"),
    prog("xtra-pp-wtorki-seniora",    """(?i)^Wtorki\s+dla\s+Seniora:\s+""",              "Wtorki dla Seniora cycle"),
    prog("xtra-pp-kino-dla-seniora",  """(?i)^Kino\s+dla\s+Seniora:\s+""",                "Kino dla Seniora cycle"),
    prog("xtra-pp-spotkania-seniora", """(?i)^Filmowe\s+spotkania\s+seniora:\s+""",       "Filmowe spotkania seniora"),
    prog("xtra-pp-fks-seniorki",      """(?i)^Filmowy\s+Klub\s+Seniora\s+i\s+Seniorki:\s+""", "Filmowy Klub Seniora i Seniorki"),
    prog("xtra-pp-dkf-named",         """(?i)^DKF\s+[^:]+:\s+""",                         "DKF <name>: film-club prefix"),
    prog("xtra-pp-klub-filmowy",      """(?i)^Klub\s+Filmowy\s+[^:]+:\s+""",              "Klub Filmowy <name> cycle"),
    prog("xtra-pp-modowy-klub",       """(?i)^Modowy\s+klub\s+filmowy:\s+""",             "Modowy klub filmowy"),
    prog("xtra-pp-klub-dlr",          """(?i)^Klub\s+DLR:\s+""",                          "Klub DLR"),
    prog("xtra-pp-kino-dostepne",     """(?i)^Kino\s+Dostępne:\s+""",                     "Kino Dostępne accessibility cycle"),
    prog("xtra-pp-psychoanaliza",     """(?i)^Kino\s+[ai]\s+psychoanaliz[ae]:\s+""",      "Kino a/i psychoanaliza"),
    prog("xtra-pp-psychologia",       """(?i)^Filmowe\s+spotkania\s+z\s+psycholog(?:ią|ia):\s+""", "Filmowe spotkania z psychologią"),
    prog("xtra-pp-filmoterapia",      """(?i)^Filmoterapia\s+z\s+Inspirą:\s+""",          "Filmoterapia z Inspirą"),
    prog("xtra-pp-portret-kobiety",   """(?i)^Portret\s+Kobiety:\s+""",                   "Portret Kobiety cycle"),
    prog("xtra-pp-best-film-on-tour", """(?i)^Best\s+Film\s+on\s+Tour\s*[:|]\s*""",       "Best Film on Tour")
  )

  /** Strips that fix enrichment without merging the row away — a premiere or a
   *  DKF screening keeps its own line, it just resolves ratings now. */
  val searchStrips: Seq[TitleRule] = Seq(
    searchStrip("xtra-dkf-suffix-pipe-underscore", """(?i)\s*[|_]\s*DKF\b.*$""",                      "'| DKF' / '_DKF' suffix"),
    searchStrip("xtra-dkf-suffix-dash",            """(?i)\s*[-–—]\s*DKF\b.*$""",                     "'- DKF KOT' / '- DKF III W' suffix"),
    searchStrip("xtra-dyskusyjny-suffix",          """(?i)\s*[-–—]\s*dyskusyjny\s+klub\s+filmowy\s*$""", "'- dyskusyjny klub filmowy' suffix"),
    searchStrip("xtra-przedpremiera-suffix",       """(?i)\s*[|–—-]\s*(?:przedpremiera|przedpremierowo|zobacz\s+przedpremierowo|seans\s+przedpremierowy)\s*$""", "przedpremiera suffix"),
    searchStrip("xtra-przedpremiera-prefix",       """(?i)^(?:przedpremiera|seans\s+przedpremierowy)\s*[.|:]\s*""", "przedpremiera prefix"),
    searchStrip("xtra-accessibility-star-ad",      """(?i)\s*\*\s*AD\b\s*$""",                        "'*AD' audio-description tag"),
    // Festival / retrospective banners that pipe-wrap the film (Kinoteka) or
    // brand it with the touring "Federico Fellini: ciao a tutti!" cycle (~12
    // cinemas, inconsistent shapes). Query-only strips so each screening keeps
    // its own decorated row but finally resolves the bare film.
    searchStrip("xtra-wtf-fest-prefix",            """(?i)^WTF\s+Fest\s*\|\s*""",                     "'WTF Fest | <film>' banner (film after the pipe)"),
    searchStrip("xtra-pipe-festival-suffix",       """(?i)\s*\|\s*(?:6\s+razy\s+Pedro|Kino\s+cyrkularne)\b.*$""", "'<film> | 6 razy Pedro / Kino cyrkularne …' (film before the pipe)"),
    searchStrip("xtra-fellini-prefix",             """(?i)^Federico\s+Fellini\s*:\s*(?:ciao\s+a?\s*tutti\s*!?)?\s*[:\-–—]?\s*""", "'Federico Fellini: ciao a tutti! …' retrospective prefix"),
    searchStrip("xtra-fellini-suffix",             """(?i)\s*(?:\|\s*|[–—-]\s*przegl[ąa]d\s+)Federico\s+Fellini\b.*$""", "'… | / – przegląd FEDERICO FELLINI …' retrospective suffix")
  )

  /** Orders stamped by position so the additions fold AFTER the seed rules of
   *  their scope (matching how [[ApplyExtraTitleRules]] appends them to the
   *  existing record). */
  val all: Seq[TitleRule] =
    (programmePrefixes ++ searchStrips).zipWithIndex.map {
      case (r, i) => r.copy(order = 100 + i)
    }
}
