package services.titlerules

import RuleScope._

/** The seed rule set — every formerly-hardcoded `TitleNormalizer` regex,
 *  transcribed verbatim and tagged with the tier it ran in. This is BOTH:
 *
 *   1. the in-code fallback `TitleNormalizer` installs at class-load, so the
 *      normaliser behaves correctly before Mongo is read and in tests that
 *      don't wire a rules store; and
 *   2. the documents `SeedTitleRules` upserts into an empty `titleRules`
 *      collection.
 *
 *  `TitleRuleMigrationSpec` asserts that running titles through this set
 *  produces byte-identical output to the pre-rules implementation. DO NOT edit a
 *  pattern here to change behaviour — edit the rule in the DB via the admin page;
 *  this set only exists to reproduce the original baseline.
 *
 *  Per-cinema rules (the old per-client `cleanTitle`) are added in a later step;
 *  at this baseline the per-cinema tier is empty and clients still clean inline. */
object TitleRuleDefaults {

  // ── searchTitle tier — global decoration stripping ────────────────────────
  private val structural: Seq[TitleRule] = Seq(
    TitleRule("structural-cykl-prefix", GlobalStructural, None,
      """^Cykl\s+[„"][^„""]*[„""]?\s+[-–—]\s+""", "", applyAll = false, order = 10,
      note = Some("Festival cycle banner: Cykl \"…\" – ")),
    TitleRule("structural-slash-suffix", GlobalStructural, None,
      """\s+/\s+.+$""", "", applyAll = false, order = 20,
      note = Some("Slash postfix: 'Top Gun / 40th Anniversary'")),
    TitleRule("structural-anniversary-suffix", GlobalStructural, None,
      """(?i)\s*[-–—|.]?\s*\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""", "",
      applyAll = false, order = 30, note = Some("Anniversary rerelease suffix")),
    TitleRule("structural-restored-suffix", GlobalStructural, None,
      """(?i)\s*[-–—|.]?\s*\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""", "",
      applyAll = false, order = 40, note = Some("Restored / remastered suffix (4K restored)")),
    TitleRule("structural-wersja-suffix", GlobalStructural, None,
      """(?i)\s*[-–—.]\s+wersja\s+\p{L}+\s*$""", "", applyAll = false, order = 50,
      note = Some("Polish language-version suffix: '- wersja polska'"))
  )

  // ── apiQuery tier — search-only strips (NOT in the merge key) ──────────────
  private val ProgrammePrefixPattern =
    """(?i)^(?:Kino\s+bez\s+barier|""" +
    """Pokaz\s+sensorycznie\s+przyjazny|""" +
    """Filmow[ey]\s+Poran(?:ki|ek)(?:\s+[^:]+)?|""" +
    """Zimowe\s+Poranki(?:\s+[^:]+)?|""" +
    """Poranek\s+dla\s+dzieci|""" +
    """Filmowy\s+Klub\s+Seniora|""" +
    """Dyskusyjny\s+Klub\s+Filmowy|""" +
    """Filmowe\s+spotkania\s+z\s+psychoanaliz[ąa]|""" +
    """Cinema\s+Italia\s+Oggi|""" +
    """Plenerowe\s+Pa[łl]acowe):\s+"""

  private val search: Seq[TitleRule] = Seq(
    TitleRule("search-programme-prefix", Search, None,
      ProgrammePrefixPattern, "", applyAll = false, order = 10,
      tag = Some("programmePrefix"),
      note = Some("Cinema programme banners (Kino bez barier, DKF, Filmowe Poranki…)")),
    TitleRule("search-accessibility-tag", Search, None,
      """(?i)\s*\(\s*AD\b[^)]*\)?\s*$""", "", applyAll = false, order = 20,
      note = Some("Trailing accessibility tag: (AD), (AD + CC + PJM)")),
    TitleRule("search-plus-event-suffix", Search, None,
      """\s+\+\s+\p{L}[^)]*$""", "", applyAll = false, order = 30,
      note = Some("'+ <event>' suffix: '+ spotkanie z producentką'"))
  )

  // ── canonical tier — cross-cinema spelling unifications (sanitize) ─────────
  private val canonical: Seq[TitleRule] = Seq(
    TitleRule("canonical-gwiezdne-wojny", Canonical, None,
      """^Gwiezdne Wojny: """, "", applyAll = false, order = 10,
      note = Some("Strip the 'Gwiezdne Wojny: ' franchise prefix")),
    TitleRule("canonical-ampersand-to-i", Canonical, None,
      """ & """, " i ", applyAll = true, order = 20,
      note = Some("Unify ' & ' and ' i ' spellings"))
  )

  val all: Seq[TitleRule] = structural ++ search ++ canonical

  val ruleSet: TitleRuleSet = TitleRuleSet(all)
}
