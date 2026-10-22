package services.titlerules

import RuleScope._

/** Post-baseline title-rule ADDITIONS: programme-cycle banners, search strips,
 *  canonical unifications, and per-cinema cleanups curated from a 2026-06 audit
 *  of the prod `movies` corpus. Rules in code; no DB or workflow needed.
 *
 *  Regression-tested by `ExtraTitleRulesSpec`.
 *
 *  Three tiers, matching the existing pipeline:
 *   - `programmePrefixes` — GlobalStructural scope, `tag = programmePrefix`: a
 *     banner the display EXTRACTS to its own row (the screening stays distinct)
 *     but the external-API query STRIPS, so "Klub Konesera: Ojczyzna" finally
 *     resolves ratings/poster as "Ojczyzna".
 *   - `searchStrips` — GlobalStructural scope, no tag: stripped for enrichment
 *     only; the row is kept (przedpremiera screenings, DKF-suffix forms, `*AD`).
 *   - `canonical` — Canonical scope: runs in `sanitize`, so it CHANGES the merge
 *     key and COLLAPSES spelling variants of one film into a single row (the
 *     "Mandalorian i Grogu" ~19-way fragmentation). Safe to add post-hoc — see
 *     the note on `canonical` below.
 *
 *  GlobalStructural decoration MERGES (`/Kino Cafe` collapsing into the base film)
 *  are still intentionally left out: a GlobalStructural rule does NOT change the
 *  `sanitize` key, so the stale "X/Kino Cafe" and the base "X" collide on cache
 *  hydration and Caffeine last-write-wins drops one. PerCinema and Canonical rules
 *  DON'T hit this: they change the key, so two rows have distinct keys before they
 *  collide. A GlobalStructural decoration merge requires per-cinema scoping. */
object ExtraTitleRules {

  private def perCinema(id: String, cinemaId: String, pattern: String, note: String): TitleRule =
    TitleRule(id, PerCinema, Some(cinemaId), pattern, "", applyAll = false, order = 0, note = Some(note))

  private def perCinemaReplace(id: String, cinemaId: String, pattern: String, replacement: String, note: String): TitleRule =
    TitleRule(id, PerCinema, Some(cinemaId), pattern, replacement, applyAll = false, order = 0, note = Some(note))

  /** One decoration the weekly discovery bot (`scripts.DecorationDiscovery`, `.github/workflows/decoration-discovery.yml`)
   *  proposed: a search strip measured on the unmatched clusters' fixture — at least one listing taken right, none wrong,
   *  no existing take switched, and no known film title starting (prefix) or ending (suffix) with what it strips — and the
   *  venue titles it was measured on, each with the search title it now gives. `DiscoveredDecorationsSpec` holds every
   *  rule to its examples and checks none of them comes out so without it. */
  final case class Discovered(rule: TitleRule, examples: Seq[(String, String)])

  /** The discovery bot's accepted proposals: the table's rows carrying the examples they were measured on. */
  val discovered: Seq[Discovered] = TitleRuleTable.rows.collect { case row if row.examples.nonEmpty => Discovered(row.rule, row.examples) }


  /** Per-cinema (client) cleanups — venue-specific junk too narrow to globalise
   *  safely, scoped to the one cinema's slug. PerCinema runs in `cinemaClean` at
   *  INGESTION (like the seed per-client `cleanTitle`), changing the display +
   *  merge key, so variants collapse into one rated row. Each requires the owning
   *  client to call `TitleNormalizer.cinemaClean("<slug>", …)` (wired in this
   *  change). Fourth-wave (2026-06-20). */
  val perCinemaRules: Seq[TitleRule] = Seq(
    perCinema("xtra-bajka-sps",        "kino-bajka",   """(?iu)\s+(?:2D|3D)\s+(?:DUB|NAP)\.?\s+SPS\s*$""", "Kino Bajka '… 2D DUB. SPS' screening-code suffix (Toy Story 5, Vaiana)"),
    perCinema("xtra-mok-miedzyrzecz-premiera", "kino-mok-miedzyrzecz", """(?i)\s+premiera(?=\s|$)""", "MOK Międzyrzecz '<film> PREMIERA [2D napisy]' premiere word (its client re-cases it) ahead of the format tag (Lalka, Witajcie w Hope)"),
    perCinema("xtra-cyfrowe-premiera", "cyfrowe-kino", """(?i)^Premiera!\s+""",                            "Cyfrowe Kino 'Premiera! <film>' prefix"),
    perCinema("xtra-kijow-napisy-pl",  "kino-kijow",   """(?iu)\s+(?:UA|UKR)?\s*Napisy\s+PL\s*$""",        "Kino Kijów '… [UA/UKR] Napisy PL' subtitle suffix (Diabeł …Prady 2, Mawka, On drive)"),
    // Shared-portal venues, cleaned via the owning client's Cinema.slug:
    //   Oskard → Bilety24Client, Stary Młyn → Bilety24OrganizerClient,
    //   Na Starówce + Farys → SystemBiletowyClient.
    perCinema("xtra-oskard-kino-cafe",  "kino-oskard",     """(?i)\s*/\s*(?:dubbing\s*/\s*)?Kino\s+Cafe\s*$""", "Kino Oskard '… /Kino Cafe' (and '/dubbing/Kino Cafe') venue suffix (Following, Robin Hood, Drugie życie, Supergirl, Toy Story 5)"),
    perCinema("xtra-starowce-akcja-lato", "kino-na-starowce", """(?i)\s*(?:[-–—]\s*)?(?:film\s+)?akcja\s+lato\s+w\s+kinie\s*$""", "Na Starówce '… [- film] akcja lato w kinie' campaign suffix (Toy Story 5, Vaiana)"),
    perCinema("xtra-starymlyn-sensoryczny", "kino-stary-mlyn", """(?i)\s+sensoryczny\s*$""",                   "Kino Stary Młyn '… sensoryczny' sensory-screening suffix (Toy Story 5)"),
    perCinemaReplace("xtra-farys-tot-story", "kino-farys", """(?i)^Tot\s+story\s+5$""", "Toy Story 5",        "Kino Farys source typo 'Tot story 5' → 'Toy Story 5'"),
    // Kluczbork brands its whole programme 'KLAPS / <cycle>: <film>'. Stripped HERE, in
    // the per-cinema tier, and not as a global banner rule: the seed
    // 'structural-slash-suffix' ('<film> / <decoration>') fires before anything in this
    // file — `all` stamps every extra order 100+, after the whole seed set — and on this
    // INVERTED shape it keeps the banner and throws the film away, so a global rule
    // never sees a title to strip. The per-cinema tier runs at ingest, before the
    // structural fold, so it gets there first. Leaves 'Kino Kobiet: <film>' for
    // xtra-pp-kino-kobiet-prefix above, which keeps the programme its own row.
    perCinema("xtra-bajka-kluczbork-klaps", "kino-bajka-kluczbork", """(?iu)^KLAPS\s*/\s*""", "Kino Bajka Kluczbork 'KLAPS / <cycle>: <film>' house-brand prefix (Jak żyć żeby nie zwariować, Przepis na święta)"),
    // "<film> w Helios na Scenie" — Helios brands its own event screenings in the LISTING
    // title, a spelling no other cinema uses. Its merge key is the problem: Cinema City's
    // detail page gives the plain "GHOST: 2 Big To Rig", both spellings resolve to tmdbId
    // 1693400, and with two keys the staging fold collapsed the decorated row into the
    // clean one on EVERY scrape — 31 slots rewritten per pass, which stopped the Polish leg
    // converging once per-film detail was fetched. So it comes off at ingest, where it
    // changes the key; scoped to the chain's key (every Helios venue shares `helios`,
    // `TitleRuleKey`), not a global Canonical fold of a name only Helios bills. The seed's
    // `helios-suffix-3` takes the exact " w Helios na Scenie"; this takes any case and spacing.
    perCinema("xtra-helios-na-scenie", "helios", """(?iu)\s+w\s+Helios\s+na\s+Scenie\s*$""", "'<film> w Helios na Scenie' Helios event-branding suffix, any case — merge-key fold")
  )

  /** The table's rows beyond the seed, then the venue rules still written here, numbered after the
   *  table's lookup rows as they were when all three lived here (the venue tier interleaves with the seed's orders). */
  val all: Seq[TitleRule] =
    TitleRuleTable.rows.filterNot(_.seed).map(_.rule) ++
    perCinemaRules.zipWithIndex.map { case (r, i) => r.copy(order = 407 + i) }
}
