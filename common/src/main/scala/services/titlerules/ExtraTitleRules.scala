package services.titlerules

import RuleScope._
import models.Country

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

  /** A Canonical-tier rule. `countries` defaults to every country, which is right
   *  for a language-NEUTRAL rule (a format-tag or bracketed-year strip). A rule
   *  whose replacement writes WORDS must name its countries — see
   *  [[TitleRule.countries]]: an unscoped Polish rewrite reaches the German and UK
   *  corpora and pins their rows to a key no local cinema slot can produce. */
  private def canon(id: String, pattern: String, replacement: String, note: String,
                    countries: Option[Set[Country]] = None): TitleRule =
    TitleRule(id, Canonical, None, pattern, replacement, applyAll = false, order = 0,
      note = Some(note), countries = countries)

  /** The countries a Polish-language rewrite may touch. */
  private val PolishOnly: Option[Set[Country]] = Some(Set(Country.Poland))

  /** A Canonical rewrite to a broadcaster's PROPER NAME ("Royal Ballet & Opera", "National Theatre Live") — the
   *  same words in every country's listings, so unscoped like a strip; see [[TitleRule.writesProperName]]. */
  private def canonProperName(id: String, pattern: String, replacement: String, note: String): TitleRule =
    canon(id, pattern, replacement, note).copy(writesProperName = true)

  private def perCinema(id: String, cinemaId: String, pattern: String, note: String): TitleRule =
    TitleRule(id, PerCinema, Some(cinemaId), pattern, "", applyAll = false, order = 0, note = Some(note))

  private def perCinemaReplace(id: String, cinemaId: String, pattern: String, replacement: String, note: String): TitleRule =
    TitleRule(id, PerCinema, Some(cinemaId), pattern, replacement, applyAll = false, order = 0, note = Some(note))

  /** The touring "Federico Fellini: ciao a tutti!" retrospective (~12 cinemas)
   *  brands its films in many shapes — a "Federico Fellini:" or bare "Fellini."
   *  prefix, an optional "ciao a tutti!" / "Ciao a tutti:" subtitle, and a
   *  "| / – przegląd Federico Fellini …" suffix. These patterns are used in TWO
   *  tiers: as GlobalStructural search strips (so the bare film resolves on TMDB)
   *  AND as Canonical merge-key strips (so the scraped decorated form keys the SAME
   *  as the bare display title). The Canonical tier is load-bearing: without it the
   *  decorated scrape form keyed differently from the bare TMDB title, so
   *  `MovieCache.recordCinemaScrape` re-diverted the row into staging every 30-min
   *  scrape and never settled — the Trójmiasto served-films count flapped ~172↔200
   *  (the swing was entirely Gdyńskie Centrum Filmowe). The bare colon form
   *  ("Fellini: …") is deliberately NOT matched (only "Federico Fellini:" or the
   *  dot "Fellini."), so a real film whose title merely starts "Fellini:" survives.
   *  The separator-less "Federico Fellini <FILM>" form (KinoMuza's "Federico Fellini
   *  SŁODKIE ŻYCIE") is matched too, but only with a `(?=\s+\S)` look-ahead so a bare
   *  "Federico Fellini" (the standalone documentary) is left intact, not amputated to
   *  an empty key. */
  private val FelliniPrefix =
    """(?iu)^(?:Federico\s+Fellini\s*:|Federico\s+Fellini(?=\s+\S)|Fellini\s*\.)\s*(?:ciao\s+a?\s*tutti\s*!?)?\s*[:\-–—]?\s*"""
  private val FelliniSuffix =
    """(?i)\s*(?:\|\s*|[–—-]\s*przegl[ąa]d\s+)Federico\s+Fellini\b.*$"""

  /** A festival or film club's ACRONYM tag on its screening of an ordinary film, generic in the
   *  acronym: a numbered edition BEFORE the film ("11. UFF - Demony", "19. FGA: Szepty lasu") or a
   *  pipe tag AFTER it ("Atlantyda | UFF", "Frances Ha | DKF", "Kumotry | FKS"). The tag is the
   *  screening's, never the film's, so it comes off in two tiers, like the Fellini patterns above: the
   *  lookup query (the film is found bare) and the Canonical tier (the card shows the film's own title
   *  and keys with its other listings). The Canonical rules are REWRITES (`$1`), not strips, so a
   *  film only one venue lists — most of a festival's programme — shows bare too (see
   *  `TitleNormalizer.preferredDisplay`).
   *
   *  What keeps each one honest:
   *   - prefix: an edition ORDINAL, then 2–6 CAPITALS (case-sensitive) and a separator straight after
   *     them, so "1. FC Köln - Der Film" (the name runs on) and "32. MFSP A PART - Trixie" stay whole;
   *   - year prefix: an edition YEAR in place of the ordinal and no separator ("2026 LHFF Scream (1996)"),
   *     so the acronym must be 2–6 CAPITALS standing alone and a lower-case letter must follow — "2001 A
   *     Space Odyssey" (one letter), "2001 Maniacs" (a word), "2001 MANIACS" (seven capitals) and an
   *     all-capitals listing like "1776 THE MUSICAL" (nothing says a title follows) stay whole;
   *   - suffix: 3–6 CAPITALS ending the title after a pipe, with a lower-case letter BEFORE the pipe —
   *     in "KINO SENIORA | LALKA" the capitals after the pipe are the film — and never a signed,
   *     subtitled or language-version tag (PJM, SDH, ENG, UKR, ORG): that screening is its own
   *     audience's card. Screen-format tags ("| IMAX", "| DUB") are `FormatTags`' and are peeled at
   *     ingest before this tier sees the title. */
  private val NumberedFestivalTag = """^\d{1,3}\.\s*\p{Lu}{2,6}\s*[-–—:]\s*"""
  private val YearFestivalTag     = """^(?:19|20)\d{2}\s+\p{Lu}{2,6}\s+(?=.*\p{Ll})"""
  private val FestivalTagAfterPipe = """\s*\|\s*(?!(?:PJM|SDH|ENG|UKR|ORG)\s*$)\p{Lu}{3,6}\s*$"""

  /** One decoration the weekly discovery bot (`scripts.DecorationDiscovery`, `.github/workflows/decoration-discovery.yml`)
   *  proposed: a search strip measured on the unmatched clusters' fixture — at least one listing taken right, none wrong,
   *  no existing take switched, and no known film title starting (prefix) or ending (suffix) with what it strips — and the
   *  venue titles it was measured on, each with the search title it now gives. `DiscoveredDecorationsSpec` holds every
   *  rule to its examples and checks none of them comes out so without it. */
  final case class Discovered(rule: TitleRule, examples: Seq[(String, String)])

  /** The discovery bot's accepted proposals: the table's rows carrying the examples they were measured on. */
  val discovered: Seq[Discovered] = TitleRuleTable.rows.collect { case row if row.examples.nonEmpty => Discovered(row.rule, row.examples) }


  /** Canonical (merge-key) unifications. Unlike the strips above these run in
   *  `sanitize`, so they COLLAPSE spelling variants of one film into a single
   *  `movies` row. The GlobalStructural caveat in the header (decoration MERGES left
   *  out) does NOT apply to this tier — Canonical rules change the key, so two rows
   *  have distinct keys before they collide.
   *
   *  Curated from the 2026-06 corpus where one film fragments across many rows
   *  that never share showtimes — "Mandalorian i Grogu" split ~19 ways by a
   *  lower-case "Gwiezdne wojny:" prefix the seed rule (capitalised only) missed,
   *  the English title, and trailing language/format suffixes left in the title by
   *  cinemas whose scraper doesn't run `FormatTags.extractFormatTags`. */
  val canonical: Seq[TitleRule] = Seq(
    // The "Federico Fellini: ciao a tutti!" retrospective decoration, stripped at
    // the MERGE-KEY level (not just the query level above). Without these the
    // scraped "Federico Fellini: Noce Cabirii" keyed apart from the bare "Noce
    // Cabirii" display/TMDB title, so the row re-diverted into staging every scrape
    // and the served-films count flapped (~172↔200 at Trójmiasto / Gdyńskie Centrum
    // Filmowe). Folds every decorated form onto the bare film's key so it settles.
    // Shares the exact patterns with the GlobalStructural strips (see FelliniPrefix
    // / FelliniSuffix) — same shape, two tiers, two purposes.
    //
    // Ordered FIRST — before the trailing year/format/sound strips below — so the
    // banner is removed before they run: a suffix form glues the year in BEFORE the
    // banner ("Noce Cabirii (1957) | FEDERICO FELLINI …"), where the year is
    // mid-string until the banner is gone, so the `$`-anchored trailing-paren-year
    // strip can only catch it once the banner strip has exposed it at the end.
    canon("xtra-canonical-fellini-prefix", FelliniPrefix, "",
      "'Federico Fellini: [ciao a tutti!] <film>' / 'Fellini. [Ciao a tutti:] <film>' retrospective prefix — merge-key fold (Noce Cabirii, Słodkie życie, Wałkonie, Giulietta i duchy, Osiem i pół)"),
    canon("xtra-canonical-fellini-suffix", FelliniSuffix, "",
      "'<film> [(year)] | / – przegląd Federico Fellini …' retrospective suffix — merge-key fold; runs before the year strip so a '(1957)' glued in before the banner still folds (Noce Cabirii, Wałkonie, Słodkie życie)"),
    canon("xtra-canonical-festival-tag-prefix", s"""(?u)$NumberedFestivalTag(\\S.*)$$""", "$1",
      "'11. UFF - <film>' / '19. FGA: <film>' numbered festival-acronym prefix — CANONICAL rewrite to the film's title (display + merge key); see NumberedFestivalTag"),
    canon("xtra-canonical-festival-year-tag-prefix", s"""(?u)$YearFestivalTag(\\S.*)$$""", "$1",
      "'2026 LHFF <film>' year-numbered festival-acronym prefix — CANONICAL rewrite to the film's title (display + merge key); runs before the trailing-year strips; see YearFestivalTag"),
    canon("xtra-canonical-festival-tag-suffix", s"""(?u)^(.*\\p{Ll}.*?)$FestivalTagAfterPipe""", "$1",
      "'<film> | UFF / DKF / FKS / CHKF' festival or club acronym after a pipe — CANONICAL rewrite to the film's title; runs before the trailing-year strips so a year it hid is exposed; see FestivalTagAfterPipe"),
    // A Polish distributor's ". Film" suffix ("Rolling Loud. Film" for "Rolling Loud: The Movie", Jaworzyna's
    // "Rolling Loud. Film 2026"): the film's own title, shown and merged without it. The full stop right
    // before it is required — "Straszny film" ends in the word and keeps it.
    // A REWRITE (it keeps the title before the suffix, `$1`), so a card every venue bills alike shows it too.
    canon("xtra-canonical-kropka-film-suffix",
      """(?iu)^(.*\S)\.\s*Film(?:\s+\d{4})?\s*$""", "$1",
      "'<film>. Film [2026]' distributor suffix — CANONICAL rewrite to the film's title"),
    canon("xtra-canonical-gwiezdne-wojny-ci",
      """(?iu)^Gwiezdne\s+wojny\s*:\s*""", "",
      "Case-insensitive 'Gwiezdne wojny:' franchise prefix — the seed 'canonical-gwiezdne-wojny' only matches the capitalised 'Gwiezdne Wojny:', so the lower-case spelling (Mandalorian i Grogu) never merged."),
    // The seed's 'structural-anniversary-suffix' and the extras'
    // 'xtra-rocznica-premiery-suffix' are both GlobalStructural — they shape the
    // TMDB query but leave the decorated title as the row's display AND its merge
    // key, so "Terminator 2: dzień sądu - 35. Rocznica" kept its own card and its
    // own /poznan/film/terminator-2-dzien-sadu-35-rocznica slug alongside the base
    // film. A re-release for a round anniversary IS the film, so fold it
    // canonically. Nominative 'rocznica' ONLY: the accusative 'w 100. rocznicę
    // urodzin' shapes are retrospective-series banners naming a DIRECTOR, not the
    // film ("BRZEZINA | WAJDA: re- wizje. … w 100. rocznicę urodzin"), and the
    // greedy tail would eat the trailing "w". The leading `(?<=\S)` requires a film
    // before the marker, so a listing that IS the banner ("100. rocznica urodzin
    // Tadeusza Konwickiego: …") isn't reduced to an empty key, and the required
    // digits keep the 2025 Polish film literally titled "Rocznica" whole. `\b`
    // before the digits is what makes that lookbehind honest: without it the match
    // simply restarts one character in, so the banner above stripped to "1".
    canon("xtra-canonical-rocznica-suffix",
      """(?iu)(?<=\S)\s*(?:{{SEPD}})?\b\d{1,3}\s*\.?\s*rocznica\b.*$""", "",
      "'<film> [-|.] N. Rocznica [premiery/urodzin …]' anniversary-rerelease suffix — CANONICAL fold onto the base film (display + merge key), not just the query: 'Terminator 2: dzień sądu - 35. Rocznica', 'Kosmiczny Mecz 30. Rocznica', 'Kosmiczny mecz. 30 rocznica', 'Salto | 100. rocznica urodzin Tadeusza Konwickiego'. Nominative only, and requires both a preceding film and a number — see the note above."),
    // The trailing screen-format/language rules — 2D/3D/dub/napisy/lektor in the
    // space·dash·slash·bracket shapes, plus the "(Dolby Atmos)"/"[2D]"/"(IMAX)"
    // sound-tag — moved to the shared `services.movies.FormatTags` and are applied
    // centrally at ingest (`MovieCache.recordCinemaScrape`), which rewrites the
    // title AND badges the screenings for EVERY cinema. The Ukrainian-screening
    // guard moved there too. (`xtra-canonical-trailing-paren-lang` below is kept:
    // it still carries the non-format "wersja oryginalna" paren the extractor
    // doesn't own.)
    canon("xtra-canonical-trailing-paren-lang",
      """(?iu)\s*[\[(]\s*wersja\s+oryginalna(?:\s+pl)?\s*[\])]\s*$""", "",
      "Trailing PARENTHESISED 'wersja oryginalna' (original-language, no-dub) tag ('(wersja oryginalna)', '[wersja oryginalna PL]') — merges the original-language edition onto the base. The parenthesised FORMAT tags this rule used to also strip ('(Dubbing PL)', '(Napisy PL)', '(lektor)', '[dubbing PL]') now peel via the shared `FormatTags` at ingest, which also keeps a '(ukraiński dubbing)' whole."),
    canon("xtra-canonical-trailing-org-wersja",
      """(?iu)\s*[/|.,–—-]\s*(?:wersja\s+oryginalna|org)\s*$""", "",
      "Trailing 'wersja oryginalna' / 'ORG' (original-language, no-dub) marker after a separator ('Toy Story 5 - wersja oryginalna', 'Toy Story 5 - ORG'). Requires a separator so a film that merely ends in those letters isn't eaten."),
    canon("xtra-canonical-trailing-paren-year",
      """(?iu)\s*[\[({<]\s*(?:19|20)\d{2}\s*[\])}>]\s*$""", "",
      "Trailing BRACKETED release year — parens, square, curly or angle ('(1991)', '[1976]', '{1963}', '<1999>') glued into the title by a scraper — never part of the film's identity, so 'Milczenie owiec (1991)' merges into 'Milczenie owiec'. Only a BRACKETED form, so a bare year that IS the title ('1917', '2046') survives. Matches `EmbeddedYear`'s delimited shapes (which reads the year), minus the separator form below."),
    canon("xtra-canonical-trailing-sep-year",
      """(?iu)\s*[,–—-]\s*(?:19|20)\d{2}\s*$""", "",
      "Trailing release year after a SEPARATOR ('Film - 1989', 'Film, 2001') — the non-bracketed delimited form `EmbeddedYear` also reads. Requires a comma / hyphen / en- or em-dash so a bare year that IS the title ('1917', '2049') is left alone."),
    canon("xtra-canonical-mandalorian-grogu-en",
      """(?iu)^The\s+Mandalorian\s+and\s+Grogu$""", "Mandalorian i Grogu",
      "Map the English title to the Polish canonical (same TMDB id 1228710) so the EN-titled listing merges; ordered after the trailing-format strip clears its '2D DUB' suffix first. POLAND ONLY — German cinemas list this film as 'The Mandalorian And Grogu', which matched this rule and pinned the Berlin row to the Polish key `mandalorianigrogu`, so showtimes-de displayed 'Mandalorian i Grogu'. The UK escaped only because its cinemas spell it with '&'.",
      countries = PolishOnly),
    // "Niesamowite przygody skarpetek 3. Ale kosmos!" is a TMDB-no-match kids'
    // film that fragments across cinemas at the SUBTITLE level: Helios romanises
    // the sequel number (III), Cinema City ships a source-truncated
    // "…skarpetek 3. Ale ko", and others drop the "!" or the subtitle entirely
    // ("…skarpetek 3"). With no TMDB id there's no fold edge, so every spelling
    // is its own row. Because `canonical` runs AFTER `normalize`, the bare "3"
    // has already become "III" while a dotted "3." stays Arabic — so match both.
    // The third film's subtitle "Ale kosmos!" is what disambiguates it from the
    // FIRST ("Niesamowite przygody skarpetek", no number) and the SECOND
    // ("…skarpetek 2. Skarpetki górą!"), which the `3`/`III` requirement leaves
    // untouched. Anything after the sequel number (a partial/truncated/decorated
    // subtitle) collapses onto the canonical full title.
    canon("xtra-canonical-skarpetek-3",
      """(?iu)^Niesamowite\s+przygody\s+skarpetek\s+(?:III|3)\b.*$""",
      "Niesamowite przygody skarpetek 3. Ale kosmos!",
      "Collapse every spelling of the TMDB-no-match 'Niesamowite przygody skarpetek 3. Ale kosmos!' (Roman III, the Cinema-City source-truncated 'Ale ko', the subtitle-less '…skarpetek 3', the format-tagged variants) onto one canonical title. The '3'/'III' requirement keeps films 1 (no number) and 2 (…skarpetek 2.) as their own rows. POLAND ONLY: the replacement writes Polish words, so it must not reach another country's corpus even though the pattern is unlikely to match there.",
      countries = PolishOnly),
    // Live-relay broadcasts: one production, billed under its house's banner in as many spellings as there are
    // ticketing systems — Germany alone lists Romeo und Julia as "RBO 26/27: … (Royal Ballet) (Aufz.)", "RBO
    // 2026/27: … (The Royal Ballet)" and "RBO Live 2026/27: …", and Poland lists one NT Live play three ways
    // ("NT Live: Audiencja", "Audiencja - National Theatre Live 2026", "Audiencja | TEATR W KINIE | National
    // Theatre Live"). Each spelling keyed its own row and showed its own card. These rules unify the BANNER —
    // they never strip it: the broadcast stays its own card, apart from any film of the work's name (a ballet
    // 'Manon' is not a film 'Manon'; see the fourteenth-wave note above). Ordered after the bracketed-year strip,
    // which first takes the '(2027)' some venues append. Real spellings from every country's recorded fixtures
    // are in ExtraTitleRulesSpec.
    //
    // 1. The SEASON in a house banner ("RBO 26/27:", "RBO Cinema Season 2026-27:", "Royal Ballet and Opera Sezon
    //    Kinowy 2026-27:", "Met Opera 2026-27:"), generic in the house: one production carries one season, and
    //    half the venues leave it out ("Met Opera: Manon", "Royal Ballet and Opera: Tosca"). Needs the colon, so a
    //    banner that IS the listing ("The Metropolitan Opera 2026-27 Live in HD Series") keeps its season.
    canon("xtra-canonical-broadcast-season",
      """(?iu)^(\p{L}[\p{L}\p{N}&'.+ ]{0,60}?)\s+(?:(?:Live|Cinema\s+Season|Sezon\s+Kinowy|Kinosaison|Season|Saison|Spielzeit|Temporada)\s+)?(?:19|20)?\d{2}\s*[-/–]\s*(?:19|20)?\d{2}\s*:\s*""",
      "$1: ",
      "'<house> [Live | Cinema Season | Sezon Kinowy …] 2026/27: <work>' live-relay season token — dropped from the banner so every season spelling (26/27, 2026/27, 2026-27) and the season-less form share one title"),
    // 2. Royal Ballet & Opera under its acronym, its 'and'/'i'/'+' spellings, or one of its two companies (the
    //    "The Royal Ballet: Manon" banner, the German "(The Royal Ballet)" credit after the work), plus the German
    //    "(Aufz.)" recording tag — an encore-like note on the showing, never part of the production's name.
    canonProperName("xtra-canonical-royal-ballet-opera",
      """(?iu)^(?:RBO|Royal\s+Ballet\s*(?:&|\+|and|und|i|y)\s*Opera|The\s+Royal\s+(?:Ballet|Opera))\s*(?::|\s[-–—]|-)\s*(\S.*?)(?:\s*\((?:The\s+)?Royal\s+(?:Ballet|Opera)\))?(?:\s*\(Aufz(?:\.|eichnung)\))?\s*$""",
      "Royal Ballet & Opera: $1",
      "'RBO: <work> (The Royal Ballet) (Aufz.)' / 'Royal Ballet and Opera: <work>' / 'The Royal Opera: <work>' — one house banner"),
    // 3. The Met, prefixed ("Met Opera:", "The MET Opera:") or — Kino 1410's shape — after the work
    //    ("CARMEN | Metropolitan Opera: Live in HD 2026/27").
    canonProperName("xtra-canonical-met-opera-suffix",
      """(?iu)^(\S.*?)\s*\|\s*(?:The\s+)?Metropolitan\s+Opera(?::\s*Live\s+in\s+HD)?(?:\s+(?:19|20)\d{2}\s*[-/]\s*\d{2,4})?\s*$""",
      "The Metropolitan Opera: $1",
      "'<work> | Metropolitan Opera: Live in HD 2026/27' — the Met's banner after the work, moved to the front"),
    canonProperName("xtra-canonical-met-opera",
      """(?iu)^(?:The\s+)?(?:Met|Metropolitan)\s+Opera\s*:\s*(\S.*)$""",
      "The Metropolitan Opera: $1",
      "'Met Opera: <work>' / 'The MET Opera: <work>' — one house banner"),
    // 4. National Theatre Live, prefixed ("NT Live: <play>", "NT Live <play>", its own "NTL: <play>", "NTL Live: <play>")
    //    or after the play — Cinema City's "<play> - National Theatre Live 2026" and Gdynia's "<play> | TEATR W KINIE |
    //    National Theatre Live". UK The Beehive's "NTL: Hay Fever" reached its siblings only through a bare "Hay Fever"
    //    [120′], which their running times keep apart.
    canonProperName("xtra-canonical-nt-live-suffix",
      """(?iu)^(\S.*?)\s*(?:\|\s*Teatr\s+w\s+kinie\s*)?(?:\||\s[-–—])\s*(?:NT|National\s+Theatre)\s+Live(?:\s+(?:19|20)\d{2})?\s*$""",
      "National Theatre Live: $1",
      "'<play> - National Theatre Live 2026' / '<play> | TEATR W KINIE | National Theatre Live' — the banner after the play, moved to the front"),
    canonProperName("xtra-canonical-nt-live",
      """(?iu)^(?:(?:NT|National\s+Theatre)\s+Live|NTL(?:\s+Live)?)\s*(?::\s*|\s)(\S.*)$""",
      "National Theatre Live: $1",
      "'NT Live: <play>' / 'NT Live <play>' / 'NTL: <play>' — one house banner"),
    // 5. A concert tour's live viewing billed without the tour's own name: UK Odeon's "BTS 'ARIRANG' IN BUENOS AIRES: LIVE
    //    VIEWING" ×68 (and São Paulo ×68, recording 573) is TMDB's "BTS WORLD TOUR [ARIRANG] in Buenos Aires" (1770237),
    //    which Cineworld and Flicks bill as "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES". Searched as billed it finds nothing
    //    (its "LIVE VIEWING" piece finds every K-pop live viewing), and the director's filmography weighs every city's
    //    record alike: the other city's, crediting him, won. One tour, one title.
    canonProperName("xtra-canonical-bts-world-tour",
      """(?iu)^BTS\s+(['‘’][^'‘’]+['‘’])\s+IN\s+(\S.*)$""",
      "BTS WORLD TOUR $1 IN $2",
      "'BTS '<tour>' IN <city>[: LIVE VIEWING]' — the tour's title as TMDB and the other venues bill it")
  )

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

  /** The table's rows beyond the seed, then the merge-key and venue rules still written here, numbered after the
   *  table's lookup rows as they were when all three lived here (the venue tier interleaves with the seed's orders). */
  val all: Seq[TitleRule] =
    TitleRuleTable.rows.filterNot(_.seed).map(_.rule) ++
    (canonical ++ perCinemaRules).zipWithIndex.map { case (r, i) => r.copy(order = 386 + i) }
}
