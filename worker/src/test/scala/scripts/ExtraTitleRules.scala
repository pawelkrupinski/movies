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
 *  hydration — Caffeine last-write-wins drops one before
 *  `NormalizationRebuilder.rebuild` ever sees two entries to union. PerCinema and
 *  Canonical rules DON'T hit this: they change the key, so the rebuilder re-keys
 *  and unions on collision — which is why the seed's per-cinema migrations and the
 *  `canonical` additions below back-fill fine. A GlobalStructural decoration merge
 *  still waits on a rebuild union-on-collision fix (or per-cinema scoping). */
object ExtraTitleRules {

  private def prog(id: String, pattern: String, note: String): TitleRule =
    TitleRule(id, GlobalStructural, None, pattern, "", applyAll = false, order = 0,
      tag = Some("programmePrefix"), note = Some(note))

  private def searchStrip(id: String, pattern: String, note: String): TitleRule =
    TitleRule(id, GlobalStructural, None, pattern, "", applyAll = false, order = 0, note = Some(note))

  private def canon(id: String, pattern: String, replacement: String, note: String): TitleRule =
    TitleRule(id, Canonical, None, pattern, replacement, applyAll = false, order = 0, note = Some(note))

  private def perCinema(id: String, cinemaId: String, pattern: String, note: String): TitleRule =
    TitleRule(id, PerCinema, Some(cinemaId), pattern, "", applyAll = false, order = 0, note = Some(note))

  private def perCinemaReplace(id: String, cinemaId: String, pattern: String, replacement: String, note: String): TitleRule =
    TitleRule(id, PerCinema, Some(cinemaId), pattern, replacement, applyAll = false, order = 0, note = Some(note))

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
    prog("xtra-pp-best-film-on-tour", """(?i)^Best\s+Film\s+on\s+Tour\s*[:|]\s*""",       "Best Film on Tour"),
    // Second-wave (2026-06-18) audit: retrospective / classics banners that prefix
    // the film (own display row, query stripped to the bare film so it enriches).
    prog("xtra-pp-wajda-rewizje",     """(?i)^(?:Cykl\s+[„"]?\s*)?WAJDA:\s*re-?\s*wizje[^:]*:\s*""", "WAJDA: re-wizje retrospective prefix"),
    prog("xtra-pp-klasyka-na-topie",  """(?i)^Klasyka\s+na\s+TOPie(?:\s+na\s+[^:]+)?:\s*""", "'Klasyka na TOPie [na …]:' classics strand"),
    prog("xtra-pp-klasyka-atlantic",  """(?i)^Klasyka\s+w\s+kinie\s+Atlantic:\s*""",      "'Klasyka w kinie Atlantic:' classics strand"),
    prog("xtra-pp-pnkf-prefix",       """(?i)^\d+\.\s*PRZEGLĄD\s+NOWEGO\s+KINA\s+FRANCUSKIEGO:\s*""", "'17. Przegląd Nowego Kina Francuskiego:' festival prefix"),
    // Third-wave (2026-06-19) audit of the rating-less corpus: audience / club
    // programme banners that prefix the film (own display row, query stripped).
    prog("xtra-pp-bezpieczne-wakacje", """(?i)^Bezpieczne\s+wakacje:\s+""",                "'Bezpieczne wakacje:' kids-summer strand (Fleak, Hitpig, Tom i Jerry, …)"),
    prog("xtra-pp-skocz-z-bajtlem",    """(?i)^Skocz\s+z\s+Bajtlem\s+do\s+kina:\s+""",      "'Skocz z Bajtlem do kina:' programme prefix (Ojczyzna, Drugie życie)"),
    prog("xtra-pp-kmw",                """(?i)^KMW:\s+""",                                  "'KMW:' Kino Małego Widza kids-strand prefix"),
    prog("xtra-pp-kf-klub",            """(?i)^KF\s+[^:]+:\s+""",                           "'KF <klub>:' film-club prefix (KF Ambasada: Hannah i jej siostry)"),
    prog("xtra-pp-seans-dla-rodzicow", """(?i)^Seans\s+filmowy\s+dla\s+rodziców:\s+""",      "'Seans filmowy dla rodziców:' parents'-screening prefix"),
    // Fourth-wave (2026-06-20) audit of the rating-less corpus: club / audience
    // programme banners that prefix the film (own display row, query stripped).
    prog("xtra-pp-kino-seniora-colon", """(?i)^Kino\s+seniora:\s+""",                        "'Kino seniora:' colon variant of the existing pipe form (Takie jest życie)"),
    prog("xtra-pp-fregata-seniorow",   """(?iu)^Fregata\s+dla\s+Seniorów:\s+""",             "'Fregata dla Seniorów:' senior strand (Ojczyzna)"),
    prog("xtra-pp-janosik-kids",       """(?i)^Janosik\s+(?:Dzieciom|Szkrabom):\s+""",       "'Janosik Dzieciom/Szkrabom:' kids strands (Minionki i straszydła)"),
    prog("xtra-pp-najlepsze-z-najgorszych", """(?i)^Najlepsze\s+z\s+najgorszych:\s+""",      "'Najlepsze z najgorszych:' bad-movie-night cycle — global so it covers Kosmos/Mikro/NCKF, not just the muza per-cinema seed rule (Sarnie żniwo, Brudny Henryk)"),
    prog("xtra-pp-czlowiek-na-planie", """(?iu)^Człowiek\s+na\s+pierwszym\s+planie:\s+""",   "'Człowiek na pierwszym planie:' Światowid cycle (Takie jest życie)"),
    prog("xtra-pp-meskie-kino",        """(?iu)^Męskie\s+Kino[^:]*:\s+""",                   "'Męskie Kino [na Dzień Ojca]:' cycle (Czas Apokalipsy)"),
    prog("xtra-pp-kobieta-pelna-zycia", """(?iu)^Kobieta\s+Pełna\s+Życia:\s+""",             "'Kobieta Pełna Życia:' Fregata cycle (Książę)")
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
    searchStrip("xtra-fellini-suffix",             """(?i)\s*(?:\|\s*|[–—-]\s*przegl[ąa]d\s+)Federico\s+Fellini\b.*$""", "'… | / – przegląd FEDERICO FELLINI …' retrospective suffix"),
    // Second-wave (2026-06-18) audit: classics / retrospective / cycle banners
    // that SUFFIX the film (the screening keeps its decorated row, the query
    // resolves the bare film). All cross-cinema or otherwise specific enough that
    // no real film title collides.
    searchStrip("xtra-10-10-klasyka",              """(?i)\s*\|\s*10/10\s+Klasyka\s+filmowa\s*$""",   "'| 10/10 Klasyka filmowa' classics-suffix (Piast, Apollo Wałbrzych, Lot)"),
    searchStrip("xtra-wajda-rewizje-suffix",       """(?i)\s*[|\\]\s*[„"]?\s*WAJDA:\s*re-?\s*wizje.*$""", "'… | / \\ WAJDA: re-wizje …' retrospective suffix"),
    searchStrip("xtra-pnkf-suffix-pipe",           """(?i)\s*\|\s*Przegląd\s+Nowego\s+Kina\s+Francuskiego\s*$""", "'… | Przegląd Nowego Kina Francuskiego' festival suffix"),
    searchStrip("xtra-pnkf-suffix-edycja",         """(?i)\s*\(org\.[^)]*\)\s*\d+\.\s*edycja\s+Przeglądu\s+Nowego\s+Kina\s+Francuskiego\s*$""", "'(org. …) 17. edycja Przeglądu Nowego Kina Francuskiego' suffix"),
    // `(?iu)` (not `(?i)`): the Kino Zamek raw is ALL-CAPS ("– AMERYKAŃSKA KLASYKA",
    // "| ŻUŁAWSKI. KINO EKSTAZY"), so Unicode case folding is needed to fold Ż/Ł/Ń —
    // plain `(?i)` is ASCII-only and won't match them (same gotcha as wybrzeze).
    searchStrip("xtra-amerykanska-klasyka",        """(?iu)\s*[–—-]\s*amerykańska\s+klasyka(?:\s*/.*)?\s*$""", "'– amerykańska klasyka [/ N. rocznica]' suffix (Kino Zamek)"),
    searchStrip("xtra-zulawski-kino-ekstazy",      """(?iu)\s*[|–—-]\s*żuławski\.?\s*kino\s+ekstazy\s*$""", "'… | / – żuławski. Kino ekstazy' retrospective suffix (Kino Zamek)"),
    searchStrip("xtra-poniedzialki-konwicki",      """(?i)\s*\|\s*Poniedziałki\s+z\s+Konwickim\b.*$""", "'… | Poniedziałki z Konwickim …' cycle suffix (Kino Spektrum)"),
    searchStrip("xtra-jim-jarmusch-suffix",        """(?i)\s*//\s*jim\s+jarmusch\s*$""",              "'// jim jarmusch' director-cycle suffix (Kino za Rogiem)"),
    // Third-wave (2026-06-19) audit of the rating-less corpus.
    // Retrospective / art-on-screen series whose film follows the banner
    // ('<cycle>: ' or '<cycle> - '); query-only strip keeps the decorated row.
    searchStrip("xtra-pedro-almodovar-kolory",     """(?i)^Pedro\s+Almodóvar:\s+Kolory\s+emocji\s*[-–—]\s*""", "'Pedro Almodóvar: Kolory emocji - <film>' retrospective (Matador, Matki równoległe, …)"),
    searchStrip("xtra-wielka-sztuka-rialto",       """(?i)^Wielka\s+Sztuka\s+w\s+Kinoteatrze\s+Rialto\s*[-–—]\s*""", "'Wielka Sztuka w Kinoteatrze Rialto - <film>' art-doc series (~10 titles)"),
    searchStrip("xtra-sztuka-w-centrum",           """(?iu)^SZTUKA\s+W\s+CENTRUM\.\s*NOWOŚCI\s+\d{4}\s*\|\s*""", "'SZTUKA W CENTRUM. NOWOŚCI 2026 | <film>' art-doc series"),
    searchStrip("xtra-lekcje-filmowe",             """(?i)^Lekcje\s+Filmowe\s*[-–—]\s*""",            "'Lekcje Filmowe - <film>' strand (Nić widmo, Śniadanie u Tiffany'ego)"),
    searchStrip("xtra-art-beats",                  """(?i)^Art\s+Beats:\s+""",                        "'Art Beats: <film>' art-doc series (Rafael, Święty Piotr, …)"),
    searchStrip("xtra-wajda-dot-prefix",           """(?i)^Wajda\.\s+""",                             "'Wajda. <film>' director-retrospective prefix (Brzezina, Kronika wypadków miłosnych)"),
    searchStrip("xtra-konwicki-prefix",            """(?i)^Konwicki:\s+""",                           "'Konwicki: <film>' director-retrospective prefix (Dolina Issy, Lawa, Salto)"),
    searchStrip("xtra-pokaz-przedpremierowy-prefix", """(?i)^Pokaz\s+przedpremierowy:\s*""",          "'Pokaz przedpremierowy: <film>' premiere prefix"),
    // Decoration suffixes (banner after the film); query-only strip keeps the row.
    searchStrip("xtra-jarocinski-festiwal-suffix", """(?iu)\s*[.:\s]*II\s+Jarociński\s+Festiwal\s+Filmowy\s+dla\s+Dzieci\s+i\s+Młodzieży\s*$""", "'<film>: II Jarociński Festiwal Filmowy dla Dzieci i Młodzieży' suffix (~6 films)"),
    searchStrip("xtra-przeglad-filmow-suffix",     """(?i)\s*[-–—]\s*przegląd\s+filmów\s+.*$""",      "'<film> - przegląd filmów <reżyser>' suffix (Ida, Zimna wojna)"),
    searchStrip("xtra-kino-dla-suffix",            """(?i)\s*[-–—]\s*kino\s+dla\s+(?:seniora|seniorów|kobiet|dzieci)\s*$""", "'<film> - kino dla seniora/kobiet' audience suffix"),
    searchStrip("xtra-kntj-suffix",                """(?i)\s*[-–—]\s*KNTJ\s*$""",                     "'<film> - KNTJ' cross-cinema strand suffix"),
    searchStrip("xtra-pokaz-suffix",               """(?i)\s*[-–—|]\s*pokaz\b.*$""",                  "'<film> - / | pokaz <specjalny|przedpremierowy|+ dyskusja…>' event suffix"),
    searchStrip("xtra-tadeusz-konwicki-suffix",    """(?i)\s*[-–—]\s*tadeusz\s+konwicki\b.*$""",      "'<film> – Tadeusz Konwicki / 100. rocznica urodzin' suffix"),
    searchStrip("xtra-wajda-o-filmie-suffix",      """(?i)\s*[-–—]\s*Andrzej\s+Wajda\s+o\s+filmie\s*$""", "'<film> - Andrzej Wajda o filmie' suffix (Brzezina)"),
    // Fourth-wave (2026-06-20) audit of the rating-less corpus.
    // Director-retrospective DOT prefixes ('<Director>. <film>'), siblings of the
    // existing 'Wajda.' / 'Konwicki:' rules; each merges into an already-RATED row.
    searchStrip("xtra-fellini-dot-prefix",         """(?iu)^Fellini\.\s+""",                          "'Fellini. <film>' retrospective prefix (Giulietta i duchy, Głos z księżyca, Noce Cabirii, Wałkonie — all rated)"),
    searchStrip("xtra-hosoda-dot-prefix",          """(?iu)^Hosoda\.\s+""",                           "'Hosoda. <film>' Mamoru Hosoda retrospective prefix (Wilcze dzieci rated; Summer Wars, Belle, Scarlet)"),
    searchStrip("xtra-konwicki-dot-prefix",        """(?iu)^Konwicki\.\s+""",                         "'Konwicki. <film>' DOT variant of the existing colon prefix (Ostatni dzień lata, Salto, Lawa)"),
    // Cross-cinema decoration suffixes (banner after the film); query-only strip.
    // ('+ prelekcja' / '+ wstęp' is already stripped by the seed PlusSuffix rule.)
    searchStrip("xtra-kf-klub-suffix",             """(?i)\s*\|\s*KF\s+\S.*$""",                      "'<film> | KF <klub>' film-club suffix (Fellini. Noce Cabirii | KF Ambasada)"),
    searchStrip("xtra-wtorek-seniora-suffix",      """(?i)\s*\|\s*Wtorek\s+Seniora\s*$""",            "'<film> | Wtorek Seniora' senior-screening suffix (Ojczyzna)"),
    searchStrip("xtra-fks-suffix",                 """(?iu)\s*[|_]\s*FKS\s*$""",                      "'<film>_FKS' / '<film> | FKS' Filmowy Klub Seniora suffix (Takie jest życie, 500 Mil, Posłani)"),
    searchStrip("xtra-pokazy-specjalne-suffix",    """(?i)\s*[-–—|]\s*pokazy\s+specjalne\s*$""",      "'<film> - pokazy specjalne' suffix (the xtra-pokaz-suffix rule's pokaz\\b can't match the 'pokazy' plural) (Milczenie owiec)"),
    searchStrip("xtra-wakacje-z-dokumentem-suffix", """(?i)\s*\|\s*Wakacje\s+z\s+dokumentem\s*$""",   "'<film> | Wakacje z dokumentem' documentary-strand suffix (Silver)")
  )

  /** Canonical (merge-key) unifications. Unlike the strips above these run in
   *  `sanitize`, so they COLLAPSE spelling variants of one film into a single
   *  `movies` row — and re-merge the fragments already in prod: a Canonical edit
   *  changes the sanitize key, so `NormalizationRebuilder` re-keys every affected
   *  row and unions on collision (`MovieRecordMerge.unionAll`). That back-fill is
   *  clean, so the GlobalStructural caveat in the header (decoration MERGES left
   *  out) does NOT apply to this tier — only to merges that DON'T change the key.
   *
   *  Curated from the 2026-06 corpus where one film fragments across many rows
   *  that never share showtimes — "Mandalorian i Grogu" split ~19 ways by a
   *  lower-case "Gwiezdne wojny:" prefix the seed rule (capitalised only) missed,
   *  the English title, and trailing language/format suffixes left in the title by
   *  cinemas whose scraper doesn't run `ScraperParse.extractFormatTags`. */
  val canonical: Seq[TitleRule] = Seq(
    canon("xtra-canonical-gwiezdne-wojny-ci",
      """(?iu)^Gwiezdne\s+wojny\s*:\s*""", "",
      "Case-insensitive 'Gwiezdne wojny:' franchise prefix — the seed 'canonical-gwiezdne-wojny' only matches the capitalised 'Gwiezdne Wojny:', so the lower-case spelling (Mandalorian i Grogu) never merged."),
    canon("xtra-canonical-trailing-lang-format",
      """(?iu)(?:\s+|\s*[\[/|.,–—-]\s*)(?:(?:2D|3D|4DX)\s*[/-]?\s*(?:ukrai[ńn]ski|ukrainian)?\s*(?:dubbing|napisy|lektor|dub|nap|lek)|(?:ukrai[ńn]ski|ukrainian)?\s*(?:dubbing|napisy|lektor))\s*\]?\s*$""",
      "",
      "Trailing language/format suffix glued to the title ('… 2D DUBBING', '… / napisy', '… - 2D DUB', '… [napisy]', '… ukraiński dubbing', '… – LEKTOR') — merges the 2D/3D × dub/napisy/lektor variants a non-extractFormatTags scraper left in the title. The suffix must follow a space/separator (so a bare 'Lektor'/'Napisy' title isn't eaten) and the short 'dub/nap/lek' abbreviations only strip behind a 2D/3D qualifier."),
    canon("xtra-canonical-trailing-sound-paren",
      """(?iu)\s*[\[(]\s*(?:dolby(?:\s+atmos)?|atmos|imax|4dx|2d|3d)\s*[\])]\s*$""", "",
      "Trailing parenthesised sound/format tag ('(Dolby Atmos)', '[2D]', '(IMAX)') — never part of a film's identity, so the format-decorated row merges into the bare film."),
    canon("xtra-canonical-trailing-paren-lang",
      """(?iu)\s*[\[(]\s*(?:(?:ukrai[ńn]ski\s+)?(?:dubbing|napisy|lektor|dub|nap)|wersja\s+oryginalna)(?:\s+pl)?\s*[\])]\s*$""", "",
      "Trailing PARENTHESISED language/format tag ('(Dubbing PL)', '(Napisy PL)', '(lektor)', '[dubbing PL]') — the parenthesised sibling of `xtra-canonical-trailing-lang-format`, which only handled the space/dash form. Merges e.g. 'Toy Story 5 (Dubbing PL)' and '500 mil (lektor)' into the base."),
    canon("xtra-canonical-trailing-org-wersja",
      """(?iu)\s*[/|.,–—-]\s*(?:wersja\s+oryginalna|org)\s*$""", "",
      "Trailing 'wersja oryginalna' / 'ORG' (original-language, no-dub) marker after a separator ('Toy Story 5 - wersja oryginalna', 'Toy Story 5 - ORG'). Requires a separator so a film that merely ends in those letters isn't eaten."),
    canon("xtra-canonical-trailing-paren-year",
      """(?iu)\s*[\[(]\s*(?:19|20)\d{2}\s*[\])]\s*$""", "",
      "Trailing parenthesised release year ('(1991)', '(1976)') glued into the title by a scraper — never part of the film's identity, so 'Milczenie owiec (1991)' merges into 'Milczenie owiec'. Only the PARENTHESISED form, so a bare year that IS the title ('1917', '2046') survives."),
    canon("xtra-canonical-mandalorian-grogu-en",
      """(?iu)^The\s+Mandalorian\s+and\s+Grogu$""", "Mandalorian i Grogu",
      "Map the English title to the Polish canonical (same TMDB id 1228710) so the EN-titled listing merges; ordered after the trailing-format strip clears its '2D DUB' suffix first.")
  )

  /** Per-cinema (client) cleanups — venue-specific junk too narrow to globalise
   *  safely, scoped to the one cinema's slug. PerCinema runs in `cinemaClean` at
   *  INGESTION (like the seed per-client `cleanTitle`), changing the display +
   *  merge key, so variants collapse into one rated row. Each requires the owning
   *  client to call `TitleNormalizer.cinemaClean("<slug>", …)` (wired in this
   *  change). Fourth-wave (2026-06-20). */
  val perCinemaRules: Seq[TitleRule] = Seq(
    perCinema("xtra-bajka-sps",        "kino-bajka",   """(?iu)\s+(?:2D|3D)\s+(?:DUB|NAP)\.?\s+SPS\s*$""", "Kino Bajka '… 2D DUB. SPS' screening-code suffix (Toy Story 5, Vaiana)"),
    perCinema("xtra-cyfrowe-premiera", "cyfrowe-kino", """(?i)^Premiera!\s+""",                            "Cyfrowe Kino 'Premiera! <film>' prefix"),
    perCinema("xtra-kijow-napisy-pl",  "kino-kijow",   """(?iu)\s+(?:UA|UKR)?\s*Napisy\s+PL\s*$""",        "Kino Kijów '… [UA/UKR] Napisy PL' subtitle suffix (Diabeł …Prady 2, Mawka, On drive)"),
    // Shared-portal venues, cleaned via the owning client's Cinema.slug:
    //   Oskard → Bilety24Client, Stary Młyn → Bilety24OrganizerClient,
    //   Na Starówce + Farys → SystemBiletowyClient.
    perCinema("xtra-oskard-kino-cafe",  "kino-oskard",     """(?i)\s*/\s*(?:dubbing\s*/\s*)?Kino\s+Cafe\s*$""", "Kino Oskard '… /Kino Cafe' (and '/dubbing/Kino Cafe') venue suffix (Following, Robin Hood, Drugie życie, Supergirl, Toy Story 5)"),
    perCinema("xtra-starowce-akcja-lato", "kino-na-starowce", """(?i)\s*(?:[-–—]\s*)?(?:film\s+)?akcja\s+lato\s+w\s+kinie\s*$""", "Na Starówce '… [- film] akcja lato w kinie' campaign suffix (Toy Story 5, Vaiana)"),
    perCinema("xtra-starymlyn-sensoryczny", "kino-stary-mlyn", """(?i)\s+sensoryczny\s*$""",                   "Kino Stary Młyn '… sensoryczny' sensory-screening suffix (Toy Story 5)"),
    perCinemaReplace("xtra-farys-tot-story", "kino-farys", """(?i)^Tot\s+story\s+5$""", "Toy Story 5",        "Kino Farys source typo 'Tot story 5' → 'Toy Story 5'")
  )

  /** Orders stamped by position so the additions fold AFTER the seed rules of
   *  their scope (matching how [[ApplyExtraTitleRules]] appends them to the
   *  existing record). */
  val all: Seq[TitleRule] =
    (programmePrefixes ++ searchStrips ++ canonical ++ perCinemaRules).zipWithIndex.map {
      case (r, i) => r.copy(order = 100 + i)
    }
}
