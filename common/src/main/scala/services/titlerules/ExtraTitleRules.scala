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

  private def prog(id: String, pattern: String, note: String): TitleRule =
    TitleRule(id, GlobalStructural, None, pattern, "", applyAll = false, order = 0,
      tag = Some("programmePrefix"), note = Some(note))

  private def searchStrip(id: String, pattern: String, note: String): TitleRule =
    TitleRule(id, GlobalStructural, None, pattern, "", applyAll = false, order = 0, note = Some(note))

  /** Like [[searchStrip]] but keeps a capture group ($1) — for a banner whose
   *  FILM is wrapped in the match (e.g. a quoted film with a descriptor tail),
   *  where a pure strip can't isolate the title. Still GlobalStructural / query
   *  only, so the screening keeps its own decorated row. */
  private def searchReplace(id: String, pattern: String, replacement: String, note: String): TitleRule =
    TitleRule(id, GlobalStructural, None, pattern, replacement, applyAll = false, order = 0, note = Some(note))

  private def canon(id: String, pattern: String, replacement: String, note: String): TitleRule =
    TitleRule(id, Canonical, None, pattern, replacement, applyAll = false, order = 0, note = Some(note))

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
    prog("xtra-pp-kobieta-pelna-zycia", """(?iu)^Kobieta\s+Pełna\s+Życia:\s+""",             "'Kobieta Pełna Życia:' Fregata cycle (Książę)"),
    // Fifth-wave (2026-06-20) audit of the TMDB-no-match corpus: a dated
    // summer-screenings strand that prefixes ~8 well-rated classics, each its own
    // outdoor screening (own display row, query stripped to the bare film). The
    // trailing '(1984)' the series glues on stays in the query — TMDB resolves it
    // fine, same as the existing 'Noce Cabirii (1957)' search case.
    prog("xtra-pp-kino-letnie",        """(?iu)^KINO\s+LETNIE\s+\d{4}:\s+""",                "'KINO LETNIE 2026:' summer-cinema strand (Amadeusz, Requiem dla snu, Rambo, Norymberga, Dom Dobry, Przekleństwa niewinności, Jedna bitwa po drugiej)"),
    // Eighth-wave (2026-06-20) audit of the rating-less corpus (1165-title prod
    // snapshot replayed through the rule engine): festival / audience / club colon
    // banners that prefix the film (own display row, query stripped to the bare film).
    prog("xtra-pp-tnkf",               """^TNKF:\s+""",                                      "'TNKF: <film>' = Tydzień Nowego Kina Francuskiego abbreviation (~11 French titles: Guru, Mi Amor, Obcy, Nowa fala, Wielki Łuk, Windą na szafot, Wypadek fortepianowy, …) — sibling of the numbered 'xtra-pp-pnkf-prefix'"),
    prog("xtra-pp-pnkf-bare",          """(?iu)^Przegląd\s+Nowego\s+Kina\s+Francuskiego:\s+""", "'Przegląd Nowego Kina Francuskiego: <film>' un-numbered form (the seed 'xtra-pp-pnkf-prefix' needs a leading 'NN.') — ~8 titles (Guru, Obcy, Wielki Łuk, Windą na szafot, …)"),
    prog("xtra-pp-rok-z-marilyn",      """(?iu)^Rok\s+z\s+Marilyn\s+Monroe:\s+""",          "'Rok z Marilyn Monroe: <film>' retrospective (Książę i aktoreczka, Przystanek autobusowy, Pół żartem pół serio, Skłóceni z życiem, Słomiany wdowiec)"),
    prog("xtra-pp-rodzina-w-kinie",    """(?iu)^Rodzina\s+w\s+kinie:\s+""",                  "'Rodzina w kinie: <film>' family-screening strand (Belle, Chłopiec na krańcach świata, Kicia Kocia, K-popowe łowczynie demonów, Niesamowite przygody skarpetek 3)"),
    prog("xtra-pp-tani-wtorek",        """(?iu)^Tani\s+wtorek:\s+""",                        "'Tani wtorek: <film>' cheap-Tuesday promo (Czytając Lolitę w Teheranie, Drugie życie, Kumotry, Ojczyzna, Robin Hood)"),
    prog("xtra-pp-filmowe-popoludnie", """(?iu)^Filmowe\s+popołudnie\s+dla\s+dzieci:\s+""",  "'Filmowe popołudnie dla dzieci: <film>' kids-afternoon strand (Indianie i kowboje, Złoto)"),
    prog("xtra-pp-klasyk-w-kinie",     """(?iu)^Klasyk\s+w\s+kinie:\s+""",                   "'Klasyk w kinie: <film>' classics strand (Milczenie owiec, Rozmowa)"),
    prog("xtra-pp-seans-sensoryczny",  """(?iu)^Seans\s+Przyjazny\s+Sensorycznie:\s+""",     "'Seans Przyjazny Sensorycznie: <film>' sensory-friendly screening (Willow i tajemniczy las, Chłopiec na krańcach świata)"),
    prog("xtra-pp-fiesta-hiszpanskiego", """(?iu)^FIESTA\s+KINA\s+HISZPAŃSKIEGO:\s+""",      "'FIESTA KINA HISZPAŃSKIEGO: <film>' Spanish-cinema fiesta prefix (Prawo pożądania); the '| / – fiesta …' suffix forms are handled by xtra-fiesta-hiszpanskiego-suffix"),
    // Chain recurring-programme banners that were per-cinema (client `cleanTitle`)
    // seeds in TitleRuleDefaults — promoted here so the strip is GLOBAL (any cinema
    // that ships the same banner resolves the base film), query-only so the
    // decorated screening keeps its own row rather than merging. `(?iu)` for the
    // Multikino ALL-CAPS raw + Polish letters; `{{SEP}}` for the ':'/'-' the chains
    // use interchangeably.
    prog("xtra-pp-kino-na-obcasach",   """(?iu)^Kino\s+na\s+obcasach{{SEP}}""",              "'Kino na obcasach: <film>' Multikino ladies'-programme banner (Zaproszenie, Diabeł ubiera się u Prady 2, Drugie życie)"),
    prog("xtra-pp-ladies-night",       """(?i)^Ladies\s+Night{{SEP}}""",                     "'Ladies Night - <film>' Cinema City ladies'-programme banner"),
    prog("xtra-pp-mamoru-hosody",      """(?iu)^Kolekcja\s+Mamoru\s+Hosody{{SEP}}""",        "'Kolekcja Mamoru Hosody: <film>' anime-retrospective banner (Cinema City + Multikino) — sibling of the 'Hosoda. <film>' dot-prefix"),
    prog("xtra-pp-dzien-dziecka-apollo", """(?iu)^DZIEŃ\s+DZIECKA\s+W\s+APOLLO{{SEP}}""",     "'DZIEŃ DZIECKA W APOLLO - <film>' Kino Apollo Children's-Day banner")
  )

  /** Strips that fix enrichment without merging the row away — a premiere or a
   *  DKF screening keeps its own line, it just resolves ratings now. */
  val searchStrips: Seq[TitleRule] = Seq(
    // Normalise Polish typographic quotes to plain ASCII (GlobalStructural,
    // replaceAll so every quote in the title is folded).
    TitleRule("search-quote-right", GlobalStructural, None, "”", "\"", applyAll = true, order = 0,
      note = Some("Polish right double quote ” → plain ASCII \"")),
    TitleRule("search-quote-low9", GlobalStructural, None, "„", "\"", applyAll = true, order = 0,
      note = Some("Polish low-9 double quote „ → plain ASCII \"")),
    searchStrip("xtra-dkf-suffix-pipe-underscore", """(?i)\s*[|_]\s*DKF\b.*$""",                      "'| DKF' / '_DKF' suffix"),
    searchStrip("xtra-dkf-suffix-dash",            """(?i)\s*[-–—]\s*DKF\b.*$""",                     "'- DKF KOT' / '- DKF III W' suffix"),
    searchStrip("xtra-dyskusyjny-suffix",          """(?i)\s*[-–—]\s*dyskusyjny\s+klub\s+filmowy\s*$""", "'- dyskusyjny klub filmowy' suffix"),
    searchStrip("xtra-przedpremiera-suffix",       """(?i){{SEP}}(?:przedpremiera|przedpremierowo|zobacz\s+przedpremierowo|seans\s+przedpremierowy|przepdremiera)\s*$""", "przedpremiera suffix (incl. the 'przepdremiera' data-entry transposition)"),
    searchStrip("xtra-przedpremiera-prefix",       """(?i)^(?:przedpremiera|seans\s+przedpremierowy)\s*[.|:]\s*""", "przedpremiera prefix"),
    searchStrip("xtra-accessibility-star-ad",      """(?i)\s*\*\s*AD\b\s*$""",                        "'*AD' audio-description tag"),
    // Festival / retrospective banners that pipe-wrap the film (Kinoteka) or
    // brand it with the touring "Federico Fellini: ciao a tutti!" cycle (~12
    // cinemas, inconsistent shapes). Query-only strips so each screening keeps
    // its own decorated row but finally resolves the bare film.
    searchStrip("xtra-wtf-fest-prefix",            """(?i)^WTF\s+Fest\s*\|\s*""",                     "'WTF Fest | <film>' banner (film after the pipe)"),
    searchStrip("xtra-pipe-festival-suffix",       """(?i)\s*\|\s*(?:6\s+razy\s+Pedro|Kino\s+cyrkularne)\b.*$""", "'<film> | 6 razy Pedro / Kino cyrkularne …' (film before the pipe)"),
    searchStrip("xtra-fellini-prefix",             FelliniPrefix, "'Federico Fellini: ciao a tutti! …' / 'Fellini. [Ciao a tutti:] <film>' retrospective prefix (also folded in the canonical tier — see FelliniPrefix)"),
    searchStrip("xtra-fellini-suffix",             FelliniSuffix, "'… | / – przegląd FEDERICO FELLINI …' retrospective suffix"),
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
    searchStrip("xtra-kino-kobiet-suffix",         """(?iu)\s*[-–—]\s*Kino\s+Kobiet\s*$""",           "'<film> - Kino Kobiet' Helios ladies'-programme suffix (promoted from the helios per-cinema event tag; sibling of the 'Kino na obcasach' / 'Ladies Night' prefix banners)"),
    searchStrip("xtra-kntj-suffix",                """(?i)\s*[-–—]\s*KNTJ\s*$""",                     "'<film> - KNTJ' cross-cinema strand suffix"),
    searchStrip("xtra-pokaz-suffix",               """(?i)\s*[-–—|]\s*pokaz\b.*$""",                  "'<film> - / | pokaz <specjalny|przedpremierowy|+ dyskusja…>' event suffix"),
    searchStrip("xtra-tadeusz-konwicki-suffix",    """(?i)\s*[-–—]\s*tadeusz\s+konwicki\b.*$""",      "'<film> – Tadeusz Konwicki / 100. rocznica urodzin' suffix"),
    searchStrip("xtra-wajda-o-filmie-suffix",      """(?i)\s*[-–—]\s*Andrzej\s+Wajda\s+o\s+filmie\s*$""", "'<film> - Andrzej Wajda o filmie' suffix (Brzezina)"),
    // Fourth-wave (2026-06-20) audit of the rating-less corpus.
    // Director-retrospective DOT prefixes ('<Director>. <film>'), siblings of the
    // existing 'Wajda.' / 'Konwicki:' rules; each merges into an already-RATED row.
    // ('Fellini. <film>' is folded by the shared FelliniPrefix above, query + key.)
    searchStrip("xtra-hosoda-dot-prefix",          """(?iu)^Hosoda\.\s+""",                           "'Hosoda. <film>' Mamoru Hosoda retrospective prefix (Wilcze dzieci rated; Summer Wars, Belle, Scarlet)"),
    searchStrip("xtra-konwicki-dot-prefix",        """(?iu)^Konwicki\.\s+""",                         "'Konwicki. <film>' DOT variant of the existing colon prefix (Ostatni dzień lata, Salto, Lawa)"),
    // Cross-cinema decoration suffixes (banner after the film); query-only strip.
    // ('+ prelekcja' / '+ wstęp' is already stripped by the seed PlusSuffix rule.)
    searchStrip("xtra-kf-klub-suffix",             """(?i)\s*\|\s*KF\s+\S.*$""",                      "'<film> | KF <klub>' film-club suffix (Fellini. Noce Cabirii | KF Ambasada)"),
    searchStrip("xtra-wtorek-seniora-suffix",      """(?i)\s*\|\s*Wtorek\s+Seniora\s*$""",            "'<film> | Wtorek Seniora' senior-screening suffix (Ojczyzna)"),
    searchStrip("xtra-fks-suffix",                 """(?iu)\s*[|_]\s*FKS\s*$""",                      "'<film>_FKS' / '<film> | FKS' Filmowy Klub Seniora suffix (Takie jest życie, 500 Mil, Posłani)"),
    searchStrip("xtra-pokazy-specjalne-suffix",    """(?i)\s*[-–—|]\s*pokazy\s+specjalne\s*$""",      "'<film> - pokazy specjalne' suffix (the xtra-pokaz-suffix rule's pokaz\\b can't match the 'pokazy' plural) (Milczenie owiec)"),
    searchStrip("xtra-wakacje-z-dokumentem-suffix", """(?i)\s*\|\s*Wakacje\s+z\s+dokumentem\s*$""",   "'<film> | Wakacje z dokumentem' documentary-strand suffix (Silver)"),
    // Fifth-wave (2026-06-20) audit of the TMDB-no-match corpus.
    // A Ukrainian-language screening is a DISTINCT version (separate audience), so
    // — exactly like the canonical UA exclusion below — it must KEEP its own row
    // and merge key. But a query-only strip still lets it resolve the base film's
    // TMDB id/poster/ratings: '<film> ukraiński dubbing' → '<film>'. Covers the
    // bare-space, dot ('Straszny film. Ukrainian dubbing') and parenthesised
    // ('… (ukraiński dubbing)') shapes, and the English 'ukrainian' spelling. ~7
    // major films × the Cinema City network (Toy Story 5, Spider-Man, Odyseja,
    // Minionki i straszydła, Dzień objawienia, Straszny film).
    searchStrip("xtra-ukrainski-lang-suffix",      """(?iu)\s*[.(\s]\s*(?:ukrai[ńn]ski|ukrainian)\s+(?:dubbing|napisy|lektor)\s*\)?\s*$""", "'<film> [.( ]ukraiński/ukrainian dubbing/napisy/lektor' UA-version marker — query-only strip so the row stays its own (matching the canonical UA exclusion) but resolves the base film"),
    // '<film> reż. <director>' / '<film> reżyseria: <director>' authorship tag
    // appended by a few art-house venues; query-only strip resolves the bare film
    // (Perfect Days, Droga do Vermiglio, Za duży na bajki 3). Anchored on a
    // separator + a following name so a real title can't be amputated.
    searchStrip("xtra-rezyseria-suffix",           """(?iu)\s+re[zż](?:yseria)?\s*[.:]\s+\S.*$""", "'<film> reż. / reżyseria: <director>' authorship suffix (Perfect Days, Droga do Vermiglio, Za duży na bajki 3)"),
    // Bare 'DKF - ' / 'DKF: ' film-club prefix (no club name) — the existing
    // 'xtra-pp-dkf-named' only matches 'DKF <name>: ', so a clubless 'DKF: <film>'
    // / 'DKF - <film>' never stripped. Query-only (Drugie życie, Czytając Lolitę
    // w Teheranie). Anchored so 'DKF Kropka: …' still falls to the named prog rule.
    searchStrip("xtra-dkf-bare-prefix",            """(?iu)^DKF\s*[-–—:]\s+""",                  "'DKF - / DKF: <film>' clubless film-club prefix (Drugie życie, Czytając Lolitę w Teheranie)"),
    searchStrip("xtra-filmowy-klub-seniora-dash",  """(?iu)^Filmowy\s+Klub\s+Seniora\s*[-–—]\s+""", "'Filmowy Klub Seniora - <film>' dash variant of the seed senior-club banner (Drugie życie)"),
    // Sixth-wave (2026-06-20) audit. Only rules whose stripped query was VERIFIED
    // to resolve unambiguously on TMDB are kept — most residual no-match titles
    // either don't decode to a TMDB film (yearless titles that map to two films,
    // e.g. 'Substancja' = The Substance AND The Stuff) or are genuinely absent
    // (Ostatni konsjerż), so a strip there would be theatre.
    // 'Żywot Briana Grupy Monty Pythona. Wersja zremasterowana' (~30 Multikino
    // rows) — strip the studio attribution + remaster tail so the query is the
    // bare 'Żywot Briana', which is the UNIQUE TMDB result (583, Life of Brian).
    searchStrip("xtra-zywot-briana-monty-suffix",  """(?iu)\s+Grupy\s+Monty\s+Pythona\b.*$""",  "'Żywot Briana Grupy Monty Pythona. Wersja zremasterowana' (~30 Multikino rows) → 'Żywot Briana' (TMDB 583, unique)"),
    // Re-release / restoration banner appended to a classic ('… - PONOWNIE NA
    // WIELKIM EKRANIE', '… Wersja zremasterowana/odrestaurowana'); query-only
    // strip resolves the bare film (Żywot Briana at Kinoteatr Rialto).
    searchStrip("xtra-rerelease-suffix",           """(?iu)\s*[-–—|.]\s*(?:ponownie\s+na\s+wielkim\s+ekranie|wersja\s+(?:zremasterowan\w*|odrestaurowan\w*|zrekonstruowan\w*))\s*$""", "'<film> - PONOWNIE NA WIELKIM EKRANIE / Wersja zremasterowana' re-release/restoration suffix (Żywot Briana)"),
    // '<film> - <director> 4K' restoration-print suffix (Kino Kultura's Wong Kar
    // Wai retrospective). 'Chungking Express' is the unique exact-title TMDB hit.
    searchStrip("xtra-wong-kar-wai-suffix",        """(?iu)\s*[-–—]\s*Wong\s+Kar[\s-]?Wai\b.*$""", "'<film> - Wong Kar Wai 4K' restoration-print suffix (Chungking Express → TMDB 11104)"),
    // Eighth-wave (2026-06-20) audit of the rating-less corpus: pipe/paren decoration
    // suffixes whose film stays its own decorated row (query-only strip).
    searchStrip("xtra-kino-przy-herbatce",         """(?iu)\s*\|\s*Kino\s+przy\s+herbatce\s*$""",     "'<film> | Kino przy herbatce' cosy-screening suffix (Drugie życie, Dzień objawienia, Wolność po włosku)"),
    searchStrip("xtra-teleskop-suffix",            """(?iu)\s*\|\s*Teleskop\s*$""",                   "'<film> | Teleskop' cine-club suffix (Following (Śledząc), Frances Ha, Sny o pociągach)"),
    searchStrip("xtra-lato-robin-williams",        """(?iu)\s*\|\s*Lato\s+z\s+Robinem\s+Williamsem\b.*$""", "'<film> | Lato z Robinem Williamsem [+ …]' actor-cycle suffix (Fisher King, Klatka dla ptaków, Stowarzyszenie umarłych poetów)"),
    searchStrip("xtra-kinoteka-dla-rodzica",       """(?iu)\s*\|\s*Kinoteka\s+dla\s+rodzic\S*\s*$""", "'<film> | Kinoteka dla rodziców/rodzica' parents'-screening suffix (Erupcja, Projekt Hail Mary, Zaproszenie)"),
    searchStrip("xtra-fiesta-hiszpanskiego-suffix", """(?iu)\s*[|–—-]\s*fiesta\s+kina\s+hiszpańskiego\b.*$""", "'<film> | / – Fiesta Kina Hiszpańskiego[: almodóvar/banderas]' Spanish-cinema suffix (Matador, Prawo pożądania, Kobiety na skraju załamania nerwowego); prefix form is xtra-pp-fiesta-hiszpanskiego"),
    searchStrip("xtra-amondo-grindhouse",          """(?iu)\s*\|\s*AMONDO\s+GRINDHOUSE\s*$""",        "'<film> | AMONDO GRINDHOUSE' cult-strand suffix (Santa Sangre, Sleepaway Camp)"),
    // Trailing PARENTHESISED '(pokaz …)' / '(seans …)' event note — the seed pokaz/
    // przedpremiera strips only catch a dash/pipe separator, not a paren. Strips from
    // the open paren to end, so the truncated '(seans' forms fold too.
    searchStrip("xtra-paren-pokaz-seans",          """(?iu)\s*\(\s*(?:pokaz|seans)\b.*$""",           "'<film> (pokaz jednorazowy/przedpremierowy)' / '(seans z prelekcją)' paren event-note suffix (Erupcja, Ojczyzna, Zawieście czerwone latarnie, Młode matki, Ostatni dzień lata)"),
    // QUOTED film whose tail is a banner ('"<film>" - UROCZYSTA POLSKA PREMIERA',
    // '"<film>" | specjalny pokaz …', '"<film>" pokaz przedpremierowy w ramach …').
    // The closing quote + a banner keyword (premiera/gala/pokaz/seans/specjalny/
    // uroczysta/'w ramach') anchor the extraction; the closing quote is REQUIRED so a
    // bare title can't match. Runs BEFORE the cycle-dash + w-ramach rules below so it
    // wins on this inverse shape (quoted part = film, not cycle). Open quote optional:
    // a seed rule strips a leading „ before this runs, leaving '<film>" - …'.
    searchReplace("xtra-quoted-film-banner-tail",  """(?iu)^[„"]?\s*([^„""]+?)\s*[„""]\s+(?:[|–—-]\s*)?(?:premiera|prapremiera|gala|pokaz|seans|specjaln|uroczysta|w\s+ramach).*$""", "$1", "'\"<film>\" - UROCZYSTA POLSKA PREMIERA' / '\"<film>\" | specjalny pokaz …' inverse of xtra-quoted-cycle-dash: quoted part is the FILM, banner tail dropped (Backrooms. Bez wyjścia, Drugie życie, Wędrówka na północ)"),
    // Seventh-wave (2026-06-20) audit: QUOTED cycle/series banners. Both keep their
    // own decorated display row (query-only strip), but resolve the bare film.
    // Two opposite shapes, disjoint patterns (one keys on a dash after the closing
    // quote, the other on a 'w ramach cyklu' descriptor), so they never collide:
    //
    //  (a) '"<cycle>" - <film>' — the QUOTED part is the cycle banner and the film
    //      follows the dash. Strip the quoted prefix + dash, generic over the banner
    //      so any future '"<cycle>" - …' series folds (the dash convention matches
    //      the existing 'Pedro Almodóvar: Kolory emocji - …' / 'Wielka Sztuka … - …'
    //      strips). Matches both ASCII " and the Polish „…" pair.
    // The negative look-ahead is the safety guard for the INVERSE shape, where the
    // quoted part is the FILM and the dash introduces a premiere/gala banner
    // ('"Backrooms. Bez wyjścia" - UROCZYSTA POLSKA PREMIERA'): if a premiere/gala/
    // pokaz/seans word follows the dash (within ~2 tokens), this is NOT a cycle
    // banner, so DON'T strip the film. The xtra-quoted-film-banner-tail rule above
    // extracts the film from that inverse shape instead.
    searchStrip("xtra-quoted-cycle-dash",          """(?iu)^[„"][^„""]+[„""]\s*[-–—]\s+(?!(?:\S+\s+){0,2}(?:premiera|prapremiera|gala|pokaz|seans)\b)""", "'\"<cycle>\" - <film>' quoted cycle/series banner prefix ('\"Kultowe Wakacje\" - Amelia (2001)', '\"Kultowe Wakacje\" - Milczenie Owiec')"),
    // (b) '"<film>" w ramach cyklu <cycle>' — the OPPOSITE: the quoted part is the
    //     FILM and 'w ramach cyklu <cycle> …' is the descriptor tail. Keep the
    //     captured film ($1) and drop the wrapping quotes + the whole tail, so it
    //     resolves as the bare film. The closing quote / literal 'w ramach cyklu'
    //     anchor the lazy capture, so a film whose own title contains 'w …' is safe.
    searchReplace("xtra-quoted-film-w-ramach-cyklu", """(?iu)^[„"]?\s*(.+?)\s*[„""]?\s+w\s+ramach\s+cyklu\b.*$""", "$1", "'\"<film>\" w ramach cyklu <cycle>' — quoted film + cycle descriptor tail kept its own row, resolves the bare film ('\"Drugie życie\" w ramach cyklu SWPS — 2025' → 'Drugie życie')"),
    // Ninth-wave (2026-06-21) audit. The Kino Luna 'Filmoterapia w Lunie' therapy
    // series pipe-WRAPS the film between its banner and an event tail
    // ('Filmoterapia W Lunie | <film> | seans + rozmowa Martyny Harland'), so the
    // film is in the MIDDLE — neither the seed plus-suffix nor a pure prefix strip
    // isolates it. Capture the first pipe-delimited segment and drop the banner +
    // tail; query-only, so the therapy screening keeps its own decorated row but
    // resolves the bare film. Sibling of the 'Filmoterapia z Inspirą:' prog prefix.
    searchReplace("xtra-filmoterapia-w-lunie", """(?iu)^Filmoterapia\s+w\s+Lunie\s*\|\s*(.+?)\s*(?:\|.*)?$""", "$1", "'Filmoterapia W Lunie | <film> | seans + rozmowa Martyny Harland' Kino Luna therapy series — film pipe-wrapped in the middle (O czym sobie nie mówmy → TMDB 1473635)"),
    // Tenth-wave (2026-06-23) audit of the rating-less prod corpus (402 / 1141 rows
    // carry no ratings, 334 of them also TMDB-no-match — their SEARCH title still
    // wears a cinema banner the normaliser doesn't strip). Programme/cycle banners
    // that PREFIX a real, TMDB-resolvable film, plus decoration that SUFFIXES it.
    // Per the "search titles only" scope these are query-only strips (no tag, no
    // Canonical/PerCinema rule): the screening keeps its own decorated DISPLAY row
    // and merge key, it just resolves ratings/poster off the bare film. Each stripped
    // query was verified to resolve on TMDB before adding — a few targets (Amelia,
    // Zaproszenie, Drugie życie) are year-ambiguous title-only, but the strip itself
    // is correct and the year-scoped pipeline lookup disambiguates.
    searchStrip("xtra-lato-wakacje-klasyka",       """(?iu)^LATO\s*['’‘`]?\s*\d{2,4}\.?\s*Wakacje\s+z\s+Klasyką\s+Kina{{SEP}}""", "'LATO'26. Wakacje z Klasyką Kina - <film>' summer-classics strand (Casablanca, Amadeusz, Mulholland Drive, 2001: Odyseja kosmiczna, Amelia, Łowca androidów, Wielki błękit, Wielkie piękno, Piknik pod Wiszącą Skałą)"),
    searchStrip("xtra-wakacje-dla-dzieci",         """(?iu)^Wakacje\s+dla\s+dzieci{{SEP}}""",        "'Wakacje dla dzieci: <film>' kids-summer strand (Arco, Pucio, Basia mam swój świat, Kicia Kocia w podróży, Fleak. Futrzak i ja, Fantastyczny Angelo)"),
    searchStrip("xtra-lato-w-lunie",               """(?iu)^LATO\s+w\s+LUNIE{{SEP}}""",              "'LATO w LUNIE | <film>' Kino Luna summer strand (Drzewo magii, Chłopiec na krańcach świata, Willow i tajemniczy las, Ekipa zwierzaków, Superfutrzak i złośliwa wiewiórka)"),
    searchStrip("xtra-kobiece-strands",            """(?iu)^(?:Kino\s+dla\s+Kobiet|Babski\s+(?:wieczór|czwartek)|Kobiecy\s+świat|Kobiece\s+Wieczory\s+w\s+Kino\s+Cafe|Wieczory\s+filmowe\s+na\s+boku){{SEP}}""", "ladies'-night strands (Kino dla Kobiet / Babski wieczór / Babski czwartek / Kobiecy świat / Kobiece Wieczory w Kino Cafe / Wieczory filmowe na boku) — film after the banner separator (Drugie życie, Zaproszenie, Czytając Lolitę w Teheranie, Zupa nic)"),
    searchStrip("xtra-klasyka-cycles",             """(?iu)^Klasyk[ai]\s+(?:Kina|na\s+fali|w\s+Kulturze|w\s+NCKF){{SEP}}""", "'Klasyka Kina / Klasyka na fali / Klasyka w Kulturze / Klasyka w NCKF: <film>' classics strands (Milczenie owiec, Lot nad kukułczym gniazdem, La Strada, Przekleństwa niewinności) — siblings of the existing 'Klasyk w kinie:' rule"),
    searchStrip("xtra-filmy-z-lektorem",           """(?iu)^FILMY\s+Z\s+LEKTOREM{{SEP}}""",          "'FILMY Z LEKTOREM - <film>' dubbed-screening strand (Poprzednie życie, Emilia Pérez)"),
    searchStrip("xtra-smiech-przez-lzy",           """(?iu)^ŚMIECH\s+PRZEZ\s+ŁZY{{SEP}}""",          "'ŚMIECH PRZEZ ŁZY: <film>' comedy strand (Chłopaki nie płaczą, Wesele)"),
    searchStrip("xtra-wajda-dziedzictwo",          """(?iu)^Andrzej\s+Wajda\.\s+Dziedzictwo\s+Mistrza{{SEP}}""", "'Andrzej Wajda. Dziedzictwo Mistrza: <film>' Wajda retrospective (Popiół i diament)"),
    searchStrip("xtra-konkurs-pelnometrazowych",   """(?iu)^MIĘDZYNARODOWY\s+KONKURS\s+FILMÓW\s+PEŁNOMETRAŻOWYCH{{SEP}}""", "'MIĘDZYNARODOWY KONKURS FILMÓW PEŁNOMETRAŻOWYCH - <film>' festival full-length competition (Allah Is Not Obliged, Space Cadet → Nowa w kosmosie, The Square); the KRÓTKOMETRAŻOWYCH short-film SETS are left alone (they don't resolve to one film)"),
    // Decoration suffixes (banner after the film); query-only strip keeps the row.
    searchStrip("xtra-tani-dzien-suffix",          """(?iu)\s+tani\s+(?:poniedziałek|wtorek)\s*$""", "'<film> tani poniedziałek/wtorek' cheap-day suffix (Supergirl, Toy Story 5) — sibling of the 'Tani wtorek:' prefix"),
    searchStrip("xtra-bare-format-suffix",         """(?iu)\s+(?:2D|3D)\s*$""",                       "'<film> 2D/3D' bare screen-format suffix the seed/canonical format strips miss (they require a dub/napisy word or parens) — Supergirl 2D"),
    // Eleventh-wave (2026-06-23) audit by CONTAINMENT: a rating-less raw title that
    // contains an already-RATED film title as a substring proves the surrounding
    // text is strippable decoration (and the inner film is rated by construction).
    // Spurious substring hits ("Szlagierowy zawrót głowy" ⊅ Zawrót głowy/Vertigo,
    // "60. ROCZNICA PREMIERY" ⊅ the film 'Rocznica', the 'Minimaraton Superman i
    // Supergirl' double-feature, the 'NT Live:' theatre broadcast that must NOT
    // inherit the film's ratings) were reviewed out by hand. Query-only strips.
    searchStrip("xtra-maraton-prefix",             """(?iu)^Maraton{{SEP}}""",                       "'Maraton: <film>' marathon prefix (Powrót do przyszłości); NOT 'Minimaraton …' which fronts a double-feature bundle"),
    searchStrip("xtra-spotkania-filmowe-banner",   """(?iu)^(?:\p{L}+\s+)?Spotkania\s+Filmowe\b[^:|]*[:|]\s*""", "'[<adj>] Spotkania Filmowe [„<cycle>”] : / | <film>' film-meeting banner — the pipe form + a leading-adjective form the seed 'spotkani…:' prefix (anchored, colon-only) misses (Spotkania Filmowe | Ojczyzna; Psychoanalityczne Spotkania Filmowe „W głębi”: Czytając Lolitę w Teheranie)"),
    searchStrip("xtra-classy-monday-prefix",       """(?iu)^Classy\s+Monday{{SEP}}""",               "'Classy Monday - <film>' cycle prefix (Rambo: Pierwsza krew)"),
    searchStrip("xtra-fga-prefix",                 """(?iu)^\d+\.\s*FGA{{SEP}}""",                    "'19. FGA: <film>' film-festival numbered prefix (Szepty lasu)"),
    searchStrip("xtra-bkf-prefix",                 """(?iu)^BKF\s*#?\s*\d+\s+""",                     "'BKF #53 <film>' film-club numbered prefix (Chronologia wody)"),
    searchStrip("xtra-niedziela-z-dokumentem",     """(?iu)^Niedziela\s+z\s+Dokumentem{{SEP}}""",    "'Niedziela z Dokumentem: <film>' documentary-strand prefix (Dziecko z pyłu)"),
    searchStrip("xtra-nlecie-org-prefix",          """(?iu)^\d+[-–]lecie\s+{{NSEP}}{2,40}{{SEP}}""", "'70-lecie Wydawnictwa Poznańskiego: <film>' anniversary-of-an-organisation prefix; the name guard is {{NSEP}} so it stops at the banner separator (Wędrówka na północ)"),
    // Decoration suffixes (banner after the film).
    searchStrip("xtra-klub-filmowy-suffix",        """(?iu){{SEP}}(?:\p{L}+\s+){0,3}Klub\s+Filmowy\b.*$""", "'<film> <sep> <name> Klub Filmowy <…>' film-club suffix (Backrooms. Bez wyjścia - Młodzieżowy Klub Filmowy Leżak; Tajny agent | Filozoficzny Klub Filmowy) — subsumes the former 'xtra-filozoficzny-klub-suffix'"),
    searchStrip("xtra-capital-i-format-wrapper",   """\s+I\s+(?i:2D|3D|DUB(?:BING)?|NAP(?:ISY)?|LEKTOR|ATMOS|IMAX|4DX)\s+I\s*$""", "'<film> I 2D I' — a cinema that wraps the screen-format tag in literal capital-'I' separators instead of pipes (Backrooms. Bez wyjścia I 2D I). The 'I's are case-SENSITIVE so the Polish conjunction ' i ' can't trigger it"),
    searchStrip("xtra-ostatni-seans-suffix",       """(?iu){{SEPD}}Ostatni\s+seans\s*$""",           "'<film>. Ostatni seans' last-screening suffix (Rozmowa. Ostatni seans)"),
    searchStrip("xtra-czwartek-konesera-suffix",   """(?iu)\s+czwartek\s+konesera\s*$""",            "'<film> czwartek konesera' connoisseur-Thursday suffix (Werdykt czwartek konesera)"),
    searchStrip("xtra-dot-spotkanie-suffix",       """(?iu){{SEPD}}Spotkani\p{L}*\s+z\b.*$""",       "'<film>. Spotkanie z <person>' meeting suffix introduced by a PERIOD (or any separator) — the seed meeting-suffix only fires after a dash/pipe/plus (Człowiek z marmuru. Spotkanie z Michałem Tarkowskim)"),
    searchStrip("xtra-filmoteka-dojrzalego",       """(?iu){{SEP}}Filmoteka\s+Dojrzałego\s+Człowieka\s*$""", "'<film> - Filmoteka Dojrzałego Człowieka' mature-viewers'-strand suffix (Sprawiedliwość owiec)"),
    searchStrip("xtra-format-pl-suffix",           """(?iu)(?:{{SEP}})?(?:2D|3D)\s+(?:dubbing|napisy|lektor)\s+PL\s*$""", "'<film>- 2D Dubbing PL' screen-format + dub/napisy + PL suffix (the canonical format strips stop before the trailing ' PL') (Toy Story 5)"),
    // Twelfth-wave (2026-06-25) audit of the TMDB-no-match corpus: programme/series
    // banners that prefix the film and weren't yet covered. Query-only strips (own
    // display row kept); each target verified to resolve on TMDB after the strip.
    // (A 'po śląsku/ślōnsku' Silesian-dub suffix was tried here and DROPPED — that
    // phrase is part of the release's own title, not strippable decoration.)
    searchStrip("xtra-przeglad-filmow-prefix",     """(?iu)^Przegląd\s+filmów\s+{{NSEP}}+{{SEP}}""", "'Przegląd filmów <reżyser> - <film>' retrospective PREFIX — sibling of the existing '<film> - przegląd filmów <reż>' SUFFIX rule; the {{NSEP}} guard eats the director name and stops at the banner separator (Ziemia obiecana, Powidoki, Brzezina, Człowiek z marmuru)"),
    searchStrip("xtra-filmowe-wakacje-za-rogiem",  """(?iu)^Filmowe\s+wakacje\s+za\s+Rogiem{{SEP}}""", "'Filmowe wakacje za Rogiem: <film>' Kino za Rogiem kids-summer strand (Koszmarek, Pies i robot, Pan Zabawka, Legenda Ochi, Skrzat. Nowy początek, Zmiennokształtni, Fantastyczny Angelo, O psie który jeździł koleją 2)"),
    searchStrip("xtra-filmowe-lato",               """(?iu)^Filmowe\s+lato{{SEP}}""",                "'Filmowe lato: <film>' summer strand (Toy Story 5)"),
    searchStrip("xtra-wakacje-w-kinie",            """(?iu)^Wakacje\s+w\s+[Kk]inie(?:\s+Orzeł)?{{SEP}}""", "'Wakacje w Kinie Orzeł: / Wakacje w kinie: <film>' kids-summer strand (Drzewo magii, Chłopiec na krańcach świata, Anzu. Kot-duch, Mała Amelia, Yuku i magiczny kwiat) — sibling of 'Wakacje dla dzieci:'"),
    // Thirteenth-wave (2026-06-25) CONTAINMENT audit: rating-less rows whose
    // rule-cleaned query still WRAPS an already-rated film as a word-boundary
    // substring → the surrounding text is strippable decoration no rule caught.
    // Each revealed film is rated in prod (so the strip lets the decorated
    // screening inherit that rating). The blind-substring traps the audit surfaced
    // are deliberately NOT stripped — they're banner-anchored, never bare contains:
    //   "Minimaraton Supergirl & Superman" (a bundle, not Supergirl), "Szlagierowy
    //   zawrót głowy" (⊅ Vertigo), "60. ROCZNICA premiery" (⊅ the film 'Rocznica'),
    //   "BACKROOM: Bez wyjścia" (the film IS 'Backrooms. Bez wyjścia') — see the
    //   negative controls in ExtraTitleRulesSpec.
    searchStrip("xtra-pp-ffs-festival",            """(?iu)^\d+\s*FFS\s+""",                          "'26 FFS <film>' film-festival edition prefix (Być kochaną, Ostatni wiking, Wartość sentymentalna, Świat po pracy, Wiking i magiczny miecz)"),
    searchStrip("xtra-dub-sps-suffix",             """(?iu)\s+(?:2D|3D)\s+(?:DUB|NAP)\.?\s+SPS\s*$""", "'<film> 2D DUB. SPS' screening-code suffix — the global form of the kino-bajka per-cinema rule (other venues use it too: Toy Story 5, Vaiana)"),
    searchStrip("xtra-premiera-prefix",            """(?iu)^Premiera\s*[:!]+\s*""",                   "'Premiera: <film>' / 'PREMIERA!!! <film>' release-announcement prefix (Kumotry, Straszny film) — distinct from the existing przedpremiera prefix"),
    searchStrip("xtra-dialog-przez-film",          """(?iu)^Dialog\s+przez\s+Film:\s*""",             "'Dialog przez Film: <film>' discussion-cycle prefix (Co do... Kury?)"),
    searchStrip("xtra-mistrzowska-kreska",         """(?iu)^Mistrzowska\s+Kreska:\s*""",              "'Mistrzowska Kreska: <film>' animation-cycle prefix (Podwójne życie Weroniki)"),
    searchStrip("xtra-najlepsze-dash-prefix",      """(?iu)^Najlepsze\s+z\s+Najgorszych\s*[-–—]\s*""", "'Najlepsze z Najgorszych - <film>' bad-movie-night DASH prefix — sibling of the existing colon-only rule (Big Shark, Brudny Henryk, The Room)"),
    searchStrip("xtra-najlepsze-suffix",           """(?iu){{SEP}}Najlepsze\s+z\s+Najgorszych\s*$""", "'<film> | Najlepsze z Najgorszych' bad-movie-night SUFFIX form (Brudny Henryk)"),
    searchStrip("xtra-sztuka-na-ekranie",          """(?iu)^Sztuka\s+na\s+ekranie\s*[-–—]\s*""",     "'Sztuka na ekranie - <film>' art-doc strand (Caravaggio. Arcydzieła niepokornego geniusza) — sibling of Wielka Sztuka w Kinoteatrze Rialto"),
    searchStrip("xtra-exhibition-on-screen",       """(?iu)^Exhibition\s+On\s+Screen:\s*""",          "'Exhibition On Screen: <film>' art-doc series (David Hockney. Pejzaże, portrety i martwe natury)"),
    searchStrip("xtra-poranki-dzieciece-suffix",   """(?iu){{SEP}}Poranki\s+dziecięce\s*$""",         "'<film> - Poranki dziecięce' kids-morning suffix (Minionki i straszydła)"),
    // Fourteenth-wave (2026-06-25) audit of the TMDB-no-match corpus via the
    // resolve-by-synopsis report: programme-cycle banners that PREFIX a real,
    // TMDB-resolvable film and weren't yet covered. Query-only strips (the
    // screening keeps its own decorated display row); each stripped query was
    // verified to return a TMDB hit. The broadcast/concert/festival-compilation
    // prefixes the same audit surfaced (NT Live:, Royal Ballet and Opera …:,
    // Pavarotti concerts, Animator/Annecy compilations, Cirque du Soleil:, Sia:,
    // 'Seans w ciemno:' surprise-screenings) are DELIBERATELY left alone — they're
    // distinct entities, not a banner over one film, and resolving them to the
    // underlying play/film is wrong (a ballet 'Manon' is not Pagnol's 'Manon').
    searchStrip("xtra-kinowy-poranek",             """(?iu)^Kinowy\s+Poranek{{SEP}}""",              "'Kinowy Poranek: <film>' kids-morning strand (Lato, kiedy nauczyłam się latać → TMDB) — distinct from the seed 'Filmowy/Zimowe Poranki'"),
    searchStrip("xtra-przyblizenia-psychoanaliza", """(?iu)^Przybliżenia\s+[-–—]\s+okiem\s+psychoanalizy{{SEP}}""", "'Przybliżenia - okiem psychoanalizy: <film>' psychoanalysis cycle (Perfect Days)"),
    searchStrip("xtra-kino-bez-barier-dzieci",     """(?iu)^Kino\s+bez\s+barier\s+dla\s+dzieci{{SEP}}""", "'Kino bez barier dla dzieci: <film>' accessibility kids-strand — the seed 'Kino bez barier:' wants the colon right after 'barier', so this longer form never matched (Oskar, Patka i złoto Bałtyku)"),
    // Fifteenth-wave (2026-06-29) centralisation pass: GENERIC screening-cycle /
    // festival banners — a colon-terminated PREFIX or a keyword-guarded pipe SUFFIX —
    // each generalising a shape the curated per-banner rules only ever covered for one
    // named cycle/venue. Query-only strips (the decorated screening keeps its own
    // display row + merge key), so the bare film resolves before any external resolver
    // runs. `{{NSEP}}+:` is the established banner-name guard (it stops at the first
    // hard separator, like the DKF/Klub Filmowy prefixes), anchored on an explicit
    // ':' so only a colon-terminated cycle prefix is stripped.
    searchStrip("xtra-poniedzialki-konwicki-prefix", """(?iu)^Poniedziałki\s+z\s+{{NSEP}}+:\s+""", "'Poniedziałki z Konwickim: <film>' Mondays-with-<author> cycle PREFIX — the existing 'xtra-poniedzialki-konwicki' only caught the '| Poniedziałki z Konwickim' SUFFIX (Lawa, Salto, Dolina Issy)"),
    searchStrip("xtra-klasyka-w-kinie-prefix",      """(?iu)^Klasyka\s+w\s+kinie:\s+""", "'Klasyka w kinie: <film>' classics strand — the bare colon form the venue-specific 'Klasyka w kinie Atlantic:' programme rule doesn't reach"),
    searchStrip("xtra-cykl-prefix",                 """(?iu)^Cykl\s+{{NSEP}}+:\s+""", "'Cykl <name>: <film>' generic programme-cycle prefix (Cykl Filmowy:, Cykl Dokumentu:) — sibling of the DKF/Klub Filmowy named-cycle prefixes"),
    searchStrip("xtra-przeglad-colon-prefix",       """(?iu)^Przegląd\s+{{NSEP}}+:\s+""", "'Przegląd <name>: <film>' generic festival/review COLON prefix — complements the dash-form 'Przegląd filmów <reż> - <film>' rule (Przegląd Nowego Kina Francuskiego:, Przegląd Kina Hiszpańskiego:)"),
    searchStrip("xtra-filmowe-lato-named",          """(?iu)^Filmowe\s+lato\s+{{NSEP}}+:\s+""", "'Filmowe Lato <name>: <film>' named summer strand — the bare 'Filmowe lato:' rule needs the colon right after 'lato' (Filmowe Lato w Kinie:)"),
    searchStrip("xtra-wakacje-z-klasyka-prefix",    """(?iu)^Wakacje\s+z\s+klasyką(?:\s+{{NSEP}}+)?:\s+""", "'Wakacje z klasyką [kina]: <film>' summer-classics strand — the bare form the LATO-prefixed 'Wakacje z Klasyką Kina' rule doesn't cover"),
    searchStrip("xtra-pipe-cycle-banner",           """(?iu)\s*\|\s*(?:Cykl|Przegląd|Festiwal|DKF|Retrospektywa|Klasyka|Klub\s+Filmowy|Poniedziałki\s+z|Wtorki\s+z)\b.*$""", "'<film> | <cycle/festival banner>' generic PIPE-suffix strip, keyword-guarded so it only fires when the segment right after the pipe is a recognised cycle word — never amputates a 'Banner | Film' shape where the film FOLLOWS the pipe (KINO SENIORA | Ojczyzna, LATO w LUNIE | Drzewo magii stay intact)")
  )

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
   *  cinemas whose scraper doesn't run `ScraperParse.extractFormatTags`. */
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
    canon("xtra-canonical-gwiezdne-wojny-ci",
      """(?iu)^Gwiezdne\s+wojny\s*:\s*""", "",
      "Case-insensitive 'Gwiezdne wojny:' franchise prefix — the seed 'canonical-gwiezdne-wojny' only matches the capitalised 'Gwiezdne Wojny:', so the lower-case spelling (Mandalorian i Grogu) never merged."),
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
      """(?iu)\s*[\[(]\s*(?:19|20)\d{2}\s*[\])]\s*$""", "",
      "Trailing parenthesised release year ('(1991)', '(1976)') glued into the title by a scraper — never part of the film's identity, so 'Milczenie owiec (1991)' merges into 'Milczenie owiec'. Only the PARENTHESISED form, so a bare year that IS the title ('1917', '2046') survives."),
    canon("xtra-canonical-mandalorian-grogu-en",
      """(?iu)^The\s+Mandalorian\s+and\s+Grogu$""", "Mandalorian i Grogu",
      "Map the English title to the Polish canonical (same TMDB id 1228710) so the EN-titled listing merges; ordered after the trailing-format strip clears its '2D DUB' suffix first."),
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
      "Collapse every spelling of the TMDB-no-match 'Niesamowite przygody skarpetek 3. Ale kosmos!' (Roman III, the Cinema-City source-truncated 'Ale ko', the subtitle-less '…skarpetek 3', the format-tagged variants) onto one canonical title. The '3'/'III' requirement keeps films 1 (no number) and 2 (…skarpetek 2.) as their own rows.")
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

  /** Orders stamped by position so the extras fold AFTER the seed rules. */
  val all: Seq[TitleRule] =
    (programmePrefixes ++ searchStrips ++ canonical ++ perCinemaRules).zipWithIndex.map {
      case (r, i) => r.copy(order = 100 + i)
    }
}
