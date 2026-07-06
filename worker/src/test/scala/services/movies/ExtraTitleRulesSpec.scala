package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer.normalize
import services.titlerules.{ExtraTitleRules, TitleRuleDefaults, TitleRuleSet}

import java.util.Locale

/** Locks the behaviour of [[ExtraTitleRules]]. The "load-bearing" pair on each
 *  case is the gate: seed rules ALONE must NOT strip the banner (fail-before),
 *  seed + extras MUST (pass-after) — so a rule that silently stops matching is
 *  caught here, not in prod. */
class ExtraTitleRulesSpec extends AnyFlatSpec with Matchers {

  private val withExtras = TitleRuleSet(TitleRuleDefaults.all ++ ExtraTitleRules.all)
  private val seedOnly   = TitleRuleDefaults.ruleSet

  // ── programme prefixes: extracted as own row (display), stripped for query ──

  private val programmeCases = Seq(
    "Klub Konesera: Ojczyzna"                         -> ("Klub Konesera: ",                  "Ojczyzna"),
    "Kino Konesera: Drugie życie"                     -> ("Kino Konesera: ",                  "Drugie życie"),
    "KINO SENIORA | Ojczyzna"                         -> ("KINO SENIORA | ",                  "Ojczyzna"),
    "Pora dla Seniora: Ojczyzna"                      -> ("Pora dla Seniora: ",               "Ojczyzna"),
    "Wtorki dla Seniora: Młode matki"                 -> ("Wtorki dla Seniora: ",             "Młode matki"),
    "Filmowy Klub Seniora i Seniorki: OJCZYZNA"       -> ("Filmowy Klub Seniora i Seniorki: ", "OJCZYZNA"),
    "DKF Kropka: Riefenstahl"                         -> ("DKF Kropka: ",                     "Riefenstahl"),
    "DKF Człowiek w Zagrożeniu: Pociągi"              -> ("DKF Człowiek w Zagrożeniu: ",      "Pociągi"),
    "Klub Filmowy Urania: Młode matki"               -> ("Klub Filmowy Urania: ",            "Młode matki"),
    "Klub Filmowy Żółty Fotel: Chronologia wody"     -> ("Klub Filmowy Żółty Fotel: ",       "Chronologia wody"),
    "Kino Dostępne: Drugie życie"                     -> ("Kino Dostępne: ",                  "Drugie życie"),
    "Filmoterapia z Inspirą: Drugie życie"            -> ("Filmoterapia z Inspirą: ",         "Drugie życie"),
    "Portret Kobiety: Takie jest życie"               -> ("Portret Kobiety: ",               "Takie jest życie"),
    // Second-wave (2026-06-18) retrospective / classics prefixes.
    "WAJDA: re-wizje: Brzezina"                        -> ("WAJDA: re-wizje: ",               "Brzezina"),
    "WAJDA: re- wizje: CZŁOWIEK Z MARMURU"             -> ("WAJDA: re- wizje: ",              "CZŁOWIEK Z MARMURU"),
    "Klasyka na TOPie: Podziemny krąg"                 -> ("Klasyka na TOPie: ",              "Podziemny krąg"),
    "Klasyka na TOPie na Dzień Ojca: Rambo: Pierwsza krew." -> ("Klasyka na TOPie na Dzień Ojca: ", "Rambo: Pierwsza krew."),
    "Klasyka w kinie Atlantic: Zawieście czerwone latarnie" -> ("Klasyka w kinie Atlantic: ", "Zawieście czerwone latarnie"),
    "17. PRZEGLĄD NOWEGO KINA FRANCUSKIEGO: Fałszerz stulecia" -> ("17. PRZEGLĄD NOWEGO KINA FRANCUSKIEGO: ", "Fałszerz stulecia"),
    // Third-wave (2026-06-19) audience / club programme prefixes.
    "Bezpieczne wakacje: Fleak. Futrzak i ja"         -> ("Bezpieczne wakacje: ",             "Fleak. Futrzak i ja"),
    "Skocz z Bajtlem do kina: Ojczyzna"               -> ("Skocz z Bajtlem do kina: ",        "Ojczyzna"),
    "KMW: Kurozając i świątynia Świstaka"             -> ("KMW: ",                             "Kurozając i świątynia Świstaka"),
    "KF Ambasada: Hannah i jej siostry"               -> ("KF Ambasada: ",                     "Hannah i jej siostry"),
    "KF Ambasada: Trzy kolory: Czerwony"              -> ("KF Ambasada: ",                     "Trzy kolory: Czerwony"),
    "Seans filmowy dla rodziców: Ojczyzna"            -> ("Seans filmowy dla rodziców: ",      "Ojczyzna"),
    // Fourth-wave (2026-06-20) club / audience programme prefixes.
    "Kino seniora: Takie jest życie"                  -> ("Kino seniora: ",                    "Takie jest życie"),
    "Fregata dla Seniorów: Ojczyzna"                  -> ("Fregata dla Seniorów: ",            "Ojczyzna"),
    "Janosik Dzieciom: Minionki i straszydła"         -> ("Janosik Dzieciom: ",                "Minionki i straszydła"),
    "Janosik Szkrabom: Reksio i paw"                  -> ("Janosik Szkrabom: ",                "Reksio i paw"),
    "Najlepsze z najgorszych: Sarnie żniwo"           -> ("Najlepsze z najgorszych: ",         "Sarnie żniwo"),
    "CZŁOWIEK NA PIERWSZYM PLANIE: Takie jest życie"  -> ("CZŁOWIEK NA PIERWSZYM PLANIE: ",     "Takie jest życie"),
    "Męskie Kino na Dzień Ojca: Czas Apokalipsy"      -> ("Męskie Kino na Dzień Ojca: ",       "Czas Apokalipsy"),
    "Kobieta Pełna Życia: Książę"                     -> ("Kobieta Pełna Życia: ",             "Książę"),
    // Fifth-wave (2026-06-20) dated summer-cinema strand. The trailing '(1984)'
    // is a Canonical-only strip, so it stays in the query — TMDB resolves it.
    "KINO LETNIE 2026: Amadeusz (1984)"               -> ("KINO LETNIE 2026: ",                "Amadeusz (1984)"),
    "KINO LETNIE 2026: Requiem dla snu (2000)"        -> ("KINO LETNIE 2026: ",                "Requiem dla snu (2000)"),
    // Eighth-wave (2026-06-20) festival / audience / club colon prefixes.
    "TNKF: Guru"                                      -> ("TNKF: ",                            "Guru"),
    "Przegląd Nowego Kina Francuskiego: Obcy"         -> ("Przegląd Nowego Kina Francuskiego: ", "Obcy"),
    "Rok z Marilyn Monroe: Słomiany wdowiec"          -> ("Rok z Marilyn Monroe: ",            "Słomiany wdowiec"),
    "Rodzina w kinie: Kicia Kocia w podróży"          -> ("Rodzina w kinie: ",                 "Kicia Kocia w podróży"),
    "Tani wtorek: OJCZYZNA"                           -> ("Tani wtorek: ",                     "OJCZYZNA"),
    "Filmowe popołudnie dla dzieci: Złoto"            -> ("Filmowe popołudnie dla dzieci: ",   "Złoto"),
    "Klasyk w kinie: Milczenie owiec"                 -> ("Klasyk w kinie: ",                  "Milczenie owiec"),
    "Seans Przyjazny Sensorycznie: Willow i tajemniczy las" -> ("Seans Przyjazny Sensorycznie: ", "Willow i tajemniczy las"),
    "FIESTA KINA HISZPAŃSKIEGO: Prawo pożądania"      -> ("FIESTA KINA HISZPAŃSKIEGO: ",       "Prawo pożądania"),
    // Chain decoration banners promoted from per-cinema seeds (were TitleRuleDefaults
    // cleanTitle strips): now GLOBAL query-only, so any cinema's copy resolves the
    // base film while the decorated screening keeps its own display row.
    "Kino na obcasach: Zaproszenie"                   -> ("Kino na obcasach: ",                "Zaproszenie"),
    "Ladies Night - Narodziny gwiazdy"                -> ("Ladies Night - ",                   "Narodziny gwiazdy"),
    "Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas" -> ("Kolekcja Mamoru Hosody: ", "O dziewczynie skaczącej przez czas"),
    "DZIEŃ DZIECKA W APOLLO - Drzewo Magii"           -> ("DZIEŃ DZIECKA W APOLLO - ",         "Drzewo Magii"),
    // Sixteenth-wave (2026-07-06) TMDB-no-match audit: the dot separator variant
    // of the existing 'Klasyk w kinie:' colon strand. (WSP: is a search-only strip
    // — see searchStripCases — so the all-caps acronym isn't re-cased on display.)
    "Klasyk w kinie. Milczenie owiec"                 -> ("Klasyk w kinie. ",                  "Milczenie owiec")
  )

  "ExtraTitleRules programme prefixes" should "extract the banner for the display row" in {
    programmeCases.foreach { case (in, (banner, _)) =>
      withClue(s"programmePrefix('$in'): ")(withExtras.programmePrefix(in) shouldBe Some(banner))
    }
  }

  it should "strip the banner for the external-API query" in {
    programmeCases.foreach { case (in, (_, query)) =>
      withClue(s"search('$in'): ")(withExtras.search(in) shouldBe query)
    }
  }

  it should "be load-bearing — the seed rules alone leave the banner in place" in {
    programmeCases.foreach { case (in, _) =>
      withClue(s"seedOnly.search('$in') should be unchanged: ")(seedOnly.search(in) shouldBe in)
      withClue(s"seedOnly.programmePrefix('$in'): ")(seedOnly.programmePrefix(in) shouldBe None)
    }
  }

  // ── search-only strips: row kept, query fixed ──────────────────────────────

  private val searchStripCases = Seq(
    "Ojczyzna_DKF"                     -> "Ojczyzna",
    "Czytając Lolitę w Teheranie | DKF" -> "Czytając Lolitę w Teheranie",
    "OJCZYZNA - DKF KOT"               -> "OJCZYZNA",
    "DRUGIE ŻYCIE - DKF III W"         -> "DRUGIE ŻYCIE",
    "Pociągi - dyskusyjny klub filmowy" -> "Pociągi",
    "Ojczyzna | PRZEDPREMIERA"         -> "Ojczyzna",
    "Takie jest życie - przedpremiera" -> "Takie jest życie",
    "Drugie życie | przedpremierowo"   -> "Drugie życie",
    "Przedpremiera | Ojczyzna"         -> "Ojczyzna",
    "PRZEDPREMIERA: Wielki łuk"        -> "Wielki łuk",
    "Młode matki *AD"                  -> "Młode matki",
    // Festival / retrospective banners (Kinoteka + the Fellini cycle), real
    // corpus strings. Display row kept (searchTitle), query stripped to the film.
    "WTF Fest | Crash"                                          -> "Crash",
    "WTF Fest | Setki bobrów"                                   -> "Setki bobrów",
    "Ból i blask | 6 razy Pedro"                                -> "Ból i blask",
    "Lawrence z Arabii | Kino cyrkularne EXTRA"                 -> "Lawrence z Arabii",
    "Federico Fellini: ciao a tutti! – Osiem i pół"             -> "Osiem i pół",
    "FEDERICO FELLINI: ciao a tutti!: Wałkonie"                 -> "Wałkonie",
    "Federico Fellini: GIULIETTA I DUCHY"                       -> "GIULIETTA I DUCHY",
    "Noce Cabirii (1957) | FEDERICO FELLINI: ciao a tutti!"     -> "Noce Cabirii (1957)",
    "WAŁKONIE – przegląd FEDERICO FELLINI: ciao a tutti!"       -> "WAŁKONIE",
    // Second-wave (2026-06-18) classics / retrospective / cycle suffixes.
    "Chinatown | 10/10 Klasyka filmowa"                         -> "Chinatown",
    "Stowarzyszenie Umarłych Poetów | 10/10 Klasyka filmowa"    -> "Stowarzyszenie Umarłych Poetów",
    "CZŁOWIEK Z MARMURU | WAJDA: re- wizje. Przegląd filmów Andrzeja Wajdy w 100. rocznicę urodzin" -> "CZŁOWIEK Z MARMURU",
    "CZŁOWIEK Z MARMURU\\ Wajda: re-wizje"                      -> "CZŁOWIEK Z MARMURU",
    "Guru | Przegląd Nowego Kina Francuskiego"                  -> "Guru",
    "Guru (org. Gourou) 17. edycja Przeglądu Nowego Kina Francuskiego" -> "Guru",
    "Donnie darko – amerykańska klasyka"                        -> "Donnie darko",
    "Co się zdarzyło baby jane? – amerykańska klasyka"          -> "Co się zdarzyło baby jane?",
    "Najważniejsze to kochać – żuławski. Kino ekstazy"          -> "Najważniejsze to kochać",
    "Opętanie | ŻUŁAWSKI. KINO EKSTAZY"                         -> "Opętanie",
    "Salto | Poniedziałki z Konwickim: pisarz – scenarzysta – reżyser" -> "Salto",
    "Truposz // JIM JARMUSCH"                                   -> "Truposz",
    // Third-wave (2026-06-19) retrospective / series prefixes + decoration suffixes.
    "Pedro Almodóvar: Kolory emocji - Matador"                  -> "Matador",
    "Pedro Almodóvar: Kolory emocji - Matki równoległe"         -> "Matki równoległe",
    "Wielka Sztuka w Kinoteatrze Rialto - Velázquez i jego tajemnica" -> "Velázquez i jego tajemnica",
    "SZTUKA W CENTRUM. NOWOŚCI 2026 | Caravaggio. Arcydzieła niepokornego geniusza" -> "Caravaggio. Arcydzieła niepokornego geniusza",
    "Lekcje Filmowe - Nić widmo"                                -> "Nić widmo",
    "Lekcje Filmowe - Śniadanie u Tiffany'ego"                  -> "Śniadanie u Tiffany'ego",
    "Art Beats: Rafael. Młody geniusz"                          -> "Rafael. Młody geniusz",
    "Wajda. Brzezina"                                           -> "Brzezina",
    "Wajda. Kronika wypadków miłosnych"                         -> "Kronika wypadków miłosnych",
    "Konwicki: Dolina Issy (1982)"                              -> "Dolina Issy (1982)",
    "Pokaz przedpremierowy: minionki i straszydła"             -> "minionki i straszydła",
    "Światłoczuła: II Jarociński Festiwal Filmowy dla Dzieci i Młodzieży" -> "Światłoczuła",
    "STRAŻNICZKA SMOKÓW: II Jarociński Festiwal Filmowy dla Dzieci i Młodzieży" -> "STRAŻNICZKA SMOKÓW",
    "LARP. Miłość, trolle i inne questy. : II Jarociński Festiwal Filmowy dla Dzieci i Młodzieży" -> "LARP. Miłość, trolle i inne questy",
    "Ida - przegląd filmów Pawła Pawlikowskiego"               -> "Ida",
    "Zimna wojna - przegląd filmów Pawła Pawlikowskiego"       -> "Zimna wojna",
    "Pomoc domowa- kino dla kobiet"                            -> "Pomoc domowa",
    "Mickey i nicky – kino dla seniora"                        -> "Mickey i nicky",
    "Zaproszenie - Kino Kobiet"                                -> "Zaproszenie",
    "Niesamowite przygody skarpetek 3 - KNTJ"                  -> "Niesamowite przygody skarpetek 3",
    "Opętanie – pokaz specjalny na 60. Urodziny kina zamek"    -> "Opętanie",
    "Mistrzynie | POKAZ PRZEDPREMIEROWY"                       -> "Mistrzynie",
    "Lawa – Tadeusz Konwicki"                                  -> "Lawa",
    "Brzezina - Andrzej Wajda o filmie"                        -> "Brzezina",
    // Fourth-wave (2026-06-20) director DOT prefixes + cross-cinema decoration suffixes.
    "Fellini. Giulietta i duchy"                               -> "Giulietta i duchy",
    "Fellini. Noce Cabirii | KF Ambasada"                      -> "Noce Cabirii",
    "Fellini. Wałkonie"                                        -> "Wałkonie",
    "Hosoda. Wilcze dzieci"                                    -> "Wilcze dzieci",
    "Hosoda. Summer Wars"                                      -> "Summer Wars",
    "Konwicki. Ostatni dzień lata"                             -> "Ostatni dzień lata",
    "Konwicki. Salto"                                          -> "Salto",
    "Ojczyzna | Wtorek Seniora"                                -> "Ojczyzna",
    "Takie jest życie_FKS"                                     -> "Takie jest życie",
    "Posłani | FKS"                                            -> "Posłani",
    "Milczenie owiec - pokazy specjalne"                       -> "Milczenie owiec",
    "Silver | Wakacje z dokumentem"                            -> "Silver",
    // Fifth-wave (2026-06-20) audit of the TMDB-no-match corpus.
    // Ukrainian-language version markers (bare-space, dot, parenthesised, and the
    // English 'ukrainian' spelling). Query-only strip — the row stays its own (the
    // canonical UA exclusion below keeps the key) but resolves the base film.
    "Toy Story 5 ukraiński dubbing"                            -> "Toy Story 5",
    "Odyseja ukraiński dubbing"                                -> "Odyseja",
    "Straszny film. Ukrainian dubbing"                         -> "Straszny film",
    "Minionki i straszydła (ukraiński dubbing)"                -> "Minionki i straszydła",
    // 'reż. / reżyseria: <director>' authorship suffix.
    "Perfect Days reż. Wim Wenders"                            -> "Perfect Days",
    "Droga do Vermiglio reż. Maura Delpero"                    -> "Droga do Vermiglio",
    "Za duży na bajki 3 reżyseria: Kristoffer Rus"             -> "Za duży na bajki 3",
    // Clubless 'DKF - / DKF:' prefix + the 'Filmowy Klub Seniora -' dash variant.
    "DKF - Drugie życie"                                       -> "Drugie życie",
    "DKF: Czytając Lolitę w Teheranie"                         -> "Czytając Lolitę w Teheranie",
    "Filmowy Klub Seniora - Drugie życie"                      -> "Drugie życie",
    "Tajny agent | Filozoficzny Klub Filmowy"                  -> "Tajny agent",
    // Sixth-wave (2026-06-20). Studio-attribution strip, re-release banner, and a
    // restoration-print director suffix — each verified to leave a query that
    // resolves on TMDB. (The seed already strips '. Wersja zremasterowana', so the
    // prod query for the ~30 Multikino rows is already 'Żywot Briana Grupy Monty
    // Pythona'; this rule removes the studio tail so it resolves to TMDB 583.)
    "Żywot Briana Grupy Monty Pythona"                         -> "Żywot Briana",
    "Żywot Briana - PONOWNIE NA WIELKIM EKRANIE"               -> "Żywot Briana",
    "Chungking Express  - Wong Kar Wai 4K"                     -> "Chungking Express",
    // Seventh-wave (2026-06-20) quoted cycle/series banners.
    // (a) '"<cycle>" - <film>' — quoted banner prefix, film after the dash. The
    // trailing '(2001)' is a Canonical-only strip, so it stays in the query (TMDB
    // resolves it), same as the KINO LETNIE '(1984)' precedent.
    "\"Kultowe Wakacje\" - Amelia (2001)"                      -> "Amelia (2001)",
    "\"Kultowe Wakacje\" - Milczenie Owiec"                    -> "Milczenie Owiec",
    // (b) '"<film>" w ramach cyklu <cycle>' — quoted film, descriptor tail dropped
    // (the standalone '— 2025' year column the listing glues on is swallowed too).
    "\"Drugie życie\" w ramach cyklu SWPS    —    2025"        -> "Drugie życie",
    "\"Drugie życie\" w ramach cyklu SWPS"                     -> "Drugie życie",
    // Eighth-wave (2026-06-20) pipe/paren decoration suffixes.
    "Drugie życie | Kino przy herbatce"                        -> "Drugie życie",
    "Frances Ha | Teleskop"                                    -> "Frances Ha",
    "Fisher King | Lato z Robinem Williamsem"                  -> "Fisher King",
    "Stowarzyszenie umarłych poetów | Lato z Robinem Williamsem" -> "Stowarzyszenie umarłych poetów",
    "Erupcja | Kinoteka dla Rodziców"                          -> "Erupcja",
    "Projekt Hail Mary | Kinoteka dla rodzica"                 -> "Projekt Hail Mary",
    "Matador | Fiesta Kina Hiszpańskiego"                      -> "Matador",
    "Matador – fiesta kina hiszpańskiego: almodóvar/ banderas" -> "Matador",
    "Prawo pożądania | Fiesta Kina Hiszpańskiego"              -> "Prawo pożądania",
    "Santa Sangre | AMONDO GRINDHOUSE"                         -> "Santa Sangre",
    "Erupcja (pokaz jednorazowy)"                              -> "Erupcja",
    "Ojczyzna (pokaz przedpremierowy)"                         -> "Ojczyzna",
    "Zawieście czerwone latarnie (pokaz jednorazowy)"          -> "Zawieście czerwone latarnie",
    "K-Popowe Łowczynie Demonów (seans z napisami karaoke)"    -> "K-Popowe Łowczynie Demonów",
    // Quoted FILM whose tail is a banner — the inverse of the cycle-dash shape.
    "\"Backrooms. Bez wyjścia\" - UROCZYSTA POLSKA PREMIERA"   -> "Backrooms. Bez wyjścia",
    "\"Drugie życie\" | specjalny pokaz w ramach cyklu dziewiarskiego OCZKO NA FILM" -> "Drugie życie",
    "\"Wędrówka na północ\" pokaz przedpremierowy w ramach cyklu Spotkania Filozoficzne" -> "Wędrówka na północ",
    // Tenth-wave (2026-06-23) programme/cycle prefixes + decoration suffixes.
    "LATO’26. Wakacje z Klasyką Kina - Casablanca"            -> "Casablanca",
    "LATO 2026. Wakacje z Klasyką Kina - Mulholland Drive"    -> "Mulholland Drive",
    "Wakacje dla dzieci: Pucio"                               -> "Pucio",
    "LATO w LUNIE | Drzewo magii"                             -> "Drzewo magii",
    "Kino dla Kobiet: Drugie życie"                           -> "Drugie życie",
    "Babski czwartek: Czytając Lolitę w Teheranie"           -> "Czytając Lolitę w Teheranie",
    "Kobiece Wieczory w Kino Cafe: Zupa nic"                  -> "Zupa nic",
    "Wieczory filmowe na boku | Zaproszenie"                  -> "Zaproszenie",
    "Klasyka na fali: Lot nad kukułczym gniazdem"            -> "Lot nad kukułczym gniazdem",
    "Klasyka w Kulturze: La Strada"                          -> "La Strada",
    "FILMY Z LEKTOREM - Poprzednie życie"                    -> "Poprzednie życie",
    "ŚMIECH PRZEZ ŁZY: Wesele"                               -> "Wesele",
    "Andrzej Wajda. Dziedzictwo Mistrza: Popiół i diament"  -> "Popiół i diament",
    "MIĘDZYNARODOWY KONKURS FILMÓW PEŁNOMETRAŻOWYCH - ALLAH IS NOT OBLIGED" -> "ALLAH IS NOT OBLIGED",
    // The same banner in the cinema's mixed/lower case — the `(?iu)` flag must fold
    // the Polish diacritics (Ę/Ó/Ł/Ż) so the lower-case spelling strips too. This is
    // the exact prod-queue form that kept a 'the square' ResolveTmdb task retrying.
    "Międzynarodowy konkurs filmów pełnometrażowych - the square" -> "the square",
    "Międzynarodowy konkurs filmów pełnometrażowych - The square" -> "The square",
    "Supergirl tani poniedziałek"                            -> "Supergirl",
    "Toy story 5 tani poniedziałek"                          -> "Toy story 5",
    "Supergirl 2D"                                           -> "Supergirl",
    // Eleventh-wave (2026-06-23) containment-derived strips (inner film is rated).
    "Maraton: Powrót do przyszłości"                         -> "Powrót do przyszłości",
    "Spotkania Filmowe | Ojczyzna"                           -> "Ojczyzna",
    "Psychoanalityczne Spotkania Filmowe „W głębi”: Czytając Lolitę w Teheranie" -> "Czytając Lolitę w Teheranie",
    "Classy Monday - Rambo: Pierwsza krew"                   -> "Rambo: Pierwsza krew",
    "19. FGA: Szepty lasu"                                   -> "Szepty lasu",
    "BKF #53 Chronologia wody"                               -> "Chronologia wody",
    "Niedziela z Dokumentem: Dziecko z pyłu"                 -> "Dziecko z pyłu",
    "70-lecie Wydawnictwa Poznańskiego: Wędrówka na północ"  -> "Wędrówka na północ",
    "BACKROOMS. BEZ WYJŚCIA - Młodzieżowy Klub Filmowy LEŻAK" -> "BACKROOMS. BEZ WYJŚCIA",
    "Backrooms. Bez wyjścia I 2D I"                          -> "Backrooms. Bez wyjścia",
    "Rozmowa. Ostatni seans"                                 -> "Rozmowa",
    "Werdykt czwartek konesera"                              -> "Werdykt",
    "Człowiek z marmuru. Spotkanie z Michałem Tarkowskim"    -> "Człowiek z marmuru",
    "Sprawiedliwość owiec - Filmoteka Dojrzałego Człowieka"  -> "Sprawiedliwość owiec",
    "Zaproszenie - przepdremiera"                            -> "Zaproszenie",
    "Toy Story 5- 2D Dubbing PL"                             -> "Toy Story 5",
    // Twelfth-wave (2026-06-25) programme/series prefixes + a Silesian-dub suffix.
    // The 'Przegląd filmów <reż> - <film>' PREFIX (the existing rule only caught the
    // SUFFIX form 'Ida - przegląd filmów Pawła Pawlikowskiego').
    "Przegląd filmów Andrzeja Wajdy - Ziemia obiecana"       -> "Ziemia obiecana",
    "Przegląd filmów andrzeja wajdy - człowiek z marmuru"    -> "człowiek z marmuru",
    "Filmowe wakacje za Rogiem: Koszmarek"                   -> "Koszmarek",
    "Filmowe wakacje za Rogiem:  Pies i robot"               -> "Pies i robot",
    "Filmowe lato: Toy Story 5"                              -> "Toy Story 5",
    "Wakacje w Kinie Orzeł: Drzewo magii"                    -> "Drzewo magii",
    "Wakacje w kinie: Chłopiec na krańcach świata"           -> "Chłopiec na krańcach świata",
    // Thirteenth-wave (2026-06-25) containment-audit strips: decoration wrapping a
    // rated film that no rule caught.
    "26 FFS Być kochaną"                                     -> "Być kochaną",
    "Toy Story 5 2D DUB. SPS"                                -> "Toy Story 5",
    "Vaiana 2D DUB. SPS"                                     -> "Vaiana",
    "Premiera: Kumotry"                                      -> "Kumotry",
    "Premiera!!! Straszny film"                              -> "Straszny film",
    "Dialog przez Film: Co do... Kury?"                      -> "Co do... Kury?",
    "Mistrzowska Kreska: Podwójne życie Weroniki"            -> "Podwójne życie Weroniki",
    "Najlepsze z Najgorszych - Big Shark"                    -> "Big Shark",
    "Brudny Henryk | Najlepsze z Najgorszych"                -> "Brudny Henryk",
    "Sztuka na ekranie - Caravaggio. Arcydzieła niepokornego geniusza" -> "Caravaggio. Arcydzieła niepokornego geniusza",
    "Exhibition On Screen: David Hockney. Pejzaże, portrety i martwe natury" -> "David Hockney. Pejzaże, portrety i martwe natury",
    "Minionki i straszydła - Poranki dziecięce"              -> "Minionki i straszydła",
    // Fourteenth-wave (2026-06-25) programme-cycle prefixes from the
    // resolve-by-synopsis report (each stripped query verified on TMDB).
    "Kinowy Poranek: Lato, kiedy nauczyłam się latać"        -> "Lato, kiedy nauczyłam się latać",
    "Przybliżenia - okiem psychoanalizy: Perfect days"       -> "Perfect days",
    "Kino bez barier dla dzieci: Oskar, Patka i Złoto Bałtyku" -> "Oskar, Patka i Złoto Bałtyku",
    // Fifteenth-wave (2026-06-29) generic cycle/festival banners — colon prefixes
    // and the keyword-guarded pipe suffix.
    // The task's flagship pipe-SUFFIX example (also covered by the named Konwicki
    // rule + the new generic pipe rule); the Polish „…” quotes fold to ASCII.
    "Lawa - opowieść o \"Dziadach\" | Poniedziałki z Konwickim: pisarz – scenarzysta – reżyser" -> "Lawa - opowieść o \"Dziadach\"",
    // The new Konwicki PREFIX form (the existing rule only caught the pipe suffix).
    "Poniedziałki z Konwickim: Lawa"                        -> "Lawa",
    "Poniedziałki z Konwickim: Dolina Issy"                -> "Dolina Issy",
    "Klasyka w kinie: Casablanca"                          -> "Casablanca",
    "Cykl Filmowy: Stalker"                                -> "Stalker",
    "Przegląd Kina Hiszpańskiego: Volver"                  -> "Volver",
    "Filmowe Lato w Kinie: Amelia"                         -> "Amelia",
    "Wakacje z klasyką: Rejs"                              -> "Rejs",
    "Wakacje z klasyką kina: Rejs"                         -> "Rejs",
    // The generic keyword-guarded pipe SUFFIX (banner follows the pipe).
    "Persona | Cykl Bergmana"                              -> "Persona",
    "Stalker | Przegląd Tarkowskiego"                      -> "Stalker",
    // Sixteenth-wave (2026-07-06) TMDB-no-match audit: authorship suffix with a
    // leading comma, the '(YYYY) 4K' restoration tag, and the Helios RePlay strand.
    "Przekleństwa niewinności, reż. Sofia Coppola (2021)"  -> "Przekleństwa niewinności",
    "Generał (1926) 4K"                                    -> "Generał (1926)",
    "Ghost in the shell (1995) 4K"                         -> "Ghost in the shell (1995)",
    "Wejście smoka w Helios RePlay"                        -> "Wejście smoka"
  )

  "ExtraTitleRules search strips" should "strip the marker for the external-API query" in {
    searchStripCases.foreach { case (in, query) =>
      withClue(s"search('$in'): ")(withExtras.search(in) shouldBe query)
    }
  }

  it should "be load-bearing — the seed rules alone leave the marker in place" in {
    searchStripCases.foreach { case (in, _) =>
      withClue(s"seedOnly.search('$in') should be unchanged: ")(seedOnly.search(in) shouldBe in)
    }
  }

  it should "not fire the quoted-banner rules without their anchor" in {
    // (a) needs a leading quote + a dash; a plain '- subtitle' must survive.
    withClue("plain dash subtitle: ")(withExtras.search("Rambo - Pierwsza krew") shouldBe "Rambo - Pierwsza krew")
    // A fully-quoted title (no dash, no descriptor tail) hits neither banner rule,
    // so it's left exactly as-is — crucially NOT stripped to empty.
    withClue("fully-quoted, no dash: ")(withExtras.search("\"Whiplash\"") shouldBe "\"Whiplash\"")
    // (b) needs the literal 'w ramach cyklu'; a quoted film with an unrelated 'w …'
    // must NOT be amputated to its first word (left untouched, quotes and all).
    withClue("quoted film, unrelated 'w …': ")(withExtras.search("\"Lato w mieście\"") shouldBe "\"Lato w mieście\"")
  }

  it should "strip a pipe banner only when a cycle keyword follows the pipe" in {
    // 'Banner | Film' shape: a real film (no cycle keyword) after the pipe is KEPT —
    // the keyword guard is what stops the generic pipe rule from amputating the film.
    withClue("film after pipe kept: ")(
      withExtras.search("Wieczory na tarasie | Persona") shouldBe "Wieczory na tarasie | Persona")
    // The real corpus 'Banner | Film' inverse shapes stay intact (handled as PREFIXES,
    // never eaten as a suffix): the segment after the pipe is the film, not a banner.
    withClue("KINO SENIORA prefix-extracted, not amputated: ")(
      withExtras.search("KINO SENIORA | Ojczyzna") shouldBe "Ojczyzna")
    withClue("LATO w LUNIE prefix-extracted, not amputated: ")(
      withExtras.search("LATO w LUNIE | Drzewo magii") shouldBe "Drzewo magii")
    // And it DOES fire when a cycle word is the first token after the pipe.
    withClue("cycle suffix stripped: ")(
      withExtras.search("Persona | Cykl Bergmana") shouldBe "Persona")
  }

  // Real corpus strings the SEED already partially strips (so they can't go in the
  // load-bearing searchStripCases), proving the eighth-wave suffix still resolves the
  // bare film end-to-end once the seed's '+ …' PlusSuffix has run.
  it should "resolve banner suffixes that sit behind a seed-stripped '+ …' tail" in {
    withClue("Lato + Modowy tail: ")(
      withExtras.search("Klatka dla ptaków | Lato z Robinem Williamsem + Modowy Klub Filmowy") shouldBe "Klatka dla ptaków")
    // Ninth-wave: the 'Filmoterapia w Lunie' series pipe-wraps the film in the
    // middle; the seed's '+ …' PlusSuffix already drops '+ rozmowa Martyny Harland',
    // so this can't go in the load-bearing list. The xtra rule captures the middle
    // segment regardless of the tail.
    withClue("Filmoterapia w Lunie pipe-wrapped: ")(
      withExtras.search("Filmoterapia W Lunie | O czym sobie nie mówmy | seans + rozmowa Martyny Harland") shouldBe "O czym sobie nie mówmy")
    withClue("Filmoterapia w Lunie no tail: ")(
      withExtras.search("Filmoterapia w Lunie | Drugie życie") shouldBe "Drugie życie")
  }

  // ── negative controls: real titles must survive untouched ──────────────────

  private val untouched = Seq(
    "Top Gun: Maverick",
    "Mufasa: Król Lew",
    "Diabeł ubiera się u Prady 2",
    "Ojczyzna",
    "2001: Odyseja kosmiczna",
    "Trzy kolory: Czerwony",
    "Rambo: Pierwsza krew",
    // The 'pokaz' suffix must not amputate a real '- Seans …' subtitle.
    "Pan Li - Seans Spirytustyczny",
    // The full-length-competition strip must NOT touch the short-film SETS (they
    // don't decode to a single film, so stripping the banner is pointless): the
    // rule requires PEŁNOMETRAŻOWYCH + a dash, not KRÓTKOMETRAŻOWYCH + a colon.
    "Międzynarodowy Konkurs Filmów Krótkometrażowych: set II – MOST",
    // The capital-'I' format wrapper is case-SENSITIVE on the 'I' so the Polish
    // conjunction ' i ' (and the lowercase title) can't trigger it.
    "Lilo i Stitch",
    "Asterix i Obelix: Imperium smoka",
    // The dot-'Spotkanie' suffix needs a PERIOD before 'spotkani…'; a title that
    // merely CONTAINS 'spotkania' must survive (no period, no following ' z').
    "Bliskie spotkania trzeciego stopnia",
    // Thirteenth-wave containment traps: a rated film appears as a BARE substring
    // here, but the surrounding text is NOT a strippable banner — the strips must
    // be anchored, never blind contains. These must survive whole.
    "Minimaraton Supergirl & Superman",   // a double-feature bundle, NOT 'Supergirl'
    "Szlagierowy zawrót głowy",            // ⊅ 'Zawrót głowy' (Vertigo) — ordinary words
    // 'po śląsku' is part of the release's own title, not a strippable dub suffix —
    // a twelfth-wave rule that stripped it was dropped; this guards against re-adding.
    "Seksmisja po śląsku"
  )

  it should "never touch a plain colon/word title" in {
    untouched.foreach { t =>
      withClue(s"search('$t'): ")(withExtras.search(t) shouldBe t)
      withClue(s"structural('$t'): ")(withExtras.structural(t) shouldBe t)
      withClue(s"programmePrefix('$t'): ")(withExtras.programmePrefix(t) shouldBe None)
    }
  }

  // ── canonical merges: spelling variants collapse to ONE sanitize key ────────
  // `mergeKey` reproduces `TitleNormalizer.sanitize` exactly (deburr ∘ canonical ∘
  // normalize, lower-cased + alphanumeric-only) — the `_id`-keying that decides
  // whether two listings fold into the same `movies` row. Two variants sharing a
  // key merge; the staging newcomer check (`MovieCache`) recognises an already-known
  // film by that same key, so an unmerged spelling re-incubates ("appears and
  // disappears" in /debug staging). These are the real "Mandalorian i Grogu" corpus
  // spellings (common/.../prod-movies/titles.txt), all keyed to the canonical film.

  private def mergeKey(rs: TitleRuleSet, t: String): String =
    tools.TextNormalization.deburr(rs.canonical(normalize(t)))
      .toLowerCase(Locale.ROOT).replaceAll("[^\\p{L}\\p{N}]+", "")

  // The merge key a scraped title actually lands on: format/version tags are
  // peeled by the shared `FormatTags` at ingest (MovieCache.recordCinemaScrape)
  // BEFORE the title is keyed, so a fold check must strip first. (Spelling/prefix
  // variants carry no format tag → extractFormatTags is a no-op for them.)
  private def foldKey(rs: TitleRuleSet, t: String): String =
    mergeKey(rs, services.movies.FormatTags.extractFormatTags(t)._1)

  private val canonicalFilm = "Mandalorian i Grogu"

  // Variants that the SEED alone already merges (the capitalised franchise prefix,
  // the ' & ' → ' i ' unification) — a baseline so the load-bearing split below is
  // honest about which spellings only the additions rescue.
  private val seedAlreadyMerges = Seq(
    "Gwiezdne Wojny: Mandalorian i Grogu",
    "Mandalorian & Grogu"
  )

  // Variants ONLY the additions merge — the lower-case prefix, the trailing
  // language/format suffixes, the parenthesised sound tag, the English title.
  private val additionsMerge = Seq(
    "Gwiezdne wojny: Mandalorian i Grogu",
    "Gwiezdne wojny: mandalorian i grogu",
    "GWIEZDNE WOJNY: MANDALORIAN i GROGU",
    "Gwiezdne wojny: mandalorian & grogu",
    "Gwiezdne wojny: Mandalorian i Grogu (Dolby Atmos)",
    "Gwiezdne wojny: Mandalorian i Grogu 2D DUBBING",
    "Gwiezdne wojny: Mandalorian i Grogu 2D NAPISY",
    "Mandalorian & Grogu - 2D DUB",
    "Mandalorian i Grogu [napisy]",
    "Mandalorian i Grogu/dubbing",
    "Mandalorian i Grogu/napisy",
    "The Mandalorian and Grogu 2D DUB",
    "The Mandalorian and Grogu 2D NAP"
  )

  private val canonicalKey = mergeKey(seedOnly, canonicalFilm) // "mandalorianigrogu"

  "ExtraTitleRules canonical merges" should "fold every Mandalorian i Grogu spelling onto one key" in {
    (seedAlreadyMerges ++ additionsMerge).foreach { v =>
      withClue(s"foldKey('$v'): ")(foldKey(withExtras, v) shouldBe canonicalKey)
    }
  }

  it should "be load-bearing — the seed rules leave the addition-only spellings unmerged" in {
    additionsMerge.foreach { v =>
      withClue(s"seedOnly.mergeKey('$v') should NOT yet equal the film key: ")(
        mergeKey(seedOnly, v) should not be canonicalKey)
    }
  }

  it should "strip trailing language/format suffixes across films (not just M&G)" in {
    val cases = Seq(
      "Toy Story 5 2D DUBBING"        -> "Toy Story 5",
      "Straszny film napisy"          -> "Straszny film",
      "Supergirl/dubbing"             -> "Supergirl",
      "Ojczyzna / napisy"             -> "Ojczyzna",
      "Hopnięci | DUBBING"            -> "Hopnięci",
      "Wielkie piękno – napisy"       -> "Wielkie piękno",
      "Drzewo magii 2D DUBBING"       -> "Drzewo magii",
      "Babystar - lektor"             -> "Babystar"
    )
    cases.foreach { case (variant, base) =>
      withClue(s"foldKey('$variant') vs '$base': ")(
        foldKey(withExtras, variant) shouldBe foldKey(withExtras, base))
    }
  }

  it should "merge parenthesised format / 'wersja oryginalna' / 'ORG' / parenthesised-year variants onto the base" in {
    val cases = Seq(
      "Toy Story 5 (Dubbing PL)"        -> "Toy Story 5",
      "Toy Story 5 [dubbing PL]"        -> "Toy Story 5",
      "Toy Story 5 (Napisy PL)"         -> "Toy Story 5",
      "Toy Story 5 - wersja oryginalna" -> "Toy Story 5",
      "Toy Story 5 - ORG"               -> "Toy Story 5",
      "500 mil (lektor)"                -> "500 mil",
      "Milczenie owiec (1991)"          -> "Milczenie owiec",
      "Mikey i Nicky (1976)"            -> "Mikey i Nicky"
    )
    cases.foreach { case (variant, base) =>
      withClue(s"foldKey('$variant') vs '$base': ")(
        foldKey(withExtras, variant) shouldBe foldKey(withExtras, base))
    }
  }

  // "Niesamowite przygody skarpetek 3. Ale kosmos!" is TMDB-no-match, so nothing
  // folds its fragments — Helios's Roman "III", Cinema City's source-truncated
  // "Ale ko", the subtitle-less "…skarpetek 3", and the format-tagged variants
  // each key separately. The xtra-canonical-skarpetek-3 rule collapses them all.
  private val skarpetek3Variants = Seq(
    "Niesamowite przygody skarpetek 3",
    "Niesamowite przygody skarpetek III",
    "Niesamowite przygody skarpetek 3. Ale ko",          // Cinema City source truncation
    "Niesamowite przygody skarpetek 3. Ale kosmos",
    "Niesamowite przygody skarpetek 3. Ale kosmos!",
    "Niesamowite przygody skarpetek 3. Ale kosmos! 2D Dubbing"
  )
  private val skarpetek3Canonical = "Niesamowite przygody skarpetek 3. Ale kosmos!"

  it should "fold every 'skarpetek 3. Ale kosmos!' spelling onto one key" in {
    val key = mergeKey(withExtras, skarpetek3Canonical)
    skarpetek3Variants.foreach { v =>
      withClue(s"mergeKey('$v'): ")(mergeKey(withExtras, v) shouldBe key)
    }
  }

  it should "be load-bearing — the seed rules leave the skarpetek-3 truncated/format spellings unmerged" in {
    val key = mergeKey(seedOnly, skarpetek3Canonical)
    // The "Ale ko" Cinema-City truncation and the "2D Dubbing" format-tagged
    // spelling only merge WITH the extras. (The subtitle-less "…skarpetek 3",
    // the Roman "III", and the bare "…Ale kosmos" without the "!" already share
    // the seed key — the alphanumeric-only sanitize drops the trailing "!" — so
    // those are NOT the load-bearing cases.)
    Seq(
      "Niesamowite przygody skarpetek 3. Ale ko",
      "Niesamowite przygody skarpetek 3. Ale kosmos! 2D Dubbing"
    ).foreach { v =>
      withClue(s"seedOnly.mergeKey('$v') should NOT yet equal the canonical key: ")(
        mergeKey(seedOnly, v) should not be key)
    }
  }

  it should "keep the FIRST and SECOND skarpetek films as their own rows" in {
    val third = mergeKey(withExtras, skarpetek3Canonical)
    Seq(
      "Niesamowite przygody skarpetek",                  // film 1 — no number
      "Niesamowite przygody skarpetek 2. Skarpetki górą!" // film 2
    ).foreach { v =>
      withClue(s"mergeKey('$v') must NOT collapse into film 3: ")(
        mergeKey(withExtras, v) should not be third)
    }
  }

  // The touring "Federico Fellini: ciao a tutti!" retrospective (Kino Pałacowe,
  // Gdyńskie Centrum Filmowe, Pionier, … ~12 cinemas) brands each film with the
  // banner in many shapes: a "Federico Fellini:" / "Fellini." prefix, an optional
  // "ciao a tutti!" / "Ciao a tutti:" subtitle, and a "| / – przegląd Federico
  // Fellini …" suffix. The GlobalStructural strip already resolves the bare film on
  // TMDB (the searchTitle cases above), but the banner stayed in the MERGE KEY — so
  // the scraped decorated form keyed DIFFERENTLY from the bare TMDB display title
  // and `MovieCache.recordCinemaScrape` re-diverted the row into staging on every
  // 30-min scrape, never settling: Trójmiasto's served-films count flapped ~172↔200
  // (the swing was entirely Gdyńskie Centrum Filmowe). The canonical strip folds
  // every decorated form onto the bare film's key so the row settles.
  private val felliniDecorated = Seq(
    "Federico Fellini: Noce Cabirii"                       -> "Noce Cabirii",
    "Federico fellini: Noce Cabirii"                       -> "Noce Cabirii",
    "Federico Fellini: ciao a tutti! Słodkie życie"        -> "Słodkie życie",
    "FEDERICO FELLINI: ciao a tutti!: Wałkonie"            -> "Wałkonie",
    "Federico Fellini SŁODKIE ŻYCIE"                       -> "Słodkie życie",
    "Fellini. Noce Cabirii"                                -> "Noce Cabirii",
    "Fellini. Ciao a tutti: Wałkonie"                      -> "Wałkonie",
    "Słodkie życie | Federico Fellini: ciao a tutti!"      -> "Słodkie życie",
    "Wałkonie – przegląd FEDERICO FELLINI: ciao a tutti!"  -> "Wałkonie",
    // Suffix banner with the release year glued in BEFORE it ("<film> (1957) |
    // FEDERICO FELLINI …", Kino Roma's exact prod form, incl. a double space). The
    // year-in-title is mid-string while the banner is present, so the banner strip
    // must run BEFORE the trailing-paren-year strip — otherwise the year survives
    // ("nocecabirii1957") and the row never merges with the bare form, re-diverting
    // into staging every tick (ReScrapeIdempotencySpec).
    "Noce Cabirii (1957) | FEDERICO FELLINI: ciao a tutti!" -> "Noce Cabirii",
    "Wałkonie (1953) |  FEDERICO FELLINI: ciao a tutti!"    -> "Wałkonie"
  )

  it should "fold every Fellini-retrospective decoration onto the bare film's merge key (the Trójmiasto/GCF served-count flap)" in {
    felliniDecorated.foreach { case (decorated, bare) =>
      withClue(s"mergeKey('$decorated') vs '$bare': ")(
        mergeKey(withExtras, decorated) shouldBe mergeKey(withExtras, bare))
    }
  }

  it should "be load-bearing — the seed rules leave the Fellini banner in the key" in {
    felliniDecorated.foreach { case (decorated, bare) =>
      withClue(s"seedOnly.mergeKey('$decorated') should NOT yet equal '$bare': ")(
        mergeKey(seedOnly, decorated) should not be mergeKey(seedOnly, bare))
    }
  }

  // Ukrainian-language screenings are a DISTINCT version (a separate audience),
  // so the language/format strip must NOT eat a Ukrainian marker — the variant
  // keeps its own key and stays its own row, exactly like a dub/programme
  // edition. Only the Polish format suffixes above (2D DUBBING, /napisy, lektor)
  // fold; "ukraiński/ukrainian <dub|napisy|lektor>" survives untouched.
  it should "keep Ukrainian-language variants as their own row (not fold them into the base)" in {
    val ukrainian = Seq(
      "Dzień objawienia ukraiński dubbing",
      "Mandalorian i Grogu. Ukrainian dubbing",
      "Toy Story 5 ukraiński napisy",
      "Toy Story 5 (ukraiński dubbing)"
    )
    ukrainian.foreach { v =>
      withClue(s"canonical('$v') must be unchanged (Ukrainian marker preserved): ")(
        withExtras.canonical(v) shouldBe v)
      // And it must NOT collapse onto the base film's key.
      val base = v.replaceAll("(?i)\\s*\\(?\\s*ukrai[ńn]ski|ukrainian.*$", "").trim
      withClue(s"mergeKey('$v') should NOT equal the base '$base': ")(
        mergeKey(withExtras, v) should not be mergeKey(withExtras, base))
    }
  }

  // Negative controls: a real title that merely CONTAINS a digit+D or ends in an
  // unrelated word must not be eaten by the trailing-format / paren-year strips.
  it should "not strip format-shaped fragments from real titles" in {
    val unharmed = Seq(
      "Avatar 3D",            // bare 3D, no dub/napisy word behind it
      "2001: Odyseja kosmiczna",
      "Klub",                 // ends in 'b', must not look like 'dub'
      "Lektor",               // the bare word is a title here, not a suffix
      "1917",                 // a bare year that IS the title — not parenthesised, must survive
      "Blade Runner 2049",    // trailing year, but unparenthesised + part of the title
      "Top Gun: Maverick"
    )
    unharmed.foreach { t =>
      withClue(s"canonical('$t') unchanged: ")(withExtras.canonical(t) shouldBe t)
    }
  }

  // ── per-cinema (client) rules: venue junk stripped at ingestion ─────────────
  // `perCinema(slug, raw)` is what the owning client now runs in `cinemaClean`.

  private val perCinemaCases = Seq(
    ("kino-bajka",   "TOY STORY 5 2D DUB. SPS")               -> "TOY STORY 5",
    ("kino-bajka",   "VAIANA 2D DUB. SPS")                    -> "VAIANA",
    ("cyfrowe-kino", "Premiera! toy story 5")                 -> "toy story 5",
    ("kino-kijow",   "Diabeł ubiera się u Prady 2 Napisy PL") -> "Diabeł ubiera się u Prady 2",
    ("kino-kijow",   "Mawka: Prawdziwy mit UA Napisy PL")     -> "Mawka: Prawdziwy mit",
    ("kino-kijow",   "On drive UKR NAPISY PL")                -> "On drive",
    // Shared-portal venues (cleaned via Cinema.slug in their owning client).
    ("kino-oskard",     "Following/Kino Cafe")                -> "Following",
    ("kino-oskard",     "Supergirl/dubbing/Kino Cafe")        -> "Supergirl",
    ("kino-na-starowce", "Toy story 5 akcja lato w kinie")    -> "Toy story 5",
    ("kino-na-starowce", "Vaiana - film akcja lato w kinie")  -> "Vaiana",
    ("kino-stary-mlyn", "Toy Story 5 sensoryczny")            -> "Toy Story 5",
    ("kino-farys",      "Tot story 5")                        -> "Toy Story 5"
  )

  "ExtraTitleRules per-cinema rules" should "strip venue-specific junk for the owning cinema" in {
    perCinemaCases.foreach { case ((slug, raw), clean) =>
      withClue(s"perCinema('$slug', '$raw'): ")(withExtras.perCinema(slug, raw) shouldBe clean)
    }
  }

  it should "be load-bearing — the seed rules alone leave the raw title intact" in {
    perCinemaCases.foreach { case ((slug, raw), _) =>
      withClue(s"seedOnly.perCinema('$slug', '$raw'): ")(seedOnly.perCinema(slug, raw) shouldBe raw)
    }
  }

  it should "only fire for the owning cinema, not a cinema without that rule" in {
    perCinemaCases.foreach { case ((_, raw), _) =>
      withClue(s"perCinema('some-other-cinema', '$raw'): ")(
        withExtras.perCinema("some-other-cinema", raw) shouldBe raw)
    }
  }
}
