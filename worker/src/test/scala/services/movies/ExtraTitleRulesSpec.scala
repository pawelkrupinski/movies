package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ExtraTitleRules
import services.titlerules.{TitleRuleDefaults, TitleRuleSet}

/** Locks the behaviour of the post-baseline [[ExtraTitleRules]] before they're
 *  applied to prod. The "load-bearing" pair on each case is the gate: the seed
 *  rules ALONE must NOT strip the banner (fail-before), and seed + extras MUST
 *  (pass-after) — so a rule that silently stops matching is caught here, not in
 *  prod.
 *
 *  `withExtras` is what prod runs once `ApplyExtraTitleRules` lands (seed rules
 *  + the additions); `seedOnly` is today's behaviour. */
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
    "Seans filmowy dla rodziców: Ojczyzna"            -> ("Seans filmowy dla rodziców: ",      "Ojczyzna")
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
    "Niesamowite przygody skarpetek 3 - KNTJ"                  -> "Niesamowite przygody skarpetek 3",
    "Opętanie – pokaz specjalny na 60. Urodziny kina zamek"    -> "Opętanie",
    "Mistrzynie | POKAZ PRZEDPREMIEROWY"                       -> "Mistrzynie",
    "Lawa – Tadeusz Konwicki"                                  -> "Lawa",
    "Brzezina - Andrzej Wajda o filmie"                        -> "Brzezina"
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
    "Pan Li - Seans Spirytustyczny"
  )

  it should "never touch a plain colon/word title" in {
    untouched.foreach { t =>
      withClue(s"search('$t'): ")(withExtras.search(t) shouldBe t)
      withClue(s"structural('$t'): ")(withExtras.structural(t) shouldBe t)
      withClue(s"programmePrefix('$t'): ")(withExtras.programmePrefix(t) shouldBe None)
    }
  }
}
