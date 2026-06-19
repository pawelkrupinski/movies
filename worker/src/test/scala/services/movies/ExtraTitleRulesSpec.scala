package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ExtraTitleRules
import services.movies.TitleNormalizer.normalize
import services.titlerules.{TitleRuleDefaults, TitleRuleSet}

import java.util.Locale

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
    "Mandalorian i Grogu. Ukrainian dubbing",
    "Mandalorian i Grogu/dubbing",
    "Mandalorian i Grogu/napisy",
    "The Mandalorian and Grogu 2D DUB",
    "The Mandalorian and Grogu 2D NAP"
  )

  private val canonicalKey = mergeKey(seedOnly, canonicalFilm) // "mandalorianigrogu"

  "ExtraTitleRules canonical merges" should "fold every Mandalorian i Grogu spelling onto one key" in {
    (seedAlreadyMerges ++ additionsMerge).foreach { v =>
      withClue(s"mergeKey('$v'): ")(mergeKey(withExtras, v) shouldBe canonicalKey)
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
      withClue(s"mergeKey('$variant') vs '$base': ")(
        mergeKey(withExtras, variant) shouldBe mergeKey(withExtras, base))
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
      withClue(s"mergeKey('$variant') vs '$base': ")(
        mergeKey(withExtras, variant) shouldBe mergeKey(withExtras, base))
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
}
