package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer.normalize

/**
 * The no-regression gate for migrating the hardcoded `TitleNormalizer` regexes
 * into editable DB rules. It pins a FROZEN copy of the pre-rules implementation
 * (the exact regex chains as they were before this change) and asserts that the
 * default rule set in `TitleRuleDefaults` produces byte-identical output for
 * every title in a corpus that exercises each pattern.
 *
 * Tests the PURE `TitleRuleDefaults.ruleSet` rather than the `TitleNormalizer`
 * process-global, so it can't be perturbed by a concurrently-running spec that
 * installs a different rule set.
 *
 * If this fails, the seed rules in `TitleRuleDefaults` drifted from the original
 * behaviour — fix the seed, never this frozen baseline.
 */
class TitleRuleMigrationSpec extends AnyFlatSpec with Matchers {

  private val rs = TitleRuleDefaults.ruleSet

  // The pure equivalents of TitleNormalizer.{searchTitle,apiQuery,sanitize},
  // computed off the default rule set (the global-free script bits — Arabic→Roman
  // `normalize`, NFD `deburr`, the final strip — stay as the production code).
  private def searchTitle(t: String): String = rs.structural(t)
  private def apiQuery(t: String): String    = rs.search(t)
  private def programmePrefix(t: String): Option[String] = rs.programmePrefix(t)
  private def sanitize(t: String): String =
    tools.TextNormalization.deburr(rs.canonical(normalize(t)))
      .toLowerCase.replaceAll("[^\\p{L}\\p{N}]+", "")

  // ── Frozen legacy implementation (verbatim from pre-rules TitleNormalizer) ──
  private object Legacy {
    private val CyklPrefix        = """^Cykl\s+[„"][^„""]*[„""]?\s+[-–—]\s+""".r
    private val SlashSuffix       = """\s+/\s+.+$""".r
    private val AnniversarySuffix = """(?i)\s*[-–—|.]?\s*\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""".r
    private val RestoredSuffix    = """(?i)\s*[-–—|.]?\s*\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""".r
    private val WersjaSuffix      = """(?i)\s*[-–—.]\s+wersja\s+\p{L}+\s*$""".r
    private val PlusSuffix        = """\s+\+\s+\p{L}[^)]*$""".r
    private val ProgrammePrefix   =
      ("""(?i)^(?:Kino\s+bez\s+barier|""" +
       """Pokaz\s+sensorycznie\s+przyjazny|""" +
       """Filmow[ey]\s+Poran(?:ki|ek)(?:\s+[^:]+)?|""" +
       """Zimowe\s+Poranki(?:\s+[^:]+)?|""" +
       """Poranek\s+dla\s+dzieci|""" +
       """Filmowy\s+Klub\s+Seniora|""" +
       """Dyskusyjny\s+Klub\s+Filmowy|""" +
       """Filmowe\s+spotkania\s+z\s+psychoanaliz[ąa]|""" +
       """Cinema\s+Italia\s+Oggi|""" +
       """Plenerowe\s+Pa[łl]acowe):\s+""").r
    private val AccessibilityTag  = """(?i)\s*\(\s*AD\b[^)]*\)?\s*$""".r

    def searchTitle(display: String): String = {
      val a = CyklPrefix.replaceFirstIn(display, "")
      val b = SlashSuffix.replaceFirstIn(a, "")
      val d = AnniversarySuffix.replaceFirstIn(b, "")
      val e = RestoredSuffix.replaceFirstIn(d, "")
      WersjaSuffix.replaceFirstIn(e, "").trim
    }
    def apiQuery(display: String): String = {
      val stripped  = ProgrammePrefix.replaceFirstIn(display, "")
      val tagless   = AccessibilityTag.replaceFirstIn(stripped, "")
      val eventless = PlusSuffix.replaceFirstIn(tagless, "")
      searchTitle(eventless)
    }
    def programmePrefix(title: String): Option[String] =
      ProgrammePrefix.findPrefixMatchOf(title).map(_.matched)

    private val ArabicToRoman = Map(
      "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
      "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
      "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
      "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX"
    )
    def normalize(title: String): String =
      title.split(" ").map(w => ArabicToRoman.getOrElse(w, w)).mkString(" ")
    private def canonical(t: String): String =
      searchTitle(t).stripPrefix("Gwiezdne Wojny: ").replace(" & ", " i ")
    def sanitize(title: String): String =
      tools.TextNormalization.deburr(canonical(normalize(title)))
        .toLowerCase
        .replaceAll("[^\\p{L}\\p{N}]+", "")

    // Per-cinema: Cinema City (verbatim from CinemaCityClient.cleanTitle).
    def cinemaCity(name: String): String =
      name.stripPrefix("Ladies Night - ")
        .stripSuffix(" - powrót do kin")
        .replaceFirst("^Kolekcja\\s+Mamoru\\s+Hosody:\\s*", "")

    // Per-cinema: Multikino (verbatim from MultikinoParser.cleanTitle, pre-rules).
    def multikino(filmTitle: String): String =
      filmTitle
        .replaceFirst("^Kino na obcasach:\\s*", "")
        .replaceFirst("^Kolekcja\\s+Mamoru\\s+Hosody:\\s*", "")

    // Per-cinema: Kino Muza (verbatim from KinoMuzaClient.cleanTitle, pre-rules).
    private val MuzaSeriesSuffix = """(?i)\s*\|\s*najlepsze\s+z\s+najgorszych\s*$""".r
    def kinoMuza(raw: String): String = MuzaSeriesSuffix.replaceFirstIn(raw, "").trim
  }

  // Corpus exercising every pattern + plain titles that must NOT be touched.
  private val corpus = Seq(
    "Top Gun: Maverick",
    "Mortal Kombat 2",
    "Mortal Kombat II",
    "Gwiezdne Wojny: Mandalorian i Grogu",
    "Mandalorian & Grogu",
    "Top Gun / 40th Anniversary",
    "Top Gun 40th Anniversary",
    "Pulp Fiction - 30th anniversary",
    "Cinema Paradiso - 35 rocznica",
    "Blade Runner 4K restored",
    "Blade Runner | 4K Remastered",
    """Cykl „Mistrzowie kina" – Rashomon""",
    "Amelia - wersja polska",
    "Kino bez barier: Freak Show (AD + CC + PJM)",
    "Pokaz sensorycznie przyjazny: Vaiana 2",
    "Filmowe Poranki - Miraculous: Biedronka i Czarny Kot",
    "Zimowe Poranki z Bobem Budowniczym: Bob",
    "Poranek dla dzieci: Pszczółka Maja",
    "Filmowy Klub Seniora: Ojczyzna",
    "DYSKUSYJNY KLUB FILMOWY: Vertigo",
    "Filmowe spotkania z psychoanalizą: Dobry chłopiec",
    "Cinema Italia Oggi: La Strada",
    "Ojczyzna + spotkanie z producentką Ewą Puszczyńską",
    "Orwell: 2 + 2 = 5",
    "Diabeł ubiera się u Prady 2",
    "Pizza & Pasta",
    "Rocznica",
    "Star Wars: A New Hope"
  )

  "TitleRuleDefaults" should "match the frozen legacy searchTitle for every corpus title" in {
    corpus.foreach { t =>
      withClue(s"searchTitle('$t'): ")(searchTitle(t) shouldBe Legacy.searchTitle(t))
    }
  }

  it should "match the frozen legacy apiQuery for every corpus title" in {
    corpus.foreach { t =>
      withClue(s"apiQuery('$t'): ")(apiQuery(t) shouldBe Legacy.apiQuery(t))
    }
  }

  it should "match the frozen legacy sanitize for every corpus title" in {
    corpus.foreach { t =>
      withClue(s"sanitize('$t'): ")(sanitize(t) shouldBe Legacy.sanitize(t))
    }
  }

  it should "match the frozen legacy programmePrefix for every corpus title" in {
    corpus.foreach { t =>
      withClue(s"programmePrefix('$t'): ")(programmePrefix(t) shouldBe Legacy.programmePrefix(t))
    }
  }

  // ── per-cinema migrations ──────────────────────────────────────────────────
  private val cinemaCityCorpus = Seq(
    "Ladies Night - Narodziny gwiazdy",
    "Top Gun - powrót do kin",
    "Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas",
    "Kolekcja Mamoru Hosody:   Spacer w chmurach",
    "Diabeł ubiera się u Prady 2", // untouched
    "Ladies Night - Anora - powrót do kin" // prefix + suffix together
  )

  "the cinema-city per-cinema rules" should "match the frozen legacy CinemaCityClient.cleanTitle" in {
    cinemaCityCorpus.foreach { t =>
      withClue(s"perCinema('cinema-city', '$t'): ")(
        rs.perCinema("cinema-city", t) shouldBe Legacy.cinemaCity(t))
    }
  }

  private val multikinoCorpus = Seq(
    "Kino na obcasach: Anora",
    "Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas",
    "Top Gun: Maverick",          // untouched (real colon title)
    "Mufasa: Król Lew"            // untouched
  )

  "the multikino per-cinema rules" should "match the frozen legacy MultikinoParser.cleanTitle" in {
    multikinoCorpus.foreach { t =>
      withClue(s"perCinema('multikino', '$t'): ")(
        rs.perCinema("multikino", t) shouldBe Legacy.multikino(t))
    }
  }

  private val kinoMuzaCorpus = Seq(
    "Wszystko wszędzie naraz | najlepsze z najgorszych",
    "The Room | NAJLEPSZE Z NAJGORSZYCH",
    "Anora"  // untouched
  )

  "the kino-muza per-cinema rules" should "match the frozen legacy KinoMuzaClient.cleanTitle" in {
    kinoMuzaCorpus.foreach { t =>
      withClue(s"perCinema('kino-muza', '$t'): ")(
        rs.perCinema("kino-muza", t) shouldBe Legacy.kinoMuza(t))
    }
  }
}
