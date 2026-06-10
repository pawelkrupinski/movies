package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/**
 * The no-regression gate for migrating the hardcoded `TitleNormalizer` regexes
 * into editable DB rules. It pins a FROZEN copy of the pre-rules implementation
 * (the exact regex chains as they were before this change) and asserts that the
 * rule-driven `TitleNormalizer.searchTitle` / `apiQuery` / `sanitize` produce
 * byte-identical output for every title in a corpus that exercises each pattern.
 *
 * If this fails, the seed rules in `TitleRuleDefaults` drifted from the original
 * behaviour — fix the seed, never this frozen baseline.
 */
class TitleRuleMigrationSpec extends AnyFlatSpec with Matchers {

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

  "TitleRuleDefaults-driven TitleNormalizer" should "match the frozen legacy searchTitle for every corpus title" in {
    TitleNormalizer.resetToDefaults()
    corpus.foreach { t =>
      withClue(s"searchTitle('$t'): ")(TitleNormalizer.searchTitle(t) shouldBe Legacy.searchTitle(t))
    }
  }

  it should "match the frozen legacy apiQuery for every corpus title" in {
    TitleNormalizer.resetToDefaults()
    corpus.foreach { t =>
      withClue(s"apiQuery('$t'): ")(TitleNormalizer.apiQuery(t) shouldBe Legacy.apiQuery(t))
    }
  }

  it should "match the frozen legacy sanitize for every corpus title" in {
    TitleNormalizer.resetToDefaults()
    corpus.foreach { t =>
      withClue(s"sanitize('$t'): ")(TitleNormalizer.sanitize(t) shouldBe Legacy.sanitize(t))
    }
  }

  it should "match the frozen legacy programmePrefix for every corpus title" in {
    TitleNormalizer.resetToDefaults()
    corpus.foreach { t =>
      withClue(s"programmePrefix('$t'): ")(TitleNormalizer.programmePrefix(t) shouldBe Legacy.programmePrefix(t))
    }
  }
}
