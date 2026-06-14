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

  // The pure equivalents of TitleNormalizer.{apiQuery,sanitize}, computed off the
  // default rule set (the global-free script bits — Arabic→Roman `normalize`, NFD
  // `deburr`, the final strip — stay as the production code).
  private def apiQuery(t: String): String    = rs.search(t)
  private def programmePrefix(t: String): Option[String] = rs.programmePrefix(t)
  private def sanitize(t: String): String =
    tools.TextNormalization.deburr(rs.canonical(normalize(t)))
      .toLowerCase.replaceAll("[^\\p{L}\\p{N}]+", "")

  // ── Frozen legacy implementation ──
  // Global normalisation comes from the shared FrozenLegacyNormalizer; the
  // per-cinema methods (verbatim from each client's pre-rules cleanTitle) live
  // here since they're only needed by this spec.
  private object Legacy {
    def apiQuery(display: String): String        = FrozenLegacyNormalizer.apiQuery(display)
    def programmePrefix(t: String): Option[String] = FrozenLegacyNormalizer.programmePrefix(t)
    def sanitize(title: String): String          = FrozenLegacyNormalizer.sanitize(title)

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

    // Per-cinema: Kino Alternatywy (verbatim from AlternatywyClient.cleanTitle).
    def alternatywy(alt: String): String =
      alt.replaceFirst("(?i)^okładka\\s*", "")
        .replaceAll("[„“”‟\"]", " ")
        .replaceAll("\\s+", " ")
        .trim

    // Per-cinema: Kino Pałacowe (verbatim from KinoPalacoweClient.cleanTitle).
    def kinoPalacowe(title: String): String =
      title.stripPrefix("Poranek dla dzieci: ").stripPrefix("DKF Zamek: ").stripPrefix("WAJDA: re-wizje. ")

    // Per-cinema: Kinematograf Łódź (verbatim from KinematografLodzClient.cleanTitle).
    def kinematograf(raw: String): String = {
      val noRez = """,\s*reż\.\s*.+$""".r.replaceFirstIn(raw.trim, "").trim
      val noDirector = """,\s+\p{Lu}\S+\s+\p{Lu}\S+$""".r.replaceFirstIn(noRez, "").trim
      """\s*\(\d{4}\)\s*$""".r.replaceFirstIn(noDirector, "").trim
    }

    // Per-cinema: Kino Apollo (verbatim from KinoApolloClient.cleanTitle).
    def kinoApollo(title: String): String =
      title.stripPrefix("DZIEŃ DZIECKA W APOLLO - ").stripSuffix(" - seans przedpremierowy")

    // Per-cinema: Helios (verbatim foldLeft from HeliosNuxt.cleanTitle, pre-rules).
    def helios(title: String): String =
      Seq(" w Helios RePlay", " w Helios Anime", " w Helios na Scenie", " w HnS",
          " - Salon Kultury Helios", " - KNTJ", " - KNT", " - Kino Kobiet",
          " - Kino Konesera", " - seanse z konkursami HDD", " - Event projekt",
          " - dubbing", " - Dubbing", " - napisy", " - NAP", " - DUB", " - AF")
        .foldLeft(title)((t, suffix) => t.stripSuffix(suffix))

    // Per-cinema: BoK (verbatim from BokClient.cleanTitle).
    private val BokPromoTag = """\s*\|\s*[A-ZĄĆĘŁŃÓŚŹŻ0-9 ]{3,}\s*$""".r
    def bok(raw: String): String = {
      val noNbsp  = raw.replace(' ', ' ').replaceAll("\\s+", " ").trim
      val noPromo = BokPromoTag.replaceFirstIn(noNbsp, "")
      noPromo.replaceAll("\\s*\\|\\s*", ": ").trim
    }
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

  "TitleRuleDefaults" should "match the frozen legacy apiQuery for every corpus title" in {
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

  private val alternatywyCorpus = Seq(
    "Okładka „Anora\"",
    "Okładka Mufasa",
    "„Flying Lion\"  Adam Święs Trio",
    "Zwyczajny tytuł"  // untouched but for ws-collapse
  )

  "the kino-alternatywy per-cinema rules" should "match the frozen legacy AlternatywyClient.cleanTitle" in {
    alternatywyCorpus.foreach { t =>
      withClue(s"perCinema('kino-alternatywy', '$t'): ")(
        rs.perCinema("kino-alternatywy", t) shouldBe Legacy.alternatywy(t))
    }
  }

  private val kinoPalacoweCorpus = Seq(
    "Poranek dla dzieci: Pszczółka Maja",
    "DKF Zamek: Stalker",
    "WAJDA: re-wizje. Popiół i diament",
    "Anora"  // untouched
  )

  "the kino-palacowe per-cinema rules" should "match the frozen legacy KinoPalacoweClient.cleanTitle" in {
    kinoPalacoweCorpus.foreach { t =>
      withClue(s"perCinema('kino-palacowe', '$t'): ")(
        rs.perCinema("kino-palacowe", t) shouldBe Legacy.kinoPalacowe(t))
    }
  }

  private val kinematografCorpus = Seq(
    "Znaki Pana Śliwki (2025), reż. Urszula Morga, Bartosz Mikołajczyk",
    "Zawieście czerwone latarnie (1991), Zhang Yimou",
    "Klasyk w kinie: Rozmowa (1973)",
    "Anora"  // untouched
  )

  "the kino-kinematograf per-cinema rules" should "match the frozen legacy KinematografLodzClient.cleanTitle" in {
    kinematografCorpus.foreach { t =>
      withClue(s"perCinema('kino-kinematograf', '$t'): ")(
        rs.perCinema("kino-kinematograf", t) shouldBe Legacy.kinematograf(t))
    }
  }

  private val bokCorpus = Seq(
    "Tajny agent | SENIORZY",                 // trailing ALL-CAPS promo → dropped
    "Kino dla Seniora | Tajny agent",          // banner | film → "Banner: Film"
    "Diuna Cz. II",                       // nbsp → space
    "Anora"                                     // untouched
  )

  "the bok per-cinema rules" should "match the frozen legacy BokClient.cleanTitle" in {
    bokCorpus.foreach { t =>
      withClue(s"perCinema('bok', '$t'): ")(
        rs.perCinema("bok", t) shouldBe Legacy.bok(t))
    }
  }

  private val kinoApolloCorpus = Seq(
    "DZIEŃ DZIECKA W APOLLO - Pszczółka Maja",
    "Diuna Cz. II - seans przedpremierowy",
    "Anora"  // untouched
  )

  "the kino-apollo per-cinema rules" should "match the frozen legacy KinoApolloClient.cleanTitle" in {
    kinoApolloCorpus.foreach { t =>
      withClue(s"perCinema('kino-apollo', '$t'): ")(
        rs.perCinema("kino-apollo", t) shouldBe Legacy.kinoApollo(t))
    }
  }

  private val heliosCorpus = Seq(
    "Anora w HnS",
    "Diuna - napisy",
    "Mufasa - dubbing - Event projekt",   // order-dependent double strip
    "Diabeł - KNT",
    "Top Gun: Maverick"                    // untouched
  )

  "the helios per-cinema rules" should "match the frozen legacy HeliosNuxt.cleanTitle" in {
    heliosCorpus.foreach { t =>
      withClue(s"perCinema('helios', '$t'): ")(
        rs.perCinema("helios", t) shouldBe Legacy.helios(t))
    }
  }
}
