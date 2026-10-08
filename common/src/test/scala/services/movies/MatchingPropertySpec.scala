package services.movies

import tools.SpecClock.given

import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators.{genFilmEvidence, genCandidate}
import services.resolution.{FilmEvidence, Verdict}

/**
 * The matching rules as METAMORPHIC properties: transform a title or a piece of
 * evidence in a way that cannot change which film it names (or, for an instalment,
 * in a way that always does), and the decision must move — or stay — accordingly.
 *
 * `MatchingCorporaSpec` replays the pairs that went wrong; this generates the pairs
 * that have not gone wrong YET. Every property is asked of the real decision
 * functions (`SequelMarker`, `TitleContainment`, `sanitize`, `searchKey`,
 * `groupByFilm` + `clusterByFilm`, `SamePerson`, `Verdict`), so it runs at unit speed.
 */
class MatchingPropertySpec extends IdentityPropertySpec {


  private def tokens(t: String) = TitleContainment.tokens(t)

  /** One normalizer for the suite, not a rule-set compile per property case. */
  private val titleNormalizer = SingleCountryNormalizer.titleNormalizer

  // ── Instalments ────────────────────────────────────────────────────────────

  /** Series titles in the catalogue's languages, none ending in a numeral of its own. */
  private val seriesBases: Seq[String] = Seq(
    "Toy Story", "Rocky", "Diuna", "Kingsman", "Szybcy i wściekli", "Obcy",
    "The Hunger Games: Mockingjay", "Mortal Kombat", "Minionki", "Zaplątani")

  private val Roman = Map(2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V", 6 -> "VI", 7 -> "VII", 8 -> "VIII", 9 -> "IX")
  private val EnglishWords = Map(2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "Five")
  private val PolishWords  = Map(2 -> "druga", 3 -> "trzecia", 4 -> "czwarta", 5 -> "piąta")

  /** Every way a venue or TMDB numbers instalment `n` of `base`. */
  private def notations(base: String, n: Int): Seq[String] =
    Seq(s"$base $n", s"$base: Part $n", s"$base - Part $n", s"$base Pt $n", s"$base: Część $n",
        s"$base: Chapter $n", s"$base Vol. $n") ++
      Roman.get(n).map(r => s"$base $r") ++
      EnglishWords.get(n).map(w => s"$base: Part $w") ++
      PolishWords.get(n).map(w => s"$base: Część $w")

  private val genInstalments: Gen[(String, Int, Int)] = for {
    base <- Gen.oneOf(seriesBases)
    n    <- Gen.choose(2, 9)
    m    <- Gen.choose(2, 9).suchThat(_ != n)
  } yield (base, n, m)

  private def genNotation(base: String, n: Int): Gen[String] = Gen.oneOf(notations(base, n))

  "titles differing only in an instalment ordinal" should "always read as different instalments, in any notation" in {
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) }) {
      case (a, b) =>
        SequelMarker(LatestTitleYear.current).differentInstalments(tokens(a), tokens(b)) shouldBe true
        SequelMarker(LatestTitleYear.current).differentInstalments(tokens(b), tokens(a)) shouldBe true
    }
  }

  it should "never decorate one another, nor share a merge key" in {
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) }) {
      case (a, b) =>
        TitleContainment.decorates(tokens(a), tokens(b), LatestTitleYear.current) shouldBe false
        TitleContainment.decorates(tokens(b), tokens(a), LatestTitleYear.current) shouldBe false
        titleNormalizer.sanitize(a) should not be titleNormalizer.sanitize(b)
    }
  }

  it should "stay different instalments when either side carries a rerelease year or a format tag" in {
    val trailing = Seq(" (2026)", " (2026 Re-Release)", " 4K", " 3D", " - Re-Release")
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) },
           Gen.oneOf(trailing)) { case ((a, b), tag) =>
      SequelMarker(LatestTitleYear.current).differentInstalments(tokens(a + tag), tokens(b)) shouldBe true
      SequelMarker(LatestTitleYear.current).differentInstalments(tokens(b), tokens(a + tag)) shouldBe true
    }
  }

  "two notations of ONE instalment" should "never read as different instalments" in {
    forAll(Gen.oneOf(seriesBases), Gen.choose(2, 9)) { (base, n) =>
      val all = notations(base, n)
      for (a <- all; b <- all) withClue(s"'$a' vs '$b': ") {
        SequelMarker(LatestTitleYear.current).differentInstalments(tokens(a), tokens(b)) shouldBe false
      }
    }
  }

  // ── Programme decoration ──────────────────────────────────────────────────

  /** Films a venue decorates, including a numbered instalment (a decoration must not
   *  read as — or hide — an ordinal). */
  private val multiTokenNames: Seq[String] = Seq(
    "Enyedi Ildikó", "Dag Johan Haugerud", "Michel Franco", "Jan Sobierajski", "Yann Gozlan",
    "Bong Joon Ho", "Alejandro González Iñárritu", "Francis Lawrence", "Szabó István", "Pálfi György",
    "Agnieszka Holland", "Neele Leana Vollmar")

  private val genReordered: Gen[(String, String)] = for {
    name <- Gen.oneOf(multiTokenNames)
    seed <- Gen.long
  } yield name -> permute(seed, name.split(" ").toSeq).mkString(" ")

  "a director credit with its name tokens reordered" should "name the same person" in {
    forAll(genReordered) { case (name, reordered) =>
      SamePerson(name, reordered) shouldBe true
      SamePerson(reordered, name) shouldBe true
    }
  }

  it should "leave the verdict on any candidate unchanged" in {
    forAll(genFilmEvidence, genCandidate, genReordered, Gen.oneOf(true, false)) { case (evidence, candidate, (name, reordered), onCrew) =>
      val crewed = if (onCrew) candidate.copy(crew = candidate.crew :+ name) else candidate
      Verdict.of(evidence.withDirectors(Seq(reordered)), crewed) shouldBe Verdict.of(evidence.withDirectors(Seq(name)), crewed)
    }
  }

  // ── Corroborating evidence ────────────────────────────────────────────────

  "evidence that agrees with a candidate" should "never turn an accepted candidate into a rejected one" in {
    forAll(genFilmEvidence, genCandidate, Gen.choose(0, 2)) { (evidence, candidate, which) =>
      whenever(!Verdict.of(evidence, candidate).isReject) {
        val corroborated: FilmEvidence = which match {
          case 0 => candidate.crew.headOption.fold(evidence)(c => evidence.withDirectors(Seq(c)))
          case 1 => candidate.runtime.fold(evidence)(r => evidence.copy(runtimes = (evidence.runtimes :+ r).distinct.sorted))
          case _ => candidate.year.fold(evidence)(y => evidence.copy(years = (evidence.years :+ y).distinct.sorted))
        }
        Verdict.of(corroborated, candidate).isReject shouldBe false
      }
    }
  }
}
