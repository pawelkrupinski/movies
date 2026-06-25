package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SynopsisSimilaritySpec extends AnyFlatSpec with Matchers {

  // Two Filmweb/TMDB-style blurbs of the SAME film: same distinctive nouns
  // (chłopiec, pies, zaśnieżone góry, zaginiony ojciec) but paraphrased,
  // reordered, and Polish-inflected (chłopca/chłopiec, podróż/podróży).
  private val boyAndDogTmdb =
    "Historia chłopca, który wraz ze swoim psem wyrusza w niebezpieczną podróż " +
      "przez zaśnieżone góry, by odnaleźć zaginionego ojca."
  private val boyAndDogFilmweb =
    "Mały chłopiec ze swoim wiernym psem przemierza zaśnieżone góry w " +
      "niebezpiecznej podróży, szukając zaginionego ojca."

  // A DIFFERENT film — shares only filler words with the reference.
  private val spaceStation =
    "Grupa naukowców na stacji kosmicznej odkrywa obcą formę życia, która " +
      "zagraża całej załodze podczas misji wokół Jowisza."

  "similarity" should "score an identical text at 1.0" in {
    SynopsisSimilarity.similarity(boyAndDogTmdb, boyAndDogTmdb) shouldBe (1.0 +- 1e-9)
  }

  it should "ignore case, diacritics, and inline URLs (shared normalization)" in {
    val withNoise = "HISTORIA CHLOPCA, ktory wraz ze swoim psem wyrusza w " +
      "niebezpieczna podroz https://youtu.be/abc przez zasniezone gory, by " +
      "odnalezc zaginionego ojca."
    SynopsisSimilarity.similarity(boyAndDogTmdb, withNoise) shouldBe (1.0 +- 0.05)
  }

  it should "rate a same-film paraphrase well above a different film" in {
    val same = SynopsisSimilarity.similarity(boyAndDogTmdb, boyAndDogFilmweb)
    val diff = SynopsisSimilarity.similarity(boyAndDogTmdb, spaceStation)
    same should be > 0.3
    same should be > (diff * 2)
  }

  it should "return 0.0 for empty or too-short input" in {
    SynopsisSimilarity.similarity("", boyAndDogTmdb) shouldBe 0.0
    SynopsisSimilarity.similarity("ok", boyAndDogTmdb) shouldBe 0.0
  }

  "confidentTieBreak" should "pick the same-film paraphrase over an unrelated blurb" in {
    SynopsisSimilarity.confidentTieBreak(boyAndDogTmdb, Seq(spaceStation, boyAndDogFilmweb)) shouldBe Some(1)
  }

  it should "return None when the best candidate doesn't clear the minimum" in {
    // Reference vs two unrelated blurbs — nothing should be promoted.
    val unrelatedB = "Komedia o grupie przyjaciół, którzy otwierają food truck nad morzem."
    SynopsisSimilarity.confidentTieBreak(boyAndDogTmdb, Seq(spaceStation, unrelatedB)) shouldBe None
  }

  it should "return None when two candidates are within the margin of each other" in {
    // Both candidates match the reference about equally → no confident winner.
    SynopsisSimilarity.confidentTieBreak(boyAndDogTmdb, Seq(boyAndDogFilmweb, boyAndDogFilmweb)) shouldBe None
  }

  it should "return None for a single candidate or an empty reference" in {
    SynopsisSimilarity.confidentTieBreak(boyAndDogTmdb, Seq(boyAndDogFilmweb)) shouldBe None
    SynopsisSimilarity.confidentTieBreak("", Seq(boyAndDogFilmweb, spaceStation)) shouldBe None
  }
}
