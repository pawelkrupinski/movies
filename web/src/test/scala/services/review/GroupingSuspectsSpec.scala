package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The suspect groupings the review page reads ([[GroupingSuspects]]): the reports `scripts/ci/grouping_suspects.py` writes. */
class GroupingSuspectsSpec extends AnyFlatSpec with Matchers {
  private val report =
    """{"country": "uk", "flagged": 1, "suspects": [
      | {"rank": 1, "score": 1.0, "flagged": true, "film": "tmdb:963568", "title": "National Theatre Live: Prima Facie", "size": 4,
      |  "signals": ["alone"], "findings": [{"kind": "prefers", "signal": "alone", "weight": 1.0, "why": "Chichester alone takes tmdb:1127282"}],
      |  "judged_right": 0, "judged_wrong": 1, "involved": 1,
      |  "members": [{"venue": "Chichester", "rawTitle": "Prima Facie", "involved": true}, {"venue": "Power Station", "rawTitle": "NT Live: Prima Facie", "involved": false}]},
      | {"rank": 2, "score": 0.6, "flagged": false, "film": "imdb:tt1", "title": "Lalka", "size": 2, "signals": ["facts"], "findings": [], "members": []}]}""".stripMargin

  "the reports" should "be read per country, each suspect with its findings and the members they name" in {
    val dir = Files.createTempDirectory("grouping-suspects")
    Files.writeString(dir.resolve("grouping-suspects-uk.json"), report)
    Files.writeString(dir.resolve("grouping-suspects-de.json"), "not json")
    Files.writeString(dir.resolve("other.json"), "{}")
    val read = GroupingSuspects.read(dir)
    read.keySet shouldBe Set("uk")
    val prima = read("uk").head
    prima.findings shouldBe Seq("prefers: Chichester alone takes tmdb:1127282")
    prima.involved shouldBe Seq("Chichester 'Prima Facie'")
    prima.filmUrl shouldBe Some("https://www.themoviedb.org/movie/963568")
    read("uk")(1).filmUrl shouldBe None
  }

  "the ranking" should "keep the flagged ones of the countries asked for, the most suspect first, at most the limit" in {
    val uk = GroupingSuspects.of("uk", play.api.libs.json.Json.parse(report))
    val pl = Seq(uk.head.copy(country = "pl", score = 2.0, film = "tmdb:2"), uk.head.copy(country = "pl", score = 1.5, film = "tmdb:3"))
    GroupingSuspects.ranked(Map("uk" -> uk, "pl" -> pl), None, 10).map(_.film) shouldBe Seq("tmdb:2", "tmdb:3", "tmdb:963568")
    GroupingSuspects.ranked(Map("uk" -> uk, "pl" -> pl), Some(Set("uk")), 10).map(_.film) shouldBe Seq("tmdb:963568")
    GroupingSuspects.ranked(Map("uk" -> uk, "pl" -> pl), None, 1).map(_.film) shouldBe Seq("tmdb:2")
  }

  "the directory" should "be the environment's, else target/grouping-suspects, and none when it is not there" in {
    val dir = Files.createTempDirectory("grouping-suspects")
    GroupingSuspects.locate(settings.GroupingSuspectsDirectory(dir)) shouldBe Some(dir)
    GroupingSuspects.locate(settings.GroupingSuspectsDirectory(dir.resolve("missing"))) shouldBe None
  }
}
