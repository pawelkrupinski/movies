package services.sharecards

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

class ShareCardInputsSpec extends AnyFlatSpec with Matchers {

  private val inputs = ShareCardInputs.of(film(), Country.default)

  "Card inputs" should "hash to the same name for the same picture and a new one for any drawn change" in {
    ShareCardInputs.of(film(), Country.default).drawnHash shouldBe inputs.drawnHash
    inputs.drawnHash should have length 10
    Seq(
      film(title = "Diuna 2"), film(ratings = ratings.copy(metascore = Some(80))), film().copy(synopsis = Some("Inny opis.")),
      film().copy(releaseYear = Some(2022)), film().copy(genres = Seq("Dramat")), film().copy(directors = Seq("Ktoś"))
    ).map(ShareCardInputs.of(_, Country.default).drawnHash) should not contain inputs.drawnHash
    inputs.copy(template = inputs.template + 1).drawnHash should not be inputs.drawnHash
    ShareCardInputs.of(film(), Country.UnitedKingdom).drawnHash should not be inputs.drawnHash
  }

  it should "hash ratings at the precision the card draws them, so an invisible change is no new card" in {
    val drawnAlike = film(ratings = ratings.copy(imdb = Some(7.84)))           // still "7.8"
    ShareCardInputs.of(drawnAlike, Country.default).drawnHash shouldBe inputs.drawnHash
    ShareCardInputs.of(film(ratings = ratings.copy(imdb = Some(7.86))), Country.default).drawnHash should not be inputs.drawnHash
  }

  it should "version the card by the poster it was drawn from, not by the whole candidate list" in {
    val churned = ShareCardInputs.of(film().copy(fallbackPosterUrls = Seq("https://cinema.example/other.jpg")), Country.default)
    churned.drawnHash shouldBe inputs.drawnHash
    churned.candidateVersions.head shouldBe inputs.candidateVersions.head // drawn from the primary: same card
    inputs.version(Some("https://cdn.example/poster-a.jpg")) shouldBe inputs.candidateVersions.head
    inputs.version(Some("https://cdn.example/b.jpg")) should not be inputs.candidateVersions.head
  }

  it should "draw the director label in the deployment's language" in {
    inputs.director shouldBe Some("Reżyseria: Denis Villeneuve")
    ShareCardInputs.of(film(), Country.Germany).director shouldBe Some("Regie: Denis Villeneuve")
  }

  it should "survive the task payload unchanged" in {
    ShareCardInputs.fromPayload(inputs.toPayload) shouldBe Some(inputs)
    val bare = inputs.copy(year = None, genres = Nil, imdb = None, director = None, synopsis = None)
    ShareCardInputs.fromPayload(bare.toPayload) shouldBe Some(bare)
    ShareCardInputs.fromPayload(Map("filmId" -> "x")) shouldBe None
  }

  "A card's name and URL" should "carry a plain film id, hash any other, and name the version" in {
    ShareCardFile.token("f0123456789abcd") shouldBe "f0123456789abcd"
    ShareCardFile.token("diuna|2021~dune") should fullyMatch regex "h[0-9a-f]{20}"
    ShareCardFile.name("f0123456789abcd") shouldBe "f0123456789abcd.jpg"
    val version = inputs.candidateVersions.head
    version should fullyMatch regex "[0-9a-f]{16}"
    val url = ShareCardFile.url("f0123456789abcd", version)
    url shouldBe s"f0123456789abcd.jpg?v=$version"
    ShareCardFile.versionOf(url).map(_.drawnHash) shouldBe Some(inputs.drawnHash)
    ShareCardFile.versionOf(url).map(_.posterHash) shouldBe Some(ShareCardFile.posterHash(Some("https://cdn.example/poster-a.jpg")))
    ShareCardFile.fileOf(url) shouldBe "f0123456789abcd.jpg"
  }
}
