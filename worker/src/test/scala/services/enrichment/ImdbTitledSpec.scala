package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

/** Which of IMDb's suggestions it lists under a title in some language (`ImdbClient.titled`), read from recorded
 *  suggestion and title answers. */
class ImdbTitledSpec extends AnyFlatSpec with Matchers {
  private def loadFixture(path: String): String = scala.io.Source.fromResource(path.stripPrefix("/"))(using scala.io.Codec.UTF8).mkString

  "IMDb's titles of a film" should "list its own, its original and every AKA, each once" in {
    val titles = ImdbClient.titlesIn(Json.parse(loadFixture("/fixtures/imdb/akas_compostelle_polish_title.json")))
    titles.take(2) shouldBe Seq("Santiago: The Camino Therapy", "Compostelle")
    titles should contain("Camino dla opornych")
    titles.distinct shouldBe titles
  }

  "The suggestions IMDb lists under a title" should "keep one displayed under it, accents aside, without asking its titles" in {
    // "Kuźma" (PL): IMDb displays tt43338336 as "Kuzma", which IS the title once accents are off.
    val movies = ImdbClient.movieSuggestions(Json.parse(loadFixture("/fixtures/imdb/suggestion_kuzma.json")))
    ImdbClient.titled("Kuźma", movies, _ => fail("asked titles of a suggestion displayed under the title")) shouldBe Some(Seq("tt43338336"))
  }

  it should "keep one whose AKA is the title, drop one whose titles are not, and be unknown while titles are" in {
    val movies = ImdbClient.movieSuggestions(Json.parse(loadFixture("/fixtures/imdb/suggestion_camino_dla_opornych.json")))
    val akas   = ImdbClient.titlesIn(Json.parse(loadFixture("/fixtures/imdb/akas_compostelle_polish_title.json")))
    ImdbClient.titled("Camino dla opornych", movies, id => Some(if (id == "tt39814688") akas else Seq("Something Else"))) shouldBe Some(Seq("tt39814688"))
    ImdbClient.titled("Camino dla opornych", movies, _ => None) shouldBe None
    ImdbClient.titled("Camino", movies, id => Some(if (id == "tt39814688") akas else Nil)) shouldBe Some(Nil)
  }
}
