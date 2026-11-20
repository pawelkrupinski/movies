package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.ImdbSearch
import clients.tools.FixtureFile

class ImdbSearchSpec extends AnyFlatSpec with Matchers {

  /** An IMDb answering GraphQL POSTs by the query each body carries: `answers` maps a marker in the body to the reply. */
  private def graphQlAnswering(answers: (String, String)*): (ImdbSearch, scala.collection.mutable.ListBuffer[String]) = {
    val asked = scala.collection.mutable.ListBuffer.empty[String]
    val http = new tools.HttpFetch {
      def get(url: String): String = throw new IllegalStateException(s"no GET expected: $url")
      def post(url: String, body: String, contentType: String): String = {
        asked += body
        answers.collectFirst { case (marker, reply) if body.contains(marker) => reply }
          .getOrElse(throw new IllegalStateException(s"unexpected query: $body"))
      }
    }
    (new ImdbSearch(http), asked)
  }
  private def fixture(name: String): String =
    FixtureFile.resource(s"fixtures/imdb/$name")

  "searchTitles" should "read IMDb's own title search in its order, films only, by a title in any language" in {
    // Recorded 2026-10-04: mainSearch for the Polish "Diuna" finds the films IMDb titles "Dune" (via their Polish AKA)
    // and two series, which a cinema listing never is.
    val (imdb, asked) = graphQlAnswering("TitleSearch" -> fixture("title_search_diuna_films_and_series.json"))
    imdb.searchTitles("Diuna").map(t => (t.id, t.title, t.year, t.titleType)).take(4) shouldBe Seq(
      ("tt1160419", "Dune", Some(2021), "movie"), ("tt15239678", "Dune: Part Two", Some(2024), "movie"),
      ("tt31378509", "Dune: Part Three", Some(2026), "movie"), ("tt0087182", "Dune", Some(1984), "movie"))
    imdb.searchTitles("Diuna").map(_.id) should contain noneOf ("tt10466872", "tt0142032")
    asked.head should include ("\"text\":\"Diuna\"")
    imdb.searchTitles("  ") shouldBe Nil
  }

  it should "throw on a reply that is not IMDb's answer" in {
    val (imdb, _) = graphQlAnswering("TitleSearch" -> "<html><body>Request blocked</body></html>")
    a [tools.UnexpectedBodyException] should be thrownBy imdb.searchTitles("Diuna")
  }

  "directedBy" should "walk the directing credits of the first people IMDb's name search finds" in {
    // Recorded 2026-10-04: the name search for "Denis Villeneuve" finds him first (nm0898288), then two others.
    val (imdb, asked) = graphQlAnswering(
      "NameSearch"                   -> fixture("name_search_denis_villeneuve.json"),
      "\"id\":\"nm0898288\""         -> fixture("directing_credits_nm0898288.json"),
      "\"id\":\"nm11095446\""        -> """{"data":{"name":{"credits":{"edges":[]}}}}""")
    val films = imdb.directedBy("Denis Villeneuve")
    films should contain allOf ("tt1160419", "tt15239678", "tt31378509")
    films.distinct shouldBe films
    asked.count(_.contains("DirectingCredits")) shouldBe ImdbSearch.DirectorsAsked
    asked.exists(_.contains("nm0898293")) shouldBe false
  }
}
