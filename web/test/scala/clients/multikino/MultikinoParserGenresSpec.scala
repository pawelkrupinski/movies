package clients.multikino

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.MultikinoParser

/**
 * Multikino's `cinemas/0011/films` API exposes a `genres` field. The recorded
 * fixture has `[]` across every film in the current Poznań catalog, so we
 * exercise the parser against a synthetic payload that ships both shapes
 * we've seen the field take: a list of `{name}` objects, and a list of bare
 * strings. If the live API standardises one shape and starts populating it,
 * swap in a recorded fixture and trim this spec.
 */
class MultikinoParserGenresSpec extends AnyFlatSpec with Matchers {

  private def filmJson(genres: String, title: String = "Test Film"): String = s"""
    {"result":[{
      "filmTitle": "$title",
      "filmId": "TST",
      "filmUrl": "/filmy/test",
      "posterImageSrc": "/poster.jpg",
      "synopsisShort": "",
      "cast": "",
      "director": "",
      "originalTitle": "",
      "movieXchangeCode": "",
      "showingGroups": [],
      "trailers": [],
      "genres": $genres
    }]}
  """

  "MultikinoParser" should "leave genres empty when the array is empty (current production shape)" in {
    MultikinoParser.parse(filmJson("[]")).head.movie.genres shouldBe empty
  }

  it should "leave genres empty when the field is absent" in {
    val json = """{"result":[{
      "filmTitle":"X","filmId":"1","filmUrl":"/x","posterImageSrc":"/p.jpg",
      "synopsisShort":"","cast":"","director":"","originalTitle":"",
      "movieXchangeCode":"","showingGroups":[],"trailers":[]
    }]}"""
    MultikinoParser.parse(json).head.movie.genres shouldBe empty
  }

  it should "extract genre names from a list of {name} objects" in {
    val genres = """[{"name":"Komedia"},{"name":"Romans"}]"""
    MultikinoParser.parse(filmJson(genres)).head.movie.genres shouldBe Seq("Komedia", "Romans")
  }

  it should "extract genre names from a list of bare strings" in {
    val genres = """["Dramat","Historyczny"]"""
    MultikinoParser.parse(filmJson(genres)).head.movie.genres shouldBe Seq("Dramat", "Historyczny")
  }

  it should "skip entries with neither a name nor a string value" in {
    val genres = """[{"name":"Komedia"},{},{"name":""},"Sci-Fi"]"""
    MultikinoParser.parse(filmJson(genres)).head.movie.genres shouldBe Seq("Komedia", "Sci-Fi")
  }
}
