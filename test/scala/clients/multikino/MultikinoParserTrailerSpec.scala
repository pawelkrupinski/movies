package clients.multikino

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.MultikinoParser

/**
 * The recorded `cinemas/0011/films` fixture has empty `trailers: []` arrays
 * on every film — the recording happened during a window where the upstream
 * API was returning the boolean `hasTrailer` without the URL list populated.
 * That fixture stays the source of truth for the rest of MultikinoClient's
 * shape; for the trailer field we exercise the parser against a synthetic
 * minimal payload that ships the field shape we care about.
 *
 * If the live API later populates `trailers` again, swap in a recorded
 * fixture and delete this synthetic spec.
 */
class MultikinoParserTrailerSpec extends AnyFlatSpec with Matchers {

  private def filmJson(trailers: String, title: String = "Test Film"): String = s"""
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
      "trailers": $trailers
    }]}
  """

  "MultikinoParser" should "lift a single YouTube trailer URL out of the trailers array" in {
    val movies = MultikinoParser.parse(filmJson("""["https://www.youtube.com/watch?v=dQw4w9WgXcQ"]"""))
    movies.head.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
  }

  it should "canonicalise an /embed/ URL back to watch?v= form" in {
    val movies = MultikinoParser.parse(filmJson("""["https://www.youtube.com/embed/dQw4w9WgXcQ"]"""))
    movies.head.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
  }

  it should "return None when trailers is empty" in {
    val movies = MultikinoParser.parse(filmJson("""[]"""))
    movies.head.trailerUrl shouldBe None
  }

  it should "drop non-YouTube trailer URLs (until we learn how to embed them)" in {
    val movies = MultikinoParser.parse(filmJson("""["https://cdn.multikino.pl/raw/trailer.mp4"]"""))
    movies.head.trailerUrl shouldBe None
  }
}
