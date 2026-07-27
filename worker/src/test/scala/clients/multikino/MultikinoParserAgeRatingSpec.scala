package clients.multikino

import clients.tools.FakeHttpFetch
import models.MultikinoZabrze
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{MultikinoClient, MultikinoParser}

/**
 * Multikino ships the Polish rating in the same Vue-platform
 * `certificate = {name, description, src}` shape the UK reads — `name` is the
 * label ("15+", "18+", "BO", "TBC"). The parser reads `certificate.name`
 * through [[services.cinemas.common.AgeRating.normalize]], keeping real ratings
 * verbatim while dropping the "TBC" placeholder and the Polish "no restriction"
 * marker "BO" (bez ograniczeń), which is not a rating to badge.
 *
 * The real values are asserted against the recorded `new-cities` Zabrze (0003)
 * fixture, which carries "15+" films alongside many null-certificate rows. "BO"
 * / "b.o." don't appear in any recorded corpus yet, so the no-restriction drop
 * is exercised with synthetic payloads (the same tactic MultikinoParserGenresSpec
 * uses for the field its fixture leaves empty).
 */
class MultikinoParserAgeRatingSpec extends AnyFlatSpec with Matchers {

  private def filmJson(certificate: String, title: String = "Test Film"): String = s"""
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
      "genres": [],
      "certificate": $certificate
    }]}
  """

  private def ageRatingOf(certificate: String): Option[String] =
    MultikinoParser.parse(filmJson(certificate)).head.ageRating

  "MultikinoParser" should "read the Polish rating from certificate.name (real fixture)" in {
    val movies  = new MultikinoClient(new FakeHttpFetch("new-cities"), "0003", MultikinoZabrze).fetch()
    val pasazer = movies.find(_.movie.rawTitle.exists(_.toLowerCase.contains("pasażer")))
    pasazer.flatMap(_.ageRating) shouldBe Some("15+")
    // The fixture carries exactly the two "15+" films it was recorded with…
    movies.count(_.ageRating.contains("15+")) shouldBe 2
    // …and the null-certificate rows collapse to None rather than a placeholder.
    movies.exists(_.ageRating.isEmpty) shouldBe true
  }

  it should "keep a real rating verbatim" in {
    ageRatingOf("""{"name":"18+","description":null,"src":null}""") shouldBe Some("18+")
    ageRatingOf("""{"name":"15+","description":null,"src":null}""") shouldBe Some("15+")
  }

  it should "drop the Polish no-restriction marker (BO / b.o.) as no rating" in {
    ageRatingOf("""{"name":"BO","description":null,"src":null}""")   shouldBe None
    ageRatingOf("""{"name":"b.o.","description":null,"src":null}""") shouldBe None
  }

  it should "drop the TBC placeholder and a null / absent certificate" in {
    ageRatingOf("""{"name":"TBC","description":null,"src":null}""")  shouldBe None
    ageRatingOf("""{"name":null,"description":null,"src":null}""")   shouldBe None
    ageRatingOf("{}")                                                shouldBe None
  }
}
