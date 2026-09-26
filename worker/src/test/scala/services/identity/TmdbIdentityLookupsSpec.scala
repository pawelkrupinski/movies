package services.identity

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The resolver's TMDB lookups over RECORDED answers: a film's identity record is read by the
 *  calibration's own parser from the two answers the client already asks for it, and a request
 *  the recording cannot answer is `Unknown`, never "no film". */
class TmdbIdentityLookupsSpec extends AnyFlatSpec with Matchers {

  private val fetch   = new FakeHttpFetch("08-06-2026", strict = true)
  private val tmdb    = new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("replay")), retrySleep = (_: Long) => ())
  private val lookups = new TmdbIdentityLookups(tmdb, Nil)

  "a film's identity record" should "carry what the calibrated measures read: title, year, runtime, directors, countries" in {
    val film = lookups.film(1018).toOption.flatten.get
    film.title should not be empty
    film.year shouldBe defined
    film.runtime shouldBe defined
    film.directors.get should not be empty
    film.countries.get.foreach(_ should fullyMatch regex "[A-Z]{2}")
  }

  it should "be Unknown when the recording holds no answer for it" in {
    lookups.film(999999999) shouldBe Answer.Unknown
  }
}
