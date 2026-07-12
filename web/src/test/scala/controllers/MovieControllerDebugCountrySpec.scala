package controllers

import models.{CinemaCityWroclavia, Country, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.Instant

/**
 * The Dev-only `/debug` country switch: with per-country debug stacks wired, a
 * `?country=xx` param (sticky via cookie) makes the corpus table read THAT
 * country's Mongo db SAME-ORIGIN, and the navbar switcher emits `?country=`
 * links instead of navigating to the other country's production host (which
 * 404s /debug). In the single-country holder prod + other specs use, the param
 * is ignored.
 */
class MovieControllerDebugCountrySpec extends AnyFlatSpec with Matchers {

  private def corpusStack(country: Country, title: String) = new DebugStack(
    country,
    new services.movies.InMemoryMovieRepository(Seq(
      (title, Some(2021), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some(title))))))),
    services.staging.StagingRepository.empty,
    new services.tasks.InMemoryTaskQueue,
    services.cadence.RatingCadenceReader.empty,
    () => Seq.empty, () => Seq.empty, () => Instant.EPOCH)

  private val plStack = corpusStack(Country.Poland, "Pl Only Film")
  private val ukStack = corpusStack(Country.UnitedKingdom, "Uk Only Film")

  private def switchingController =
    TestMovieController.build(Seq.empty, Mode.Dev, debugCountries = Some(
      DebugCountries.of(plStack, Map(Country.UnitedKingdom -> ukStack), devMode = true)))._1

  "GET /debug?country=uk" should "read the UK corpus and stamp the sticky cookie" in {
    val result = switchingController.debug().apply(FakeRequest(GET, "/debug?country=uk"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("Uk Only Film")
    html should not include ("Pl Only Film")
    cookies(result).get(DebugCountries.CookieName).map(_.value) shouldBe Some("uk")
  }

  it should "render a SAME-ORIGIN ?country= switcher, not a link to the UK production host" in {
    val html = contentAsString(switchingController.debug().apply(FakeRequest(GET, "/debug?country=uk")))
    // The switcher stays on this origin…
    html should include("?country=uk")
    html should include("?country=pl")
    // …and never sends you to the UK prod deployment (which 404s /debug).
    html should not include ("showtimes-uk.fly.dev")
    // The switched-to country is the selected option.
    html should include("""value="/debug?country=uk" selected""")
  }

  "GET /debug (cookie only)" should "keep the UK selection across the plain tab links" in {
    val html = contentAsString(switchingController.debug().apply(
      FakeRequest(GET, "/debug").withCookies(play.api.mvc.Cookie(DebugCountries.CookieName, "uk"))))
    html should include("Uk Only Film")
    html should not include ("Pl Only Film")
  }

  "GET /debug (no selection)" should "read the boot country's corpus" in {
    val html = contentAsString(switchingController.debug().apply(FakeRequest(GET, "/debug")))
    html should include("Pl Only Film")
    html should not include ("Uk Only Film")
  }

  "GET /debug?country=uk with the switch off (single country)" should "ignore the param and read the boot corpus" in {
    val ctrl = TestMovieController.build(Seq.empty, Mode.Dev,
      debugCountries = Some(DebugCountries.single(plStack)))._1
    val result = ctrl.debug().apply(FakeRequest(GET, "/debug?country=uk"))
    val html   = contentAsString(result)
    html should include("Pl Only Film")
    html should not include ("Uk Only Film")
    cookies(result).get(DebugCountries.CookieName) shouldBe None
  }
}
