package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Cookie
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.Instant

/**
 * The per-request country resolution behind the Dev-only `/debug` country switch:
 * `?country=` wins and is stamped into a sticky cookie, the cookie carries the
 * selection across the plain tab links, and the switch is OFF (param ignored) in
 * the single-country holder prod + tests use.
 */
class DebugCountriesSpec extends AnyFlatSpec with Matchers {

  private def stack(country: Country) = new DebugStack(
    country,
    new services.movies.InMemoryMovieRepository(Nil),
    services.staging.StagingRepository.empty,
    new services.tasks.InMemoryTaskQueue,
    services.cadence.RatingCadenceReader.empty,
    () => Seq.empty, () => Seq.empty, () => Instant.EPOCH)

  private val switching = DebugCountries.of(
    stack(Country.Poland), Map(Country.UnitedKingdom -> stack(Country.UnitedKingdom)), devMode = true)

  "resolve (switch on)" should "honour ?country= and stamp a sticky cookie" in {
    val request = FakeRequest(GET, "/debug?country=uk")
    switching.resolve(request) shouldBe Country.UnitedKingdom
    switching.selectionCookie(request).map(_.value) shouldBe Some("uk")
  }

  it should "fall back to the sticky cookie when no ?country= is present" in {
    val request = FakeRequest(GET, "/debug").withCookies(Cookie(DebugCountries.CookieName, "uk"))
    switching.resolve(request) shouldBe Country.UnitedKingdom
    // No explicit selection this request → nothing to (re)stamp.
    switching.selectionCookie(request) shouldBe None
  }

  it should "fall back to the boot country with no param and no cookie" in {
    switching.resolve(FakeRequest(GET, "/debug")) shouldBe Country.Poland
  }

  it should "ignore an unknown or not-wired country code" in {
    switching.resolve(FakeRequest(GET, "/debug?country=zz")) shouldBe Country.Poland
    // `de` is a real Country but has no stack wired here → falls back to boot.
    switching.resolve(FakeRequest(GET, "/debug?country=de")) shouldBe Country.Poland
  }

  "a single-country holder (prod / tests)" should "ignore ?country= entirely and never switch" in {
    val single = DebugCountries.single(stack(Country.Poland))
    single.switchable shouldBe false
    single.resolve(FakeRequest(GET, "/debug?country=uk")) shouldBe Country.Poland
    single.selectionCookie(FakeRequest(GET, "/debug?country=uk")) shouldBe None
  }
}
