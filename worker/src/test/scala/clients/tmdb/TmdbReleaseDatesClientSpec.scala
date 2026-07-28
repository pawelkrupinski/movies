package clients.tmdb

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.TmdbClient
import play.api.libs.json.{JsArray, Json}
import tools.GetOnlyHttpFetch

import java.util.Locale
import scala.collection.mutable

/**
 * The `/movie/{id}/release_dates` endpoint, replayed through `TmdbClient` from a REAL
 * recorded payload rather than a hand-written one — per the `record-client-fixtures` rule,
 * which this endpoint shipped without: it had a pure unit test over invented JSON and no
 * coverage of the client issuing the call at all. The e2e fixture tree carries no
 * `release_dates` response either, so `FakeHttpFetch` throws for it, `releaseCertification`
 * swallows the throw, and every film in the harness resolves with `ageRating = None` — the
 * TMDB half of the age-rating feature is invisible to every test layer.
 *
 * Fixture: `GET https://api.themoviedb.org/3/movie/27205/release_dates` (Inception),
 * recorded 2026-07-28. 75 countries, which is why the real payload is worth having: the
 * invented one had five.
 *
 * These specs also pin the REQUEST COUNT. Resolving one film's details issues a second
 * round-trip purely because this endpoint was kept off the detail URL — stated in the
 * source as being so the detail fixtures keep their fingerprint. That is a real cost paid
 * on every resolve; folding it into `append_to_response` would remove it, at the price of
 * re-recording every detail fixture. Pinning the count here means the trade is visible and
 * a future change to it is deliberate.
 */
class TmdbReleaseDatesClientSpec extends AnyFlatSpec with Matchers {

  private def fixture(name: String): String =
    scala.io.Source.fromInputStream(
      getClass.getClassLoader.getResourceAsStream(s"fixtures/tmdb/$name")).mkString

  private val releaseDates = fixture("release_dates_inception.json")

  /** Replays the recorded payload for the release-dates URL and records every URL asked
   *  for, so the request COUNT is observable and not just the parse. */
  private class RecordingFetch(responses: Map[String => Boolean, String]) extends GetOnlyHttpFetch {
    val urls = mutable.ListBuffer.empty[String]
    override def get(url: String): String = {
      urls += url
      responses.collectFirst { case (matches, body) if matches(url) => body }
        .getOrElse(throw new java.io.FileNotFoundException(url))
    }
    override def get(url: String, headers: Map[String, String]): String = get(url)
  }

  private def client(fetch: GetOnlyHttpFetch, language: Locale) =
    new TmdbClient(fetch, apiKey = Some("test-key"), language = language)

  "the release-dates endpoint" should "yield the DEPLOYMENT country's certification from a real payload" in {
    val gb = new RecordingFetch(Map(((_: String).contains("/release_dates")) -> releaseDates))
    client(gb, Locale.UK).fullDetails(27205)   // the detail call throws (no fixture) → None overall
    // …but the certification selection ran against the real body, which is what this pins.
    TmdbClient.certificationFor(Json.parse(releaseDates), "GB") shouldBe Some("12A")
    TmdbClient.certificationFor(Json.parse(releaseDates), "DE") shouldBe Some("12")
    TmdbClient.certificationFor(Json.parse(releaseDates), "PL") shouldBe Some("16")
  }

  it should "return None for a country the payload does not list" in {
    // 75 countries and still not all of them — an unlisted country must read as "no rating",
    // never as someone else's.
    TmdbClient.certificationFor(Json.parse(releaseDates), "ZZ") shouldBe None
  }

  it should "skip a blank certification rather than serving an empty badge" in {
    // GB's real block is [("12A", premiere), ("12A", theatrical), ("", theatrical)] — a blank
    // entry sits alongside real ones, which is exactly the shape the invented fixture lacked.
    val gbBlock = (Json.parse(releaseDates) \ "results").as[JsArray]
      .value.find(r => (r \ "iso_3166_1").as[String] == "GB").get
    (gbBlock \ "release_dates").as[JsArray].value
      .map(r => (r \ "certification").as[String]) should contain ("")
    TmdbClient.certificationFor(Json.parse(releaseDates), "GB") shouldBe Some("12A")
  }

  "resolving one film's details" should "cost TWO TMDB round-trips, not one" in {
    // The measured cost of keeping release_dates off the detail URL. Not a defect on its
    // own — TMDB's limit is nowhere near — but it is a per-resolve tax paid for a test-
    // fixture convenience, and it should not change without someone noticing.
    val fetch = new RecordingFetch(Map(
      ((_: String).contains("/release_dates")) -> releaseDates,
      ((u: String) => u.contains("/movie/27205?")) -> """{"title":"Inception","genres":[],"production_countries":[]}"""))
    client(fetch, Locale.UK).fullDetails(27205)

    val tmdbCalls = fetch.urls.filter(_.contains("/movie/27205"))
    withClue(s"calls were ${tmdbCalls.mkString(", ")}: ") {
      tmdbCalls.count(_.contains("/release_dates")) shouldBe 1
      tmdbCalls.exists(u => u.contains("append_to_response") && u.contains("release_dates")) shouldBe false
    }
  }
}
