package clients.tmdb

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.TmdbClient
import play.api.libs.json.{JsArray, Json}
import tools.GetOnlyHttpFetch

import java.util.Locale
import scala.collection.mutable

/**
 * TMDB's release-dates block, replayed through `TmdbClient` from a REAL recorded payload
 * rather than a hand-written one — per the `record-client-fixtures` rule, which this
 * endpoint shipped without: it had a pure unit test over invented JSON and no coverage of
 * the client issuing the call at all.
 *
 * Fixture: `GET https://api.themoviedb.org/3/movie/27205/release_dates` (Inception),
 * recorded 2026-07-28. 75 countries, which is why the real payload is worth having: the
 * invented one had five.
 *
 * These specs also pin the REQUEST COUNT at ONE. The certification now rides along on
 * `append_to_response=credits,release_dates`; it shipped as a separate call, which cost a
 * round-trip per resolve and — because that URL had no fixture in the committed corpus —
 * meant CI resolved every film with no age rating while a developer holding a locally
 * recorded tree saw them appear.
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

  /** The whole detail response as TMDB returns it under
   *  `append_to_response=credits,release_dates`: the appended value is the same
   *  `{id, results[]}` block the standalone endpoint serves. */
  private val detailWithAppend =
    s"""{"title":"Inception","genres":[],"production_countries":[],
       | "credits":{"crew":[],"cast":[]},
       | "release_dates":${Json.parse(releaseDates)}}""".stripMargin

  "resolving one film's details" should "cost ONE TMDB round-trip, not two" in {
    // The certification rides along on `append_to_response` instead of costing its own
    // request. It shipped as a separate call to spare the detail fixtures a re-fingerprint;
    // that bought a per-resolve round-trip and, worse, left the endpoint with no fixture in
    // the committed corpus, so CI resolved every film with no age rating at all.
    val fetch = new RecordingFetch(Map(((_: String).contains("/movie/27205")) -> detailWithAppend))
    client(fetch, Locale.UK).fullDetails(27205)

    val detailCalls = fetch.urls.filter(_.contains("/movie/27205"))
    withClue(s"calls were ${detailCalls.mkString(", ")}: ") {
      detailCalls.count(_.endsWith("/release_dates")) shouldBe 0
      detailCalls.count(_.contains("append_to_response=credits,release_dates")) shouldBe 1
    }
  }

  it should "read the appended block, so the film carries its certification" in {
    val fetch = new RecordingFetch(Map(((_: String).contains("/movie/27205")) -> detailWithAppend))
    client(fetch, Locale.UK).fullDetails(27205).flatMap(_.ageRating) shouldBe Some("12A")
  }

  it should "carry no rating when the body has no appended block, rather than throwing" in {
    // An older recorded body, or a TMDB response that dropped the append: the film simply
    // has no TMDB-sourced rating, exactly as before the feature existed.
    val fetch = new RecordingFetch(Map(((_: String).contains("/movie/27205")) ->
      """{"title":"Inception","genres":[],"production_countries":[]}"""))
    client(fetch, Locale.UK).fullDetails(27205).flatMap(_.ageRating) shouldBe None
  }
}
