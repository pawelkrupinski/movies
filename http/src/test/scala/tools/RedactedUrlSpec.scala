package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the log-safety of a URL that authenticates in its query string.
 *
 * The live leak this closes: a deleted TMDB id rescheduled forever, and every
 * attempt logged `HTTP 404 for GET .../movie/1715017/external_ids?api_key=<v3 key>`
 * into `/data/logs/worker.log`, which persists on the Fly volume for 14 days and
 * is read out with `kubectl logs` during any incident.
 *
 * The message SHAPE is load-bearing and asserted separately: `MonitoringHttpFetch`
 * classifies connection failures with `HTTP 5\d\d .*`, so redaction may replace a
 * value but must never reorder or drop anything.
 */
class RedactedUrlSpec extends AnyFlatSpec with Matchers {

  private val TmdbKey = "eac2a2461a660ace84da62a03259f1e5"

  "RedactedUrl" should "mask a TMDB v3 api_key" in {
    val url = s"https://api.themoviedb.org/3/movie/1715017/external_ids?api_key=$TmdbKey"

    RedactedUrl(url) should not include TmdbKey
    RedactedUrl(url) shouldBe "https://api.themoviedb.org/3/movie/1715017/external_ids?api_key=***"
  }

  it should "mask OMDb's apikey and Firestore's key, whatever their position" in {
    RedactedUrl("https://www.omdbapi.com/?t=Dune&type=movie&apikey=abc123") shouldBe
      "https://www.omdbapi.com/?t=Dune&type=movie&apikey=***"
    RedactedUrl("https://firestore.googleapis.com/v1/documents/seanse?key=AIzaSecret&pageSize=300") shouldBe
      "https://firestore.googleapis.com/v1/documents/seanse?key=***&pageSize=300"
  }

  it should "match the parameter name case-insensitively" in {
    RedactedUrl("https://x/y?API_KEY=abc") shouldBe "https://x/y?API_KEY=***"
  }

  it should "leave a URL without a query string, and non-secret parameters, untouched" in {
    RedactedUrl("https://api.themoviedb.org/3/movie/550") shouldBe "https://api.themoviedb.org/3/movie/550"
    RedactedUrl("https://x/y?language=pl-PL&include_adult=false") shouldBe "https://x/y?language=pl-PL&include_adult=false"
  }

  it should "mask the OAuth credentials the sign-in providers send in a query string" in {
    // Facebook's code exchange is a GET carrying the app secret; its debug_token and
    // Google's tokeninfo carry a visitor's own bearer token. A failed call (an expired
    // code is a routine 400) logs the URL through HttpStatusException.
    RedactedUrl("https://graph.facebook.com/v19.0/oauth/access_token?client_id=1&client_secret=s3cr3t&code=c") shouldBe
      "https://graph.facebook.com/v19.0/oauth/access_token?client_id=1&client_secret=***&code=***"
    RedactedUrl("https://graph.facebook.com/debug_token?input_token=user&access_token=app") shouldBe
      "https://graph.facebook.com/debug_token?input_token=***&access_token=***"
    RedactedUrl("https://oauth2.googleapis.com/tokeninfo?id_token=jwt") shouldBe
      "https://oauth2.googleapis.com/tokeninfo?id_token=***"
    RedactedUrl("https://x/token?refresh_token=r") shouldBe "https://x/token?refresh_token=***"
  }

  it should "not sweep a fragment into the last parameter's value" in {
    RedactedUrl("https://x/y?api_key=abc#section") shouldBe "https://x/y?api_key=***#section"
  }

  it should "leave a valueless flag alone rather than inventing an '=' for it" in {
    RedactedUrl("https://x/y?debug&api_key=abc") shouldBe "https://x/y?debug&api_key=***"
  }

  "HttpStatusException" should "keep the key out of the message every caller logs" in {
    val url       = s"https://api.themoviedb.org/3/movie/1715017/external_ids?api_key=$TmdbKey"
    val exception = new HttpStatusException(404, "GET", url, retryAfter = None)

    exception.getMessage should not include TmdbKey
    exception.getMessage shouldBe
      "HTTP 404 for GET https://api.themoviedb.org/3/movie/1715017/external_ids?api_key=***"
    // The raw url stays available to callers that re-issue or inspect the request —
    // redaction is a rendering concern, not a storage one.
    exception.url shouldBe url
  }

  it should "preserve the message shape MonitoringHttpFetch's 5xx classifier matches on" in {
    val exception = new HttpStatusException(503, "GET", "https://x/y?api_key=abc", retryAfter = None)

    exception.getMessage.matches("HTTP 5\\d\\d .*") shouldBe true
  }

  // A convergence leg lists a POST it could not answer, body and all, in a public release asset — only when the body
  // names no credential the way a URL's would be masked.
  "RedactedUrl.carriesCredential" should "find a credential in a form or JSON body, and nothing in a GraphQL query" in {
    RedactedUrl.carriesCredential("grant_type=client_credentials&client_secret=s3cret") shouldBe true
    RedactedUrl.carriesCredential("""{"api_key":"abc","query":"x"}""") shouldBe true
    RedactedUrl.carriesCredential("""{"Token": "abc"}""") shouldBe true
    RedactedUrl.carriesCredential("""{"query":"query Titles($id:ID!){title(id:$id){titleText{text}}}","variables":{"id":"tt1"}}""") shouldBe false
    RedactedUrl.carriesCredential("monkey=1&keystone=2") shouldBe false
  }
}
