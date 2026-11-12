package clients.odeon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import services.cinemas.uk.OdeonAuthHarvester

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import scala.concurrent.duration._

class OdeonAuthHarvesterSpec extends AnyFlatSpec with Matchers {

  /** A JWT whose payload carries `exp` (seconds) — header/signature are ignored. */
  private def jwt(expSeconds: Long): String = {
    val enc = (s: String) => Base64.getUrlEncoder.withoutPadding.encodeToString(s.getBytes(UTF_8))
    s"${enc("""{"alg":"RS256"}""")}.${enc(Json.obj("exp" -> expSeconds).toString)}.sig"
  }

  "extractToken" should "pull window.initialData.api.authToken out of page HTML" in {
    val html = """<script>window.initialData={"api":{"url":"https://vwc.odeon.co.uk","authToken":"abc.def.ghi"}};</script>"""
    OdeonAuthHarvester.extractToken(html) shouldBe Some("abc.def.ghi")
  }

  it should "yield None when the token isn't present" in {
    OdeonAuthHarvester.extractToken("<html>no token here</html>") shouldBe None
  }

  "jwtExpiryMillis" should "decode the exp claim to epoch millis" in {
    OdeonAuthHarvester.jwtExpiryMillis(jwt(1_900_000_000L)) shouldBe Some(1_900_000_000_000L)
  }

  it should "yield None for a malformed token" in {
    OdeonAuthHarvester.jwtExpiryMillis("not-a-jwt") shouldBe None
  }

  // The cache is the point: harvest once, then serve from memory until the token is
  // within refreshMargin of its own exp — so one background scrape every ~10h pays
  // the slow Zyte browser fetch, not every tick.
  "token()" should "harvest once and serve the cache until near expiry" in {
    var calls = 0
    var nowMs = 1_000_000_000_000L
    val expMs = nowMs + 12.hours.toMillis
    val h = new OdeonAuthHarvester(
      fetchPage = () => { calls += 1; Some(s"""x"authToken":"${jwt(expMs / 1000)}"x""") },
      now = () => nowMs, refreshMargin = 2.hours)

    h.token() shouldBe defined
    h.token(); h.token()
    calls shouldBe 1                                   // cached — one harvest for three reads

    nowMs = expMs - 90.minutes.toMillis                // inside the 2h refresh margin
    h.token() shouldBe defined
    calls shouldBe 2                                   // re-harvested
  }

  // 2026-11-04 17:04-17:20: a harvest that failed was asked again by every Odeon chunk that wanted a token,
  // 131 paid Zyte browser fetches (and 131 warnings) in 16 minutes. A failure is remembered for a cooldown:
  // the venues ride their flicks fallback meanwhile, and the next harvest is tried once it has run out.
  it should "return None when the harvest fails, and not ask Zyte again until the cooldown has run out" in {
    var calls = 0
    var nowMs = 1_000_000_000_000L
    val h = new OdeonAuthHarvester(fetchPage = () => { calls += 1; None }, now = () => nowMs)
    h.token() shouldBe None
    (1 to 50).foreach(_ => h.token() shouldBe None)
    calls shouldBe 1

    nowMs += OdeonAuthHarvester.FailureCooldown.toMillis
    h.token() shouldBe None
    calls shouldBe 2
  }

  // ...and a refresh that fails inside the margin keeps serving the token it already holds while that is unexpired.
  it should "keep serving a still-valid token when its early refresh fails" in {
    var nowMs = 1_000_000_000_000L
    val expMs = nowMs + 12.hours.toMillis
    var up    = true
    val h = new OdeonAuthHarvester(
      fetchPage = () => if (up) Some(s"""x"authToken":"${jwt(expMs / 1000)}"x""") else None,
      now = () => nowMs, refreshMargin = 2.hours)
    val first = h.token()
    first shouldBe defined

    up = false
    nowMs = expMs - 90.minutes.toMillis
    h.token() shouldBe first
    nowMs = expMs
    h.token() shouldBe None
  }

  it should "re-harvest after invalidate() (mid-life 401)" in {
    var calls = 0
    val nowMs = 1_000_000_000_000L
    val h = new OdeonAuthHarvester(
      fetchPage = () => { calls += 1; Some(s"""x"authToken":"${jwt(nowMs / 1000 + 12 * 3600)}"x""") },
      now = () => nowMs)
    h.token(); calls shouldBe 1
    h.invalidate()
    h.token(); calls shouldBe 2
  }

  // The token harvest is a paid Zyte request like any other, and was the one Zyte
  // call the paid-egress counter could not see.
  "zyteFetchPage's metering" should "record each browserHtml call's outcome on the paid-egress meter" in {
    val seen  = scala.collection.mutable.ListBuffer.empty[String]
    val meter: tools.HttpOutcomeRecorder = (o: String) => seen += o

    OdeonAuthHarvester.meteredBrowserHtml(meter)(200 -> """{"browserHtml":"<html/>"}""") shouldBe Some("<html/>")
    OdeonAuthHarvester.meteredBrowserHtml(meter)(401 -> "denied") shouldBe None
    OdeonAuthHarvester.meteredBrowserHtml(meter)(throw new java.net.http.HttpTimeoutException("slow")) shouldBe None

    seen.toList shouldBe List(tools.HttpOutcome.Success, tools.HttpOutcome.Http401, tools.HttpOutcome.Timeout)
  }
}

