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

  it should "return None (and not cache) when the harvest fails" in {
    var calls = 0
    val h = new OdeonAuthHarvester(fetchPage = () => { calls += 1; None })
    h.token() shouldBe None
    h.token() shouldBe None
    calls shouldBe 2                                   // no cache of a failure — retried each call
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
}
