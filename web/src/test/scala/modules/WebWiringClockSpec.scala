package modules

import controllers.{DebugCountries, DebugStack, TestAdminAction}
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.UptimeMonitor
import services.auth.{AppleTokenValidator, OauthProfile, OauthProvider}
import tools.{HttpFetch, MutableClock}

import java.nio.charset.StandardCharsets.UTF_8
import java.security.interfaces.RSAPublicKey
import java.security.{KeyPairGenerator, Signature}
import java.time.{Duration, Instant, LocalDateTime}
import java.util.Base64

/**
 * The web tier reads ONE clock, `Wiring.clock`: every time-dependent answer the
 * composition root builds follows it. Each case pins the wiring's clock to a day in
 * 2020 and checks an output that would read the SYSTEM clock's year instead if its
 * component were wired without it — the shape of a spec that passes today and flips
 * on a date nobody picked.
 */
class WebWiringClockSpec extends AnyFlatSpec with Matchers {

  private val Pinned = Instant.parse("2020-06-10T08:00:00Z")

  private class ClockedWiring(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) extends TestWebWiring(seed) {
    val pinned = new MutableClock(Pinned)
    override lazy val clock: java.time.Clock = pinned
  }

  "the wiring's AuthExchangeCodes" should "expire a code on the wiring clock" in {
    val wiring = new ClockedWiring
    val code   = wiring.authExchangeCodes.mint("u1")
    wiring.pinned.advance(Duration.ofMinutes(3))
    wiring.authExchangeCodes.redeem(code) shouldBe None
  }

  "the wiring's AuthController" should "stamp a starting flow's state on the wiring clock" in {
    val wiring = new ClockedWiring {
      override lazy val oauthProviders: Map[String, OauthProvider] = Map("google" -> new OauthProvider {
        val name = "google"
        def authUrl(state: String, redirectUri: String): String = s"https://google.test/?state=$state"
        def exchangeCode(code: String, redirectUri: String): OauthProfile = OauthProfile("s", None, None, None)
      })
    }
    val start = wiring.authController.start("google")(FakeRequest("GET", "/auth/google/start"))
    session(start).get("oauthStateTimestamp").value shouldBe Pinned.toEpochMilli.toString
  }

  "the wiring's MetricsController" should "window recent uptime on the wiring clock" in {
    val wiring = new ClockedWiring
    wiring.uptimeMonitor.recordSuccess("Probe")
    contentAsString(wiring.metricsController.metrics(FakeRequest())) should include (
      s"""kinowo_uptime_recent_successes{country="${models.Country.fromEnv.code}",service="Probe"} 1""")
  }

  "the wiring's UptimeController" should "draw its bars up to the wiring clock's bucket" in {
    val wiring = new ClockedWiring { override lazy val adminAction = TestAdminAction() }
    wiring.uptimeMonitor.recordFailure("Probe", "boom")
    val page = contentAsString(wiring.uptimeController.index(
      FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)))
    page should include (s"""class="bar red" data-ts="${UptimeMonitor.bucketTimestamp(Pinned.toEpochMilli)}"""")
  }

  "the wiring's DebugController" should "age the read-mirror on the wiring clock" in {
    val wiring = new ClockedWiring {
      override lazy val debugCountries: DebugCountries = DebugCountries.of(
        new DebugStack(models.Country.fromEnv, movieRepository, stagingRepository, taskQueue, ratingCadenceReader,
          enrichmentAttemptReader, () => Seq.empty, () => Seq.empty, () => Pinned,
          mirrorFreshness = () => Some(Pinned.minusSeconds(300))),
        Map.empty, devMode = true)
    }
    contentAsString(wiring.debugController.cadence()(FakeRequest())) should include ("mirror 5m behind")
  }

  "the wiring's listing" should "label a showing day against the wiring clock's year" in {
    val record = MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(
      title     = Some("Clocked Film"),
      showtimes = Seq(models.Showtime(LocalDateTime.of(2020, 6, 10, 20, 0), None, None, Nil)))))
    val wiring = new ClockedWiring(Seq(("Clocked Film", None, record)))
    wiring.boot()
    val body = contentAsString(wiring.movieController.apiRepertoire("poznan")(FakeRequest()))
    body should include ("\"label\":\"Środa 10 czerwca\"")
  }

  "the wiring's UserStateController" should "stamp an account with no stored state on the wiring clock" in {
    val wiring = new ClockedWiring {
      override lazy val userRepository: services.users.UserRepository = new services.users.InMemoryUserRepository
    }
    wiring.userRepository.upsert(models.User(
      id = "newbie", provider = "google", providerSub = "G-newbie", email = Some("newbie@example.com"),
      displayName = None, avatarUrl = None, createdAt = Pinned, lastSeenAt = Pinned))
    val answer = wiring.userStateController.hiddenFilms("pl")(
      FakeRequest("GET", "/api/me/pl/hidden-films").withSession("userId" -> "newbie", "sessionVersion" -> "0"))
    status(answer) shouldBe OK
    header("Last-Modified", answer).value shouldBe "Wed, 10 Jun 2020 08:00:00 GMT"
  }

  "the wiring's AppleTokenValidator" should "judge a token's expiry on the wiring clock" in {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(2048)
    val keys = generator.generateKeyPair()
    val b64  = Base64.getUrlEncoder.withoutPadding
    val pub  = keys.getPublic.asInstanceOf[RSAPublicKey]
    val jwks = s"""{"keys":[{"kid":"k1","n":"${b64.encodeToString(pub.getModulus.toByteArray)}","e":"${b64.encodeToString(pub.getPublicExponent.toByteArray)}"}]}"""
    val wiring = new ClockedWiring {
      override lazy val httoFetch: HttpFetch = new HttpFetch {
        def get(url: String): String = jwks
        def post(url: String, body: String, contentType: String): String = fail(s"unexpected POST $url")
      }
    }
    def part(json: String) = b64.encodeToString(json.getBytes(UTF_8))
    val body = part("""{"kid":"k1","alg":"RS256"}""") + "." + part(
      s"""{"iss":"https://appleid.apple.com","aud":"dev.kinowo.Kinowo","sub":"apple-1","exp":${Pinned.getEpochSecond + 600}}""")
    val signer = Signature.getInstance("SHA256withRSA")
    signer.initSign(keys.getPrivate)
    signer.update(body.getBytes(UTF_8))
    val token = body + "." + b64.encodeToString(signer.sign())
    val validator: AppleTokenValidator = wiring.appleTokenValidator.value
    validator.validate(token, None).sub shouldBe "apple-1"
  }
}
