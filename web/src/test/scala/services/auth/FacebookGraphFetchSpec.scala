package services.auth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.facebook.{FacebookSlots, FacebookUsageBrake}
import tools.AnsweringHttpServer.answering
import tools.HttpStatusException

import java.time.{Clock, Instant, ZoneOffset}

/** A Facebook sign-in's Graph calls over real HTTP: each one's `X-App-Usage` reaches the brake on
 *  the fleet's re-scrapes, which share the app's allowance with them. */
class FacebookGraphFetchSpec extends AnyFlatSpec with Matchers {
  private val Now = Instant.parse("2026-10-15T06:00:00Z")
  private val AtCeiling = s"""{"call_count":${FacebookUsageBrake.UsageCeiling},"total_time":3,"total_cputime":2}"""

  private final class Fixture(failing: Boolean = false) {
    val held = collection.mutable.Buffer.empty[Instant]
    val fetch = new FacebookGraphFetch(new FacebookUsageBrake(new FacebookSlots {
      def holdSlots(until: Instant): Unit = if (failing) throw new IllegalStateException("fleet database down") else held += until
    }, Clock.fixed(Now, ZoneOffset.UTC)), javax.net.ssl.SSLContext.getDefault)
  }

  "A sign-in's Graph call at the ceiling" should "answer the body and hold the fleet's re-scrapes" in {
    val f = new Fixture
    answering(200, """{"id":"42"}""", Map("X-App-Usage" -> AtCeiling)) { endpoint =>
      f.fetch.get(s"${endpoint}me?fields=id") shouldBe """{"id":"42"}"""
    }
    f.held shouldBe Seq(Now.plusMillis(FacebookUsageBrake.UsageHold.toMillis))
  }

  "One under the ceiling" should "hold nothing" in {
    val f = new Fixture
    answering(200, "{}", Map("x-app-usage" -> """{"call_count":10}""")) { endpoint => f.fetch.get(endpoint.toString) }
    f.held shouldBe empty
  }

  "A refusal" should "throw its status, after reporting the usage it carries" in {
    val f = new Fixture
    answering(403, """{"error":{"code":4}}""", Map("x-app-usage" -> """{"call_count":104}""")) { endpoint =>
      (the[HttpStatusException] thrownBy f.fetch.get(endpoint.toString)).code shouldBe 403
    }
    f.held should have size 1
  }

  "A hold the fleet database cannot take" should "not fail the sign-in" in {
    val f = new Fixture(failing = true)
    answering(200, "{}", Map("x-app-usage" -> AtCeiling)) { endpoint => f.fetch.get(endpoint.toString) shouldBe "{}" }
  }
}
