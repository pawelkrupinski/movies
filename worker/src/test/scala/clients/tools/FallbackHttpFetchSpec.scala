package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, HttpFetch, RoutingHttpFetch}

import scala.collection.mutable

class FallbackHttpFetchSpec extends AnyFlatSpec with Matchers {

  // Backends that either answer a stub body or throw a named exception, so each
  // test arranges the failure shape it cares about without mocking HTTP at all.
  private def ok(body: String): HttpFetch = new ConstantHttpFetch(body)
  // `boom` not `fail` — `Assertions.fail` is inherited by every ScalaTest
  // suite, so a `private def fail` here would clash with the override.
  private def boom(message: String): HttpFetch = new FailingHttpFetch((_, _) => new RuntimeException(message))

  "FallbackHttpFetch" should "return the first backend's body when it succeeds" in {
    val primary   = new RequestLogHttpFetch(ok("primary-body"))
    val secondary = new RequestLogHttpFetch(ok("secondary-body"))
    val chain = new FallbackHttpFetch(Seq("primary" -> primary, "secondary" -> secondary), _root_.tools.SpecStopwatch.Frozen)
    chain.get("https://example") shouldBe "primary-body"
    primary.gets shouldBe Seq("https://example")
    secondary.calls shouldBe empty  // secondary never consulted
  }

  it should "roll over to the next backend when the first throws" in {
    val chain = new FallbackHttpFetch(Seq("primary" -> boom("zyte boom"), "secondary" -> ok("from-secondary")), _root_.tools.SpecStopwatch.Frozen)
    chain.get("https://example") shouldBe "from-secondary"
  }

  it should "keep rolling through every failing backend until one succeeds" in {
    val chain = new FallbackHttpFetch(Seq(
      "zyte"   -> boom("zyte 5xx"),
      "direct" -> ok("direct-finally-worked")
    ), _root_.tools.SpecStopwatch.Frozen)
    chain.get("https://example") shouldBe "direct-finally-worked"
  }

  it should "throw with all named failures aggregated when every backend fails" in {
    val chain = new FallbackHttpFetch(Seq(
      "zyte"   -> boom("z-failure"),
      "direct" -> boom("d-failure")
    ), _root_.tools.SpecStopwatch.Frozen)
    val exception = intercept[RuntimeException](chain.get("https://example"))
    exception.getMessage should include ("All 2 backends failed")
    exception.getMessage should include ("zyte:")
    exception.getMessage should include ("z-failure")
    exception.getMessage should include ("direct:")
    exception.getMessage should include ("d-failure")
  }

  // A 429 wrapped in the composite RuntimeException was invisible to ThrottledHttpFetch, which
  // sits outside the chain and keys on the typed status: the recording's TMDB fill never paused
  // or retried and drew ~6,000 429s a minute. A rate limit from any leg is the chain's answer.
  it should "propagate a leg's 429 as the typed status, Retry-After intact, when every backend fails" in {
    val limited = new tools.HttpStatusException(429, "GET", "https://example", Some(scala.concurrent.duration.Duration(7, "s")))
    val chain = new FallbackHttpFetch(Seq(
      "direct" -> new FailingHttpFetch((_, _) => limited),
      "proxy"  -> boom("p-failure")
    ), _root_.tools.SpecStopwatch.Frozen)
    val exception = intercept[tools.HttpStatusException](chain.get("https://example"))
    exception.code shouldBe 429
    exception.retryAfter shouldBe Some(scala.concurrent.duration.Duration(7, "s"))
    exception.getSuppressed.map(_.getMessage).mkString should include ("p-failure")
  }

  it should "propagate a 503 carrying Retry-After as the rate limit it is, but not a bare 503" in {
    val asked = new tools.HttpStatusException(503, "GET", "https://example", Some(scala.concurrent.duration.Duration(3, "s")))
    val bare  = new tools.HttpStatusException(503, "GET", "https://example", None)
    def chainOf(failure: Throwable) = new FallbackHttpFetch(Seq(
      "direct" -> new FailingHttpFetch((_, _) => failure),
      "proxy"  -> boom("p-failure")
    ), _root_.tools.SpecStopwatch.Frozen)
    intercept[tools.HttpStatusException](chainOf(asked).get("https://example")).code shouldBe 503
    intercept[RuntimeException](chainOf(bare).get("https://example")) should not be a [tools.HttpStatusException]
  }

  // A residential-proxy exit IP that Cloudflare challenges answers 200 with the interstitial. The
  // chain took that as the proxy leg SERVING, handed the challenge to the scraper (which failed
  // the read) and never tried Zyte — the leg that exists to clear exactly that block — while
  // /uptime booked the proxy as healthy.
  it should "roll over to the next backend when one answers a challenge page with a 2xx" in {
    val challenge = "<html><head><title>Just a moment...</title></head><script>window._cf_chl_opt={}</script></html>"
    val outcomes  = mutable.ListBuffer.empty[(String, Option[String])]
    val chain = new FallbackHttpFetch(Seq("proxy" -> ok(challenge), "fallback" -> ok("<html>listing</html>")), _root_.tools.SpecStopwatch.Frozen,
      onOutcome = (name, error) => outcomes += ((name, error)))
    chain.get("https://cinema.example/listing") shouldBe "<html>listing</html>"
    chain.getBytes("https://cinema.example/listing").toSeq shouldBe "<html>listing</html>".getBytes("UTF-8").toSeq
    outcomes.collect { case ("proxy", error) => error.isDefined }.toSet shouldBe Set(true)
  }

  it should "fail, not answer, when every backend answers a challenge page" in {
    val challenge = "<title>Just a moment...</title>"
    val chain = new FallbackHttpFetch(Seq("proxy" -> ok(challenge), "fallback" -> ok(challenge)), _root_.tools.SpecStopwatch.Frozen)
    val exception = intercept[RuntimeException](chain.get("https://cinema.example/listing"))
    exception.getMessage should include ("Cloudflare challenge")
  }

  // Imperva injects its `_Incapsula_Resource?SWJIYLWA=…` script into every page it protects: a
  // healthy listing carrying it is the proxy SERVING, not a failed leg skipped for Zyte.
  it should "take a protected site's real page carrying Incapsula's injected script as the leg serving" in {
    val page = "<html><head><script type=\"text/javascript\" src=\"/_Incapsula_Resource?SWJIYLWA=719d34d31c8e3a6e6fffd425f7e032f3&ns=1\" async></script></head>" +
      "<body>" + ("<div class=\"film\">Diuna</div>" * 300) + "</body></html>"
    val outcomes = mutable.ListBuffer.empty[(String, Option[String])]
    val chain = new FallbackHttpFetch(Seq("proxy" -> ok(page), "fallback" -> boom("must not be tried")), _root_.tools.SpecStopwatch.Frozen,
      onOutcome = (name, error) => outcomes += ((name, error)))
    chain.get("https://cinema.example/listing") shouldBe page
    outcomes.toSeq shouldBe Seq("proxy" -> None)
  }

  it should "exercise the same fallback for post as for get" in {
    val secondary = new RoutingHttpFetch(Seq("https://x" -> "post-body"))
    val chain = new FallbackHttpFetch(Seq("primary" -> boom("503"), "secondary" -> secondary), _root_.tools.SpecStopwatch.Frozen)
    chain.post("https://x", "payload", "text/plain") shouldBe "post-body"
    secondary.postBodies shouldBe Seq(("https://x", "payload", "text/plain"))
  }

  it should "refuse to construct with an empty backend list — wiring bug, not runtime fallback" in {
    intercept[IllegalArgumentException] { new FallbackHttpFetch(Seq.empty, _root_.tools.SpecStopwatch.Frozen) }
  }

  // ── onOutcome metering (powers the /uptime "Residential proxy" row) ─────────

  it should "report a failed primary then the served fallback (proxy failed → Zyte used)" in {
    val outcomes = mutable.ListBuffer.empty[(String, Option[String])]
    val chain = new FallbackHttpFetch(
      Seq("proxy" -> boom("HTTP 407"), "fallback" -> ok("served")), _root_.tools.SpecStopwatch.Frozen,
      onOutcome = (name, error) => outcomes += (name -> error))
    chain.get("https://x") shouldBe "served"
    outcomes.toList shouldBe List(
      "proxy"    -> Some("proxy: RuntimeException: HTTP 407"),
      "fallback" -> None)
  }

  it should "report only the primary when it succeeds (proxy served, fallback untouched)" in {
    val outcomes = mutable.ListBuffer.empty[(String, Option[String])]
    val chain = new FallbackHttpFetch(
      Seq("proxy" -> ok("served"), "fallback" -> boom("unused")), _root_.tools.SpecStopwatch.Frozen,
      onOutcome = (name, error) => outcomes += (name -> error))
    chain.get("https://x") shouldBe "served"
    outcomes.toList shouldBe List("proxy" -> None)
  }

  it should "not let an onOutcome that throws break the fetch" in {
    val chain = new FallbackHttpFetch(
      Seq("proxy" -> ok("served")), _root_.tools.SpecStopwatch.Frozen,
      onOutcome = (_, _) => throw new RuntimeException("listener boom"))
    chain.get("https://x") shouldBe "served"
  }
}
