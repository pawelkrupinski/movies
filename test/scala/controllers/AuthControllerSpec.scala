package controllers

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{OauthProfile, OauthProvider}
import services.users.InMemoryUserRepo

import java.time.{Clock, Instant, ZoneOffset}

class AuthControllerSpec extends AnyFlatSpec with Matchers {

  // Hand-rolled fake — `authUrl` returns a deterministic redirect URL;
  // `exchangeCode` returns the canned profile. The real OAuth providers
  // get their own specs (Google/FacebookOauthProviderSpec); this spec is
  // about the controller's session / CSRF / redirect plumbing.
  private class FakeProvider(val name: String, profile: OauthProfile) extends OauthProvider {
    var lastExchange: Option[(String, String)] = None
    def authUrl(state: String, redirectUri: String): String =
      s"https://$name.test/authorize?state=$state&redirect=$redirectUri"
    def exchangeCode(code: String, redirectUri: String): OauthProfile = {
      lastExchange = Some((code, redirectUri))
      profile
    }
  }

  private val Profile = OauthProfile(
    sub         = "G-1",
    email       = Some("alice@example.com"),
    displayName = Some("Alice"),
    avatarUrl   = Some("https://lh3/avatar")
  )

  // Fixed clock so spec assertions don't depend on wall clock. `Now` is
  // the test's "now"; the state-ts in the session is always relative to
  // this anchor.
  private val Now      = Instant.parse("2026-05-19T12:00:00Z")
  private val NowMs    = Now.toEpochMilli
  private val fixedClk = Clock.fixed(Now, ZoneOffset.UTC)

  private def fixture(providers: OauthProvider*): (AuthController, InMemoryUserRepo) = {
    val repo = new InMemoryUserRepo
    val ctl  = new AuthController(
      Helpers.stubControllerComponents(),
      providers.map(p => p.name -> p).toMap,
      repo,
      clock = fixedClk
    )
    (ctl, repo)
  }

  // ── /auth/:provider/start ─────────────────────────────────────────────────

  "AuthController.start" should "302 to the provider's authUrl and stash state + provider in session" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val result   = ctl.start("google")(FakeRequest("GET", "/auth/google/start"))

    status(result) shouldBe SEE_OTHER
    val location = redirectLocation(result).value
    location should startWith ("https://google.test/authorize?state=")
    location should include ("redirect=http://")  // Helpers stub has no TLS

    val sess = session(result)
    sess.get("oauthState").value      should not be empty
    sess.get("oauthProvider").value   shouldBe "google"
  }

  it should "404 when the provider isn't wired (env var missing → not in the map)" in {
    val (ctl, _) = fixture()
    status(ctl.start("google")(FakeRequest("GET", "/auth/google/start"))) shouldBe NOT_FOUND
  }

  // ── /auth/:provider/callback — happy path ────────────────────────────────

  "AuthController.callback" should "exchange code, create a new user, set userId in session, redirect to /" in {
    val provider = new FakeProvider("google", Profile)
    val (ctl, repo) = fixture(provider)

    val request = FakeRequest("GET", "/auth/google/callback?code=AUTH_CODE&state=THE_STATE")
      .withSession("oauthState" -> "THE_STATE", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    val result  = ctl.callback("google")(request)

    status(result)            shouldBe SEE_OTHER
    redirectLocation(result)  shouldBe Some("/")
    provider.lastExchange.value._1 shouldBe "AUTH_CODE"

    val sess = session(result)
    val userId = sess.get("userId").value
    sess.get("oauthState")    shouldBe empty   // one-shot CSRF drops after use
    sess.get("oauthProvider") shouldBe empty

    val stored = repo.findById(userId).value
    stored.provider    shouldBe "google"
    stored.providerSub shouldBe "G-1"
    stored.email       shouldBe Some("alice@example.com")
    stored.displayName shouldBe Some("Alice")
  }

  it should "update the existing user (not duplicate) when (provider, sub) is already known" in {
    val provider = new FakeProvider("google", Profile)
    val (ctl, repo) = fixture(provider)

    // First login — creates the user.
    val firstSession = session(ctl.callback("google")(
      FakeRequest("GET", "/auth/google/callback?code=C1&state=S1")
        .withSession("oauthState" -> "S1", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    ))
    val firstUserId = firstSession.get("userId").value

    // Second login with a different display name from the provider — the
    // upsert should refresh the row, not create a new one.
    val updatedProfile = Profile.copy(displayName = Some("Alice (married)"))
    val provider2     = new FakeProvider("google", updatedProfile)
    val (ctl2, repo2) = (
      new AuthController(Helpers.stubControllerComponents(), Map("google" -> provider2), repo, clock = fixedClk),
      repo
    )
    val secondSession = session(ctl2.callback("google")(
      FakeRequest("GET", "/auth/google/callback?code=C2&state=S2")
        .withSession("oauthState" -> "S2", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    ))
    secondSession.get("userId").value shouldBe firstUserId   // same id, not a fresh signup
    repo2.findById(firstUserId).value.displayName shouldBe Some("Alice (married)")
  }

  // ── /auth/:provider/callback — sad paths ─────────────────────────────────

  it should "reject the callback when state doesn't match the session" in {
    val (ctl, repo) = fixture(new FakeProvider("google", Profile))
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=ATTACKER_GUESS")
      .withSession("oauthState" -> "THE_REAL_ONE", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    val result  = ctl.callback("google")(request)

    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("state mismatch")
    repo.findById("anything") shouldBe empty
  }

  it should "reject the callback when session has no state at all (no prior /start)" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val result   = ctl.callback("google")(FakeRequest("GET", "/auth/google/callback?code=C&state=S"))
    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("Missing session state")
  }

  it should "reject the callback when session-stored provider doesn't match the callback path" in {
    // Attacker shows /auth/google/start (gets google session state), then
    // tries to feed it into /auth/facebook/callback. The provider mismatch
    // check blocks it.
    val (ctl, _) = fixture(
      new FakeProvider("google",   Profile),
      new FakeProvider("facebook", Profile.copy(sub = "FB-1"))
    )
    val request = FakeRequest("GET", "/auth/facebook/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    val result  = ctl.callback("facebook")(request)

    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("Provider mismatch")
  }

  it should "500 when the provider's code exchange throws (network / parse failure)" in {
    val brokenProvider = new OauthProvider {
      def name = "google"
      def authUrl(s: String, r: String) = "https://x"
      def exchangeCode(c: String, r: String) = throw new RuntimeException("upstream blew up")
    }
    val (ctl, repo) = fixture(brokenProvider)
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    val result  = ctl.callback("google")(request)

    status(result) shouldBe INTERNAL_SERVER_ERROR
    repo.findById("anything") shouldBe empty   // nothing persisted
  }

  // ── /auth/logout ─────────────────────────────────────────────────────────

  "AuthController.logout" should "drop userId from the session and redirect to /" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val request = FakeRequest("POST", "/auth/logout")
      .withSession("userId" -> "alice", "oauthState" -> "leftover", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    val result = ctl.logout()(request)

    status(result)              shouldBe SEE_OTHER
    redirectLocation(result)    shouldBe Some("/")
    val sess = session(result)
    sess.get("userId")          shouldBe empty
    sess.get("oauthState")      shouldBe empty   // any leftover cleared too
    sess.get("oauthProvider")   shouldBe empty
    sess.get("oauthStateTs")    shouldBe empty
  }

  // ── /auth/:provider/start — TTL plumbing ────────────────────────────────

  "AuthController.start" should "stamp oauthStateTs in the session" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val result   = ctl.start("google")(FakeRequest("GET", "/auth/google/start"))
    session(result).get("oauthStateTs").value.toLong shouldBe NowMs
  }

  // ── State TTL on callback ───────────────────────────────────────────────

  "AuthController.callback" should "reject when oauthStateTs is missing (legacy session, pre-TTL deploy)" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google")   // no oauthStateTs
    status(ctl.callback("google")(request))      shouldBe BAD_REQUEST
    contentAsString(ctl.callback("google")(request)) should include ("oauthStateTs")
  }

  it should "reject when oauthStateTs is older than 10 minutes" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val tenMinAndChange = NowMs - (11 * 60 * 1000).toLong
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTs" -> tenMinAndChange.toString)
    val result = ctl.callback("google")(request)
    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("expired")
  }

  it should "accept when oauthStateTs is just under 10 minutes old" in {
    val (ctl, _) = fixture(new FakeProvider("google", Profile))
    val fresh = NowMs - (9 * 60 * 1000).toLong
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTs" -> fresh.toString)
    status(ctl.callback("google")(request)) shouldBe SEE_OTHER
  }

  // ── Account linking by email ────────────────────────────────────────────

  "AuthController.upsertUser" should "merge providers sharing the same email into one user" in {
    val googleProvider = new FakeProvider("google",   Profile)
    val fbProfile      = OauthProfile(sub = "FB-99", email = Some("alice@example.com"), displayName = Some("Alice on FB"), avatarUrl = None)
    val fbProvider     = new FakeProvider("facebook", fbProfile)
    val (ctl, repo)    = fixture(googleProvider, fbProvider)

    val googleSession = session(ctl.callback("google")(
      FakeRequest("GET", "/auth/google/callback?code=C1&state=S1")
        .withSession("oauthState" -> "S1", "oauthProvider" -> "google", "oauthStateTs" -> NowMs.toString)
    ))
    val firstUserId = googleSession.get("userId").value
    firstUserId shouldBe "alice@example.com"

    val fbSession = session(ctl.callback("facebook")(
      FakeRequest("GET", "/auth/facebook/callback?code=C2&state=S2")
        .withSession("oauthState" -> "S2", "oauthProvider" -> "facebook", "oauthStateTs" -> NowMs.toString)
    ))
    fbSession.get("userId").value shouldBe firstUserId

    val linked = repo.findById(firstUserId).value
    linked.provider    shouldBe "facebook"
    linked.providerSub shouldBe "FB-99"
    linked.email       shouldBe Some("alice@example.com")
  }

  it should "reject a provider that returns no email" in {
    val (ctl, _) = fixture(
      new FakeProvider("facebook", OauthProfile(sub = "FB-99", email = None, None, None))
    )

    val result = ctl.callback("facebook")(
      FakeRequest("GET", "/auth/facebook/callback?code=C1&state=S1")
        .withSession("oauthState" -> "S1", "oauthProvider" -> "facebook", "oauthStateTs" -> NowMs.toString)
    )
    status(result) shouldBe INTERNAL_SERVER_ERROR
  }

}
