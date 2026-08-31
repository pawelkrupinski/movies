package controllers

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore, OauthProfile, OauthProvider}
import services.users.InMemoryUserRepository

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
  // the test's "now"; the state-timestamp in the session is always relative to
  // this anchor.
  private val Now      = Instant.parse("2026-05-19T12:00:00Z")
  private val NowMs    = Now.toEpochMilli
  private val fixedClk = Clock.fixed(Now, ZoneOffset.UTC)

  // The store is process-local here; the RULES it is driven by — single use, the
  // two-minute window — live in `AuthExchangeCodes` above the seam, so what this
  // spec exercises is the same policy production runs and only the persistence
  // differs. Returned alongside so a test can watch a code being spent.
  private def fixture(providers: OauthProvider*): (AuthController, InMemoryUserRepository, AuthExchangeCodes) =
    fixtureFor(models.Country.Poland, providers*)

  /** As `fixture`, for a deployment serving `country` — the apex flows need a
   *  controller that is Poland (the process mounted at `/`) finishing a UK flow. */
  private def fixtureFor(country: models.Country, providers: OauthProvider*): (AuthController, InMemoryUserRepository, AuthExchangeCodes) = {
    val repository = new InMemoryUserRepository
    val codes      = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk)
    val ctl  = new AuthController(
      Helpers.stubControllerComponents(),
      providers.map(p => p.name -> p).toMap,
      repository,
      codes,
      country,
      clock = fixedClk
    )
    (ctl, repository, codes)
  }

  // ── /auth/:provider/start ─────────────────────────────────────────────────

  "AuthController.start" should "302 to the provider's authUrl and stash state + provider in session" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
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
    val (ctl, _, _) = fixture()
    status(ctl.start("google")(FakeRequest("GET", "/auth/google/start"))) shouldBe NOT_FOUND
  }

  it should "flag the session as a mobile client for platform=ios and platform=android" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    for (platform <- Seq("ios", "android")) {
      val result = ctl.start("google")(FakeRequest("GET", s"/auth/google/start?platform=$platform"))
      session(result).get("mobileClient").value shouldBe "1"
    }
  }

  it should "not flag a mobile client for a plain web start (no platform parameter)" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val result   = ctl.start("google")(FakeRequest("GET", "/auth/google/start"))
    session(result).get("mobileClient") shouldBe empty
  }

  // ── /auth/:provider/callback — happy path ────────────────────────────────

  "AuthController.callback" should "exchange code, create a new user, set userId in session, redirect to /" in {
    val provider = new FakeProvider("google", Profile)
    val (ctl, repository, _) = fixture(provider)

    val request = FakeRequest("GET", "/auth/google/callback?code=AUTH_CODE&state=THE_STATE")
      .withSession("oauthState" -> "THE_STATE", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    val result  = ctl.callback("google")(request)

    status(result)            shouldBe SEE_OTHER
    // Straight home: this request did not arrive on a deployed origin, so there
    // is no sibling domain to establish a session on. The pairing hop, and the
    // origin that decides it, are `AuthCallbackRelaySpec`'s.
    redirectLocation(result)  shouldBe Some("/")
    provider.lastExchange.value._1 shouldBe "AUTH_CODE"

    val sess = session(result)
    val userId = sess.get("userId").value
    sess.get("oauthState")    shouldBe empty   // one-shot CSRF drops after use
    sess.get("oauthProvider") shouldBe empty

    val stored = repository.findById(userId).value
    stored.provider    shouldBe "google"
    stored.providerSub shouldBe "G-1"
    stored.email       shouldBe Some("alice@example.com")
    stored.displayName shouldBe Some("Alice")
  }

  it should "update the existing user (not duplicate) when (provider, sub) is already known" in {
    val provider = new FakeProvider("google", Profile)
    val (ctl, repository, _) = fixture(provider)

    // First login — creates the user.
    val firstSession = session(ctl.callback("google")(
      FakeRequest("GET", "/auth/google/callback?code=C1&state=S1")
        .withSession("oauthState" -> "S1", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    ))
    val firstUserId = firstSession.get("userId").value

    // Second login with a different display name from the provider — the
    // upsert should refresh the row, not create a new one.
    val updatedProfile = Profile.copy(displayName = Some("Alice (married)"))
    val provider2     = new FakeProvider("google", updatedProfile)
    val (ctl2, repository2) = (
      new AuthController(Helpers.stubControllerComponents(), Map("google" -> provider2), repository,
        new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk), models.Country.Poland, clock = fixedClk),
      repository
    )
    val secondSession = session(ctl2.callback("google")(
      FakeRequest("GET", "/auth/google/callback?code=C2&state=S2")
        .withSession("oauthState" -> "S2", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    ))
    secondSession.get("userId").value shouldBe firstUserId   // same id, not a fresh signup
    repository2.findById(firstUserId).value.displayName shouldBe Some("Alice (married)")
  }

  it should "bounce a mobile client back to the kinowo:// deep link with a one-shot code" in {
    val provider = new FakeProvider("google", Profile)
    val (ctl, repository, codes) = fixture(provider)

    val request = FakeRequest("GET", "/auth/google/callback?code=AUTH_CODE&state=THE_STATE")
      .withSession(
        "oauthState" -> "THE_STATE", "oauthProvider" -> "google",
        "oauthStateTimestamp" -> NowMs.toString, "mobileClient" -> "1"
      )
    val result = ctl.callback("google")(request)

    status(result) shouldBe SEE_OTHER
    val location = redirectLocation(result).value
    location should startWith ("kinowo://auth-done?code=")

    // The code is single-use and redeems to the just-created user.
    val code = location.stripPrefix("kinowo://auth-done?code=")
    val userId = session(result).get("userId").value
    codes.redeem(code).value shouldBe userId
    // Single use: the app gets one shot at it, so a code lifted off the deep
    // link afterwards is worth nothing.
    codes.redeem(code) shouldBe empty
    repository.findById(userId).value.email shouldBe Some("alice@example.com")

    // The mobile flag is consumed so it can't leak into a later web session.
    session(result).get("mobileClient") shouldBe empty
  }

  // ── /auth/:provider/callback — sad paths ─────────────────────────────────

  it should "reject the callback when state doesn't match the session" in {
    val (ctl, repository, _) = fixture(new FakeProvider("google", Profile))
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=ATTACKER_GUESS")
      .withSession("oauthState" -> "THE_REAL_ONE", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    val result  = ctl.callback("google")(request)

    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("state mismatch")
    repository.findById("anything") shouldBe empty
  }

  it should "reject the callback when session has no state at all (no prior /start)" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val result   = ctl.callback("google")(FakeRequest("GET", "/auth/google/callback?code=C&state=S"))
    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("Missing session state")
  }

  it should "reject the callback when session-stored provider doesn't match the callback path" in {
    // Attacker shows /auth/google/start (gets google session state), then
    // tries to feed it into /auth/facebook/callback. The provider mismatch
    // check blocks it.
    val (ctl, _, _) = fixture(
      new FakeProvider("google",   Profile),
      new FakeProvider("facebook", Profile.copy(sub = "FB-1"))
    )
    val request = FakeRequest("GET", "/auth/facebook/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
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
    val (ctl, repository, _) = fixture(brokenProvider)
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    val result  = ctl.callback("google")(request)

    status(result) shouldBe INTERNAL_SERVER_ERROR
    repository.findById("anything") shouldBe empty   // nothing persisted
  }

  // ── /auth/logout ─────────────────────────────────────────────────────────

  "AuthController.logout" should "drop userId from the session and redirect to /" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val request = FakeRequest("POST", "/auth/logout")
      .withSession("userId" -> "alice", "oauthState" -> "leftover", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    val result = ctl.logout()(request)

    status(result)              shouldBe SEE_OTHER
    // Straight home, for the same reason as the callback above.
    redirectLocation(result)    shouldBe Some("/")
    val sess = session(result)
    sess.get("userId")          shouldBe empty
    sess.get("oauthState")      shouldBe empty   // any leftover cleared too
    sess.get("oauthProvider")   shouldBe empty
    sess.get("oauthStateTimestamp")    shouldBe empty
  }

  // ── /auth/:provider/start — TTL plumbing ────────────────────────────────

  "AuthController.start" should "stamp oauthStateTimestamp in the session" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val result   = ctl.start("google")(FakeRequest("GET", "/auth/google/start"))
    session(result).get("oauthStateTimestamp").value.toLong shouldBe NowMs
  }

  // ── State TTL on callback ───────────────────────────────────────────────

  "AuthController.callback" should "reject when oauthStateTimestamp is missing (legacy session, pre-TTL deploy)" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google")   // no oauthStateTimestamp
    status(ctl.callback("google")(request))      shouldBe BAD_REQUEST
    contentAsString(ctl.callback("google")(request)) should include ("oauthStateTimestamp")
  }

  it should "reject when oauthStateTimestamp is older than 10 minutes" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val tenMinAndChange = NowMs - (11 * 60 * 1000).toLong
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTimestamp" -> tenMinAndChange.toString)
    val result = ctl.callback("google")(request)
    status(result) shouldBe BAD_REQUEST
    contentAsString(result) should include ("expired")
  }

  it should "accept when oauthStateTimestamp is just under 10 minutes old" in {
    val (ctl, _, _) = fixture(new FakeProvider("google", Profile))
    val fresh = NowMs - (9 * 60 * 1000).toLong
    val request = FakeRequest("GET", "/auth/google/callback?code=C&state=S")
      .withSession("oauthState" -> "S", "oauthProvider" -> "google", "oauthStateTimestamp" -> fresh.toString)
    status(ctl.callback("google")(request)) shouldBe SEE_OTHER
  }

  // ── Account linking by email ────────────────────────────────────────────

  "AuthController.upsertUser" should "merge providers sharing the same email into one user" in {
    val googleProvider = new FakeProvider("google",   Profile)
    val fbProfile      = OauthProfile(sub = "FB-99", email = Some("alice@example.com"), displayName = Some("Alice on FB"), avatarUrl = None)
    val fbProvider     = new FakeProvider("facebook", fbProfile)
    val (ctl, repository, _)    = fixture(googleProvider, fbProvider)

    val googleSession = session(ctl.callback("google")(
      FakeRequest("GET", "/auth/google/callback?code=C1&state=S1")
        .withSession("oauthState" -> "S1", "oauthProvider" -> "google", "oauthStateTimestamp" -> NowMs.toString)
    ))
    val firstUserId = googleSession.get("userId").value
    firstUserId shouldBe "alice@example.com"

    val fbSession = session(ctl.callback("facebook")(
      FakeRequest("GET", "/auth/facebook/callback?code=C2&state=S2")
        .withSession("oauthState" -> "S2", "oauthProvider" -> "facebook", "oauthStateTimestamp" -> NowMs.toString)
    ))
    fbSession.get("userId").value shouldBe firstUserId

    val linked = repository.findById(firstUserId).value
    linked.provider    shouldBe "facebook"
    linked.providerSub shouldBe "FB-99"
    linked.email       shouldBe Some("alice@example.com")
  }

  it should "reject a provider that returns no email" in {
    val (ctl, _, _) = fixture(
      new FakeProvider("facebook", OauthProfile(sub = "FB-99", email = None, None, None))
    )

    val result = ctl.callback("facebook")(
      FakeRequest("GET", "/auth/facebook/callback?code=C1&state=S1")
        .withSession("oauthState" -> "S1", "oauthProvider" -> "facebook", "oauthStateTimestamp" -> NowMs.toString)
    )
    status(result) shouldBe INTERNAL_SERVER_ERROR
  }


  // ── /auth/sso/start + /auth/sso/finish ───────────────────────────────────
  //
  // The country switch across a DOMAIN boundary. `/uk`, `/de` and `/us` share
  // one origin and so share the session cookie outright; kinowo.net does not,
  // and no cookie setting can make it. These two endpoints are how a live
  // session crosses that gap: mint a one-shot code on the side that has it,
  // spend it on the side that does not.

  private val UkBase = models.Country.UnitedKingdom.webUrl.value
  private val PlBase = models.Country.Poland.webUrl.value


  private def signedIn(repository: InMemoryUserRepository, email: String): String = {
    repository.upsert(models.User(
      id          = email,
      provider    = "google",
      providerSub = s"sub-$email",
      email       = Some(email),
      displayName = Some("Alice"),
      avatarUrl   = None,
      createdAt   = Now,
      lastSeenAt  = Now))
    email
  }

  "AuthController.switchTarget" should "accept every deployed country's own base URL" in {
    models.Country.switchable.flatMap(_.webUrl).foreach { base =>
      AuthController.switchTarget(Some(base)).value shouldBe base
    }
  }

  it should "tolerate a trailing slash, since the switcher's values are base URLs" in {
    AuthController.switchTarget(Some(s"$UkBase/")).value shouldBe UkBase
  }

  // This redirect carries a live sign-in code, so the target is an allowlist of
  // the addresses we actually deploy — not a shape test that some look-alike
  // host could satisfy.
  it should "refuse anything that is not one of them" in {
    AuthController.switchTarget(Some("https://evil.example.com"))     shouldBe empty
    AuthController.switchTarget(Some("https://showtimes.cc.evil.com")) shouldBe empty
    AuthController.switchTarget(Some("https://showtimes.cc/../uk"))    shouldBe empty
    AuthController.switchTarget(Some("//evil.example.com"))            shouldBe empty
    AuthController.switchTarget(Some(""))                              shouldBe empty
    AuthController.switchTarget(None)                                  shouldBe empty
  }

  "AuthController.ssoStart" should "hand a signed-in visitor over with a one-shot code" in {
    val (ctl, repository, codes) = fixture()
    val userId = signedIn(repository, "alice@example.com")

    val result = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase").withSession("userId" -> userId))

    status(result) shouldBe SEE_OTHER
    val location = redirectLocation(result).value
    location should startWith (s"$UkBase/auth/sso/finish?code=")
    codes.redeem(location.stripPrefix(s"$UkBase/auth/sso/finish?code=")).value shouldBe userId
  }

  // Nothing to hand over is not a failure — it is the plain link the switcher
  // would have followed anyway, and the visitor still lands where they asked.
  it should "send a signed-out visitor straight to the other country" in {
    val (ctl, _, _) = fixture()

    val result = ctl.ssoStart()(FakeRequest("GET", s"/auth/sso/start?to=$UkBase"))

    status(result) shouldBe SEE_OTHER
    redirectLocation(result).value shouldBe s"$UkBase/"
  }

  it should "mint nothing for a visitor whose session names a user that no longer exists" in {
    val (ctl, _, _) = fixture()

    val result = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase").withSession("userId" -> "deleted@example.com"))

    redirectLocation(result).value shouldBe s"$UkBase/"
  }

  it should "refuse a target that is not a deployed country" in {
    val (ctl, repository, _) = fixture()
    val userId = signedIn(repository, "alice@example.com")

    val result = ctl.ssoStart()(
      FakeRequest("GET", "/auth/sso/start?to=https://evil.example.com").withSession("userId" -> userId))

    status(result) shouldBe BAD_REQUEST
  }

  "AuthController.ssoFinish" should "sign the visitor in and land them on this country's home" in {
    val (ctl, repository, codes) = fixture()
    val userId = signedIn(repository, "alice@example.com")

    val result = ctl.ssoFinish()(FakeRequest("GET", s"/auth/sso/finish?code=${codes.mint(userId)}"))

    status(result) shouldBe SEE_OTHER
    session(result).get("userId").value shouldBe userId
  }

  // By the time they are here they have already left the page they came from,
  // so an error page would offer them nothing they can act on. The home page
  // has a sign-in button.
  it should "land a stale or missing code on the home page, signed out" in {
    val (ctl, _, _) = fixture()

    val noCode = ctl.ssoFinish()(FakeRequest("GET", "/auth/sso/finish"))
    status(noCode) shouldBe SEE_OTHER
    session(noCode).get("userId") shouldBe empty

    val badCode = ctl.ssoFinish()(FakeRequest("GET", "/auth/sso/finish?code=never-minted"))
    status(badCode) shouldBe SEE_OTHER
    session(badCode).get("userId") shouldBe empty
  }

  // THE WHOLE POINT, end to end: two controllers standing in for the two pods
  // either side of the domain boundary. They share a user repository and a code
  // store — which is what `MONGODB_USERS_DB` buys — and nothing else, no cookie
  // among them. An in-process code cache would fail exactly here, and used to.
  "A visitor switching country across the domain boundary" should "arrive signed in" in {
    val repository = new InMemoryUserRepository
    val store      = new InMemoryAuthExchangeCodeStore
    def pod(): AuthController = new AuthController(
      Helpers.stubControllerComponents(), Map.empty, repository,
      new AuthExchangeCodes(store, fixedClk), models.Country.Poland, clock = fixedClk)

    val poland = pod()
    val uk     = pod()
    val userId = signedIn(repository, "alice@example.com")

    val handoff  = poland.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase").withSession("userId" -> userId))
    val code     = redirectLocation(handoff).value.stripPrefix(s"$UkBase/auth/sso/finish?code=")
    val arrival  = uk.ssoFinish()(FakeRequest("GET", s"/auth/sso/finish?code=$code"))

    session(arrival).get("userId").value shouldBe userId
  }

  it should "not be able to do it twice with the same code" in {
    val repository = new InMemoryUserRepository
    val store      = new InMemoryAuthExchangeCodeStore
    def pod(): AuthController = new AuthController(
      Helpers.stubControllerComponents(), Map.empty, repository,
      new AuthExchangeCodes(store, fixedClk), models.Country.Poland, clock = fixedClk)

    val userId  = signedIn(repository, "alice@example.com")
    val handoff = pod().ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$PlBase").withSession("userId" -> userId))
    val code    = redirectLocation(handoff).value.stripPrefix(s"$PlBase/auth/sso/finish?code=")

    session(pod().ssoFinish()(FakeRequest("GET", s"/auth/sso/finish?code=$code"))).get("userId").value shouldBe userId
    // A replayed link — shoulder-surfed, or sitting in someone's history — is
    // worth nothing.
    session(pod().ssoFinish()(FakeRequest("GET", s"/auth/sso/finish?code=$code"))).get("userId") shouldBe empty
  }

  // ── one sign-in, two deployments ─────────────────────────────────────────

  "A sibling deployment" should "render the identity the sign-in just established, not the one it had cached" in {
    // THE BUG THIS PINS. showtimes.cc/us is its own pod; `/auth/*` is answered
    // by the process mounted at the apex. Signing in with Facebook to an account
    // last seen through Google updated Mongo and the apex pod's cache — and left
    // the pod that renders /us serving the Google name and avatar for the rest
    // of that cache's hour.
    val store   = new InMemoryUserRepository                 // Mongo, shared by both pods
    val apex    = new services.users.CachingUserRepository(store)
    val sibling = new services.users.CachingUserRepository(store)

    def pod(users: services.users.UserRepository, provider: OauthProvider*): AuthController =
      new AuthController(
        Helpers.stubControllerComponents(), provider.map(p => p.name -> p).toMap, users,
        new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk), models.Country.Poland,
        clock = fixedClk)

    store.upsert(models.User(
      id          = "alice@example.com",
      provider    = "google",
      providerSub = "G-1",
      email       = Some("alice@example.com"),
      displayName = Some("Alice"),
      avatarUrl   = Some("https://lh3/avatar"),
      createdAt   = Now.minusSeconds(86400),
      lastSeenAt  = Now.minusSeconds(3600)
    ))
    // Alice browsing /us while signed in through Google — this is what warms the
    // sibling pod's cache with the row the sign-in is about to replace.
    sibling.findById("alice@example.com").value.provider shouldBe "google"

    val facebook = new FakeProvider("facebook", OauthProfile(
      sub = "FB-1", email = Some("alice@example.com"),
      displayName = Some("Alice K"), avatarUrl = Some("https://platform-lookaside.fbsbx.com/alice")))
    val signIn = pod(apex, facebook).callback("facebook")(
      FakeRequest("GET", "/auth/facebook/callback?code=C&state=S")
        .withSession("oauthState" -> "S", "oauthProvider" -> "facebook", "oauthStateTimestamp" -> NowMs.toString))
    status(signIn) shouldBe SEE_OTHER

    // The next page is rendered by the OTHER pod, off the session the apex just
    // issued — the cookie is all they share.
    val onSibling = pod(sibling).me()(FakeRequest("GET", "/auth/me").withSession(session(signIn).data.toSeq*))

    status(onSibling) shouldBe OK
    (contentAsJson(onSibling) \ "provider").as[String]  shouldBe "facebook"
    (contentAsJson(onSibling) \ "avatarUrl").as[String] shouldBe "https://platform-lookaside.fbsbx.com/alice"
  }

}
