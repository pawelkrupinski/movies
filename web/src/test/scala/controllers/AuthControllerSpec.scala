package controllers

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore, OauthProfile, OauthProvider}
import services.users.InMemoryUserRepository

import java.time.{Clock, Instant, ZoneOffset}

import SignedInVisitors.signedIn

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

  // ── one sign-in, two deployments ─────────────────────────────────────────

  "A sibling deployment" should "render the identity the sign-in just established, not the one it had cached" in {
    // THE BUG THIS PINS. showtimes.cc/us is its own pod; `/auth/*` is answered
    // by the process mounted at the apex. Signing in with Facebook to an account
    // last seen through Google updated Mongo and the apex pod's cache — and left
    // the pod that renders /us serving the Google name and avatar for the rest
    // of that cache's hour.
    val store   = new InMemoryUserRepository                 // Mongo, shared by both pods
    val apex    = store // each pod reads the shared store directly (see `UsersWiring.userRepository`)
    val sibling = store

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

  // ── /api/me is now the only thing that names anybody ──────────────────────
  //
  // The listing page stopped rendering the avatar so Cloudflare could hold it
  // (`CachePolicy.RevalidatedAnywhere`), and the whole per-user surface moved
  // here. Which inverts the cache requirement: the page may be stored by anyone,
  // this must be stored by no one. It used to carry no `Cache-Control` at all,
  // and a response with none is HEURISTICALLY cacheable — so a browser could have
  // replayed a 200 after a sign-out and rebuilt an avatar for a session that was
  // already gone.

  "GET /api/me" should "forbid anything keeping a copy of the answer" in {
    val (ctl, repository, _) = fixture()
    repository.upsert(models.User(
      id = "alice@example.com", provider = "google", providerSub = "G-9",
      email = Some("alice@example.com"), displayName = Some("Alice"), avatarUrl = None,
      createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    val result = ctl.me()(
      FakeRequest("GET", "/api/me").withSession("userId" -> "alice@example.com"))

    status(result) shouldBe OK
    header("Cache-Control", result).value shouldBe PerUserResponse.CacheControl
  }

  // Including the 401. It is a smaller answer but the same kind of answer — "not
  // you" is still about the asker — and a stored 401 would keep a page insisting
  // a signed-in visitor is anonymous.
  it should "say the same about a 401" in {
    val (ctl, _, _) = fixture()
    val result = ctl.me()(FakeRequest("GET", "/api/me"))

    status(result) shouldBe UNAUTHORIZED
    header("Cache-Control", result).value shouldBe PerUserResponse.CacheControl
  }

  // ── POST /auth/logout signs out EVERY session ───────────────────────────

  private def logoutAs(ctl: AuthController, session: play.api.mvc.Session) =
    ctl.logout()(FakeRequest("POST", "/auth/logout").withSession(session.data.toSeq*))

  /** A browser signed in as the account stands now. */
  private def browserSession(repository: InMemoryUserRepository, email: String) =
    SignedInUser.establish(play.api.mvc.Session(), repository.findById(email).value)

  /** The session `/auth/token` hands a native app — `Profile` is alice@example.com. */
  private def appSession(ctl: AuthController) =
    session(ctl.token()(FakeRequest("POST", "/auth/token").withBody(play.api.libs.json.Json.obj(
      "provider" -> "google", "token" -> "C", "redirectUri" -> "kinowo://auth"))))

  private def meAs(ctl: AuthController, session: play.api.mvc.Session) =
    status(ctl.me()(FakeRequest("GET", "/api/me").withSession(session.data.toSeq*)))

  // Logout is global: the laptop's cookie and the phone app's cookie (what
  // `/auth/token` hands the native apps) are separate sessions of one account,
  // and signing out of either must leave neither answering `/api/me`.
  "AuthController.logout" should "sign out every other session of the account — another browser and the app" in {
    val (ctl, repository, _) = fixture(new FakeProvider("google", Profile))
    val email   = signedIn(repository, "alice@example.com", Now)
    val laptop  = browserSession(repository, email)
    val desktop = browserSession(repository, email)
    val app     = appSession(ctl)
    Seq(laptop, desktop, app).map(meAs(ctl, _)) shouldBe Seq(OK, OK, OK)

    status(logoutAs(ctl, laptop)) shouldBe SEE_OTHER

    meAs(ctl, desktop) shouldBe UNAUTHORIZED
    meAs(ctl, app)     shouldBe UNAUTHORIZED
  }

  it should "let the app's logout sign out the browser too" in {
    val (ctl, repository, _) = fixture(new FakeProvider("google", Profile))
    val email   = signedIn(repository, "alice@example.com", Now)
    val browser = browserSession(repository, email)
    val app     = appSession(ctl)

    status(logoutAs(ctl, app)) shouldBe SEE_OTHER

    meAs(ctl, browser) shouldBe UNAUTHORIZED
  }

  it should "redirect an anonymous caller home, revoking nothing" in {
    val (ctl, repository, _) = fixture()
    val email  = signedIn(repository, "alice@example.com", Now)
    val result = ctl.logout()(FakeRequest("POST", "/auth/logout"))

    status(result) shouldBe SEE_OTHER
    redirectLocation(result) shouldBe Some("/")
    repository.findById(email).value.sessionVersion shouldBe 0
  }

  // A cookie a previous logout already killed names no one any more: replaying
  // it must not sign the account out of the sessions it has opened since.
  it should "leave a newer session alone when the cookie logging out was already revoked" in {
    val (ctl, repository, _) = fixture()
    val email = signedIn(repository, "alice@example.com", Now)
    val stale = browserSession(repository, email)
    repository.revokeSessions(email)
    val fresh = browserSession(repository, email)

    status(logoutAs(ctl, stale)) shouldBe SEE_OTHER

    meAs(ctl, fresh) shouldBe OK
  }

  // The store being unreachable can stop the other devices signing out, but
  // never this one: the cookie here is discarded all the same.
  it should "still sign this browser out when the user store cannot be read" in {
    val repository = new InMemoryUserRepository {
      override def findById(id: String): Option[models.User] = throw new RuntimeException("mongo down")
    }
    val ctl = new AuthController(Helpers.stubControllerComponents(), Map.empty, repository,
      new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk), models.Country.Poland, clock = fixedClk)
    val result = ctl.logout()(FakeRequest("POST", "/auth/logout").withSession("userId" -> "alice@example.com"))

    status(result) shouldBe SEE_OTHER
    session(result).get("userId") shouldBe empty
  }

  // A revoke landing between a sign-in's read of the row and its write: the
  // store keeps the revoke, and the cookie the sign-in hands out must carry the
  // version the row holds now, or the fresh sign-in is dead on arrival.
  "A sign-in racing a logout elsewhere" should "get a session that is valid after it" in {
    class RevokedMidSignIn extends InMemoryUserRepository {
      @volatile var revokeOnNextRead = false
      override def findById(id: String): Option[models.User] = {
        val read = super.findById(id)
        if (revokeOnNextRead) { revokeOnNextRead = false; revokeSessions(id): Unit }
        read
      }
    }
    val repository = new RevokedMidSignIn
    val ctl = new AuthController(Helpers.stubControllerComponents(), Map("google" -> new FakeProvider("google", Profile)),
      repository, new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk), models.Country.Poland, clock = fixedClk)
    signedIn(repository, "alice@example.com", Now)
    val started = session(ctl.start("google")(FakeRequest("GET", "/auth/google/start")))
    repository.revokeOnNextRead = true
    val back = ctl.callback("google")(FakeRequest("GET", s"/auth/google/callback?code=C&state=${started.get("oauthState").value}")
      .withSession(started.data.toSeq*))
    repository.findById("alice@example.com").value.sessionVersion shouldBe 1
    SignedInUser(FakeRequest().withSession(session(back).data.toSeq*), repository).map(_.id) shouldBe Some("alice@example.com")
  }

}
