package controllers

import models.Country
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore, OauthProfile, OauthProvider}
import services.users.InMemoryUserRepository

import java.time.{Clock, Instant, ZoneOffset}

/**
 * ONE REGISTERED CALLBACK URL PER PROVIDER, for the whole project.
 *
 * Google and Facebook match `redirect_uri` byte-for-byte against a list kept in
 * their console, so the obvious shape — each deployment naming its own address —
 * costs an entry per country per provider and makes "remember to add two URLs,
 * in two consoles" an unwritten step of launching a country. Nothing fails until
 * a real person tries to sign in on the new site.
 *
 * So every country sends the provider to the apex, and the deployment mounted
 * there either finishes the flow or hands it on. Which of the two is the only
 * interesting decision in this file, and it turns on one question: is the
 * browser in front of us still sending the cookie that holds this flow's CSRF
 * state? That is what keeps the check meaningful — the alternative, a `state` we
 * sign ourselves and trust anywhere, would prove we issued it and not that this
 * browser did.
 */
class AuthCallbackRelaySpec extends AnyFlatSpec with Matchers {

  private val Now      = Instant.parse("2026-08-30T12:00:00Z")
  private val NowMs    = Now.toEpochMilli
  private val fixedClk = Clock.fixed(Now, ZoneOffset.UTC)

  private val Profile = OauthProfile(
    sub = "G-1", email = Some("alice@example.com"),
    displayName = Some("Alice"), avatarUrl = None)

  private class FakeProvider(val name: String) extends OauthProvider {
    var lastRedirectUri: Option[String] = None
    def authUrl(state: String, redirectUri: String): String = s"https://$name.test/authorize?state=$state"
    def exchangeCode(code: String, redirectUri: String): OauthProfile = {
      lastRedirectUri = Some(redirectUri)
      Profile
    }
  }

  /** A deployment serving `country`. */
  private def podFor(country: Country, provider: OauthProvider = new FakeProvider("google")) =
    new AuthController(
      Helpers.stubControllerComponents(), Map(provider.name -> provider),
      new InMemoryUserRepository,
      new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk),
      country, clock = fixedClk)

  /** A request as it arrives behind the proxy, which terminates TLS and forwards
   *  the original host — see `ForwardedUrl`. */
  private def arrivingAt(origin: String, path: String) = {
    val (scheme, host) = origin.split("://") match { case Array(s, h) => (s, h) }
    FakeRequest("GET", path).withHeaders("X-Forwarded-Proto" -> scheme, "X-Forwarded-Host" -> host)
  }

  private def sessionFor(state: String, provider: String = "google") =
    Seq("oauthState" -> state, "oauthProvider" -> provider, "oauthStateTimestamp" -> NowMs.toString)

  private val Apex = Country.oauthCallbackOrigin   // https://showtimes.cc
  private val PlOrigin = Country.Poland.webOrigin.value

  // ── The registered URL ───────────────────────────────────────────────────

  "The redirect_uri" should "be the SAME single URL from every deployed country" in {
    val urls = Country.switchable.flatMap(_.webOrigin).distinct
      .map(origin => AuthController.callbackUrlFor("google", origin))

    urls.distinct shouldBe List("https://showtimes.cc/auth/google/callback")
  }

  it should "differ only by provider — two console entries in total" in {
    AuthController.callbackUrlFor("google",   Apex) shouldBe "https://showtimes.cc/auth/google/callback"
    AuthController.callbackUrlFor("facebook", Apex) shouldBe "https://showtimes.cc/auth/facebook/callback"
  }

  // A developer's sign-in has to finish on their own machine: nothing registered
  // the apex on their behalf, and pointing them at production would be worse
  // than useless.
  it should "stay the caller's own address off a deployed origin" in {
    AuthController.callbackUrlFor("google", "http://localhost:9000") shouldBe
      "http://localhost:9000/auth/google/callback"
  }

  // The registered path is a literal because it must NOT carry the mount point
  // the reverse route would add on /uk. Pinned to the routes file through a
  // root-mounted reverse route so the two cannot drift apart.
  it should "match the route the application actually serves at the root" in {
    AuthController.callbackPath("google") shouldBe
      new controllers.ReverseAuthController("/").callback("google").url
    AuthController.callbackPath("facebook") shouldBe
      new controllers.ReverseAuthController("/").callback("facebook").url
  }

  // ── The state ────────────────────────────────────────────────────────────

  "The OAuth state" should "name the country that started the flow" in {
    Country.all.foreach { country =>
      AuthController.stateCountry(AuthController.newState(country)).value shouldBe country
    }
  }

  it should "still be unguessable — the nonce is what the cookie check compares" in {
    AuthController.newState(Country.Poland) should not be AuthController.newState(Country.Poland)
  }

  // A flow that started before this shape existed is still in the air across a
  // deploy; it finishes where it lands, which is what it did before.
  it should "name nothing when it carries no country" in {
    AuthController.stateCountry("2b6f0cc9-04b3-4f0c-9d1c-1c9d0f4b2a11") shouldBe empty
    AuthController.stateCountry("nonce.atlantis")                       shouldBe empty
    AuthController.stateCountry("")                                     shouldBe empty
  }

  // ── Finish here, or hand on ──────────────────────────────────────────────

  "A callback for a country on this origin" should "be finished here, not relayed" in {
    val state  = AuthController.newState(Country.UnitedKingdom)
    // The apex deployment IS Poland's process; the UK flow's cookie reached it
    // because the two share the showtimes.cc origin.
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(Apex, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    status(result) shouldBe SEE_OTHER
    session(result).get("userId").value shouldBe "alice@example.com"
  }

  it should "drop the visitor back on the country they signed in from, not on this one" in {
    val state  = AuthController.newState(Country.UnitedKingdom)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(Apex, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    landingAfter(result) shouldBe "https://showtimes.cc/uk/"
  }

  it should "stay on a relative landing when the flow is this deployment's own" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(PlOrigin, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    // Its own country, so the landing is this deployment's — reached, like every
    // sign-in now, by way of the other domain.
    landingAfter(result) shouldBe s"$PlOrigin/"
  }

  "A callback for a country on ANOTHER origin" should "be handed on, unread, to the deployment holding the session" in {
    val state  = AuthController.newState(Country.Poland)
    val provider = new FakeProvider("google")
    // No session: the browser's kinowo.net cookie was never sent to showtimes.cc.
    // That is exactly the case the relay exists for.
    val result = podFor(Country.Poland, provider).callback("google")(
      arrivingAt(Apex, s"/auth/google/callback?code=THE_CODE&state=$state"))

    status(result) shouldBe SEE_OTHER
    redirectLocation(result).value shouldBe
      s"https://kinowo.net/auth/google/callback?code=THE_CODE&state=$state"
    // Unread: nothing was exchanged here, so the code is still worth something
    // to the deployment it was handed to.
    provider.lastRedirectUri shouldBe empty
  }

  // A refusal has no code, and the deployment holding the session is the one
  // that should report it — so everything the provider sent is carried, not
  // just the happy-path pair.
  it should "carry a provider error across too" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(Apex, s"/auth/google/callback?error=access_denied&state=$state"))

    redirectLocation(result).value shouldBe
      s"https://kinowo.net/auth/google/callback?error=access_denied&state=$state"
  }

  it should "be finished normally once it arrives there" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(PlOrigin, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    session(result).get("userId").value shouldBe "alice@example.com"
  }

  // One hop, never two: the relay target is the origin that serves the flow's
  // country, so the check that sent it there passes on arrival.
  it should "not relay again from the deployment it was handed to" in {
    val state = AuthController.newState(Country.Poland)
    val landed = podFor(Country.Poland).callback("google")(
      arrivingAt(PlOrigin, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    redirectLocation(landed).value should not include "/auth/google/callback"
  }

  "A callback off a deployed origin" should "never be relayed at production" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      FakeRequest("GET", s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    session(result).get("userId").value shouldBe "alice@example.com"
  }

  // ── The round trip ───────────────────────────────────────────────────────

  "A UK sign-in" should "leave for the apex and come back to the UK site" in {
    val provider = new FakeProvider("google")
    val ukPod    = podFor(Country.UnitedKingdom, provider)

    // 1. Start on /uk. The provider is handed the ONE registered URL.
    val started = ukPod.start("google")(arrivingAt(Apex, "/uk/auth/google/start"))
    val state   = session(started).get("oauthState").value
    AuthController.stateCountry(state).value shouldBe Country.UnitedKingdom

    // 2. The provider comes back to the apex, which is Poland's process.
    val apexPod = podFor(Country.Poland, provider)
    val done    = apexPod.callback("google")(
      arrivingAt(Apex, s"/auth/google/callback?code=C&state=$state")
        .withSession(session(started).data.toSeq*))

    // 3. Signed in, and back where they started.
    session(done).get("userId").value shouldBe "alice@example.com"
    landingAfter(done)                shouldBe "https://showtimes.cc/uk/"
    // The exchange presented the same redirect_uri the authorize step did, or
    // the provider would have refused it.
    provider.lastRedirectUri.value shouldBe "https://showtimes.cc/auth/google/callback"
  }

  // ── Pairing the two domains, at sign-in and at sign-out ──────────────────
  //
  // kinowo.net and showtimes.cc share no cookie, so a session has to be
  // established on each. This happens on the way home from a sign-in, while the
  // visitor is already watching a redirect chain — not on arrival, which is the
  // shape it replaces: that spent a redirect on every signed-out visitor of an
  // overwhelmingly anonymous site, and could only be afforded once per browser,
  // so it stopped working the second time anyone signed in.

  private val SiblingOfPoland = Country.siblingOriginOf(Country.Poland).value
  private val UkBase          = Country.UnitedKingdom.webUrl.value

  /** Where a finished sign-in or sign-out actually drops the visitor.
   *
   *  Both now go via the OTHER domain — a sign-in to establish the session there,
   *  a sign-out to clear it — so the immediate redirect is the pairing leg and
   *  the destination is the `next` it carries. Unwrapping it here keeps these
   *  tests about where somebody ends up, which is what they were always about,
   *  rather than about the shape of the hop that gets them there. */
  private def landingAfter(result: scala.concurrent.Future[play.api.mvc.Result]): String = {
    val location = redirectLocation(result).value
    if (location.contains("/auth/sso/finish?") || location.contains("/auth/sso/logout?"))
      java.net.URLDecoder.decode(location.split("next=")(1).takeWhile(_ != '&'), "UTF-8")
    else location
  }

  /** A repository holding one signed-in person, and their id. */
  private def signedIn(repository: InMemoryUserRepository, email: String): String = {
    repository.upsert(models.User(
      id = email, provider = "google", providerSub = s"sub-$email", email = Some(email),
      displayName = Some("Alice"), avatarUrl = None, createdAt = Now, lastSeenAt = Now))
    email
  }

  /** A deployment serving `country`, with the repository and code store exposed
   *  so a test can seed a person or watch a code being spent. */
  private def podWith(country: Country): (AuthController, InMemoryUserRepository, AuthExchangeCodes) = {
    val repository = new InMemoryUserRepository
    val codes      = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk)
    (new AuthController(Helpers.stubControllerComponents(), Map.empty, repository, codes, country, clock = fixedClk),
     repository, codes)
  }

  "A finished sign-in" should "route home through the other domain, so both end up signed in" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(PlOrigin, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    status(result) shouldBe SEE_OTHER
    val location = redirectLocation(result).value
    location should startWith (s"$SiblingOfPoland/auth/sso/finish?code=")
    // ...and it carries where the visitor was actually going, so the far side
    // hands them back rather than stranding them on its own home page.
    location should include ("next=https%3A%2F%2Fkinowo.net%2F")
  }

  it should "still sign the visitor in HERE, whatever happens to the pairing leg" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(PlOrigin, s"/auth/google/callback?code=C&state=$state").withSession(sessionFor(state)*))

    session(result).get("userId").value shouldBe "alice@example.com"
  }

  // The native apps keep a cookie jar per host and finish through the deep link,
  // so there is no second domain to pair and the hop would break the handoff.
  it should "not pair for a native client finishing through the deep link" in {
    val state  = AuthController.newState(Country.Poland)
    val result = podFor(Country.Poland).callback("google")(
      arrivingAt(PlOrigin, s"/auth/google/callback?code=C&state=$state")
        .withSession((sessionFor(state) :+ ("mobileClient" -> "1"))*))

    redirectLocation(result).value should startWith ("kinowo://auth-done?code=")
  }

  "The pairing leg" should "sign the visitor in and hand them back where they were going" in {
    val repository = new InMemoryUserRepository
    val store      = new InMemoryAuthExchangeCodeStore
    def pod(country: Country) = new AuthController(
      Helpers.stubControllerComponents(), Map.empty, repository,
      new AuthExchangeCodes(store, fixedClk), country, clock = fixedClk)

    val userId = signedIn(repository, "alice@example.com")
    val code   = new AuthExchangeCodes(store, fixedClk).mint(userId)
    val landed = pod(Country.UnitedKingdom).ssoFinish()(
      arrivingAt(Apex, s"/auth/sso/finish?code=$code&next=$PlOrigin"))

    session(landed).get("userId").value shouldBe userId
    redirectLocation(landed).value      shouldBe s"$PlOrigin/"
  }

  // `next` is a redirect target named in a URL, which is an open redirect the
  // moment it is trusted. Same allowlist as the country switcher's `to`.
  it should "refuse to hand the visitor anywhere but a deployed country" in {
    val (ctl, repository, codes) = podWith(Country.Poland)
    val userId = signedIn(repository, "alice@example.com")
    val landed = ctl.ssoFinish()(
      arrivingAt(PlOrigin, s"/auth/sso/finish?code=${codes.mint(userId)}&next=https://evil.example.com"))

    session(landed).get("userId").value shouldBe userId
    redirectLocation(landed).value should not include "evil.example.com"
  }

  // ONE HOP, BY CONSTRUCTION: the pairing leg never pairs onwards, or the two
  // domains would hand the visitor back and forth forever.
  it should "not pair onwards from itself" in {
    val (ctl, repository, codes) = podWith(Country.Poland)
    val userId = signedIn(repository, "alice@example.com")
    val landed = ctl.ssoFinish()(
      arrivingAt(PlOrigin, s"/auth/sso/finish?code=${codes.mint(userId)}"))

    redirectLocation(landed).value should not include "/auth/sso/finish"
  }

  // ── Signing out has to mean both, too ────────────────────────────────────
  //
  // Pairing at sign-in makes a one-sided logout worse than never pairing at
  // all: the visitor is signed out where they pressed the button and still
  // signed in a domain away, which is the half nobody thinks to check.

  "Signing out" should "clear this domain and hand the logout to the other one" in {
    val (ctl, _, _) = podWith(Country.Poland)
    val result = ctl.logout()(arrivingAt(PlOrigin, "/auth/logout").withSession("userId" -> "alice@example.com"))

    session(result).get("userId") shouldBe empty
    val location = redirectLocation(result).value
    location should startWith (s"$SiblingOfPoland/auth/sso/logout?next=")
    location should include ("next=https%3A%2F%2Fkinowo.net%2F")
  }

  "The far half of a logout" should "clear the session there and send the visitor back" in {
    val (ctl, _, _) = podWith(Country.Poland)
    val result = ctl.ssoLogout()(
      arrivingAt(PlOrigin, s"/auth/sso/logout?next=$UkBase").withSession("userId" -> "alice@example.com"))

    session(result).get("userId") shouldBe empty
    redirectLocation(result).value shouldBe s"$UkBase/"
  }

  it should "not hand the logout onwards, or the pair would ping-pong" in {
    val (ctl, _, _) = podWith(Country.Poland)
    val result = ctl.ssoLogout()(
      arrivingAt(PlOrigin, "/auth/sso/logout").withSession("userId" -> "alice@example.com"))

    redirectLocation(result).value should not include "/auth/sso/logout"
  }

  it should "refuse to hand the visitor anywhere but a deployed country" in {
    val (ctl, _, _) = podWith(Country.Poland)
    val result = ctl.ssoLogout()(
      arrivingAt(PlOrigin, "/auth/sso/logout?next=https://evil.example.com"))

    redirectLocation(result).value should not include "evil.example.com"
  }

  // Pinned against a root-mounted reverse route, for the same reason as
  // `callbackPath`: these address ANOTHER deployment, whose mount point is not
  // ours, so they are literals that must not drift from the routes file.
  "The SSO leg paths" should "match the routes the application serves at the root" in {
    AuthController.SsoFinishPath shouldBe new controllers.ReverseAuthController("/").ssoFinish().url
    AuthController.SsoLogoutPath shouldBe new controllers.ReverseAuthController("/").ssoLogout().url
  }

}
