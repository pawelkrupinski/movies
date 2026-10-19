package controllers

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore}
import services.users.InMemoryUserRepository

import java.time.{Clock, Instant, ZoneOffset}

import SignedInVisitors.signedIn

class SsoControllerSpec extends AnyFlatSpec with Matchers {

  private val Now      = Instant.parse("2026-05-19T12:00:00Z")
  private val fixedClk = Clock.fixed(Now, ZoneOffset.UTC)

  private def fixture(): (SsoController, InMemoryUserRepository, AuthExchangeCodes) =
    fixtureFor(models.Country.Poland)

  /** A deployment serving `country`, over a user store and a code store of its own. */
  private def fixtureFor(country: models.Country): (SsoController, InMemoryUserRepository, AuthExchangeCodes) = {
    val repository = new InMemoryUserRepository
    val codes      = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, fixedClk)
    (new SsoController(Helpers.stubControllerComponents(), repository, codes, country), repository, codes)
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

  "SsoController.switchTarget" should "accept every deployed country's own base URL" in {
    models.Country.switchable.flatMap(_.webUrl).foreach { base =>
      SsoController.switchTarget(Some(base)).value shouldBe base
    }
  }

  it should "tolerate a trailing slash, since the switcher's values are base URLs" in {
    SsoController.switchTarget(Some(s"$UkBase/")).value shouldBe UkBase
  }

  // This redirect carries a live sign-in code, so the target is an allowlist of
  // the addresses we actually deploy — not a shape test that some look-alike
  // host could satisfy.
  it should "refuse anything that is not one of them" in {
    SsoController.switchTarget(Some("https://evil.example.com"))     shouldBe empty
    SsoController.switchTarget(Some("https://showtimes.cc.evil.com")) shouldBe empty
    SsoController.switchTarget(Some("https://showtimes.cc/../uk"))    shouldBe empty
    SsoController.switchTarget(Some("//evil.example.com"))            shouldBe empty
    SsoController.switchTarget(Some(""))                              shouldBe empty
    SsoController.switchTarget(None)                                  shouldBe empty
  }

  "SsoController.validatedCitySlug" should "accept a slug that belongs to the target country" in {
    SsoController.validatedCitySlug(UkBase, Some("london")).value shouldBe "london"
  }

  it should "refuse a slug that belongs to a DIFFERENT country than the target" in {
    // "warszawa" is real, just not a UK city — trusting it verbatim would land
    // a UK-bound handoff on a URL that 404s (or worse, on a same-named slug
    // that happens to exist in the target country but means something else).
    SsoController.validatedCitySlug(UkBase, Some("warszawa")) shouldBe empty
  }

  it should "refuse a slug that names no city at all" in {
    SsoController.validatedCitySlug(UkBase, Some("not-a-real-city")) shouldBe empty
  }

  it should "refuse when nothing was passed" in {
    SsoController.validatedCitySlug(UkBase, None) shouldBe empty
  }

  it should "refuse when the target itself isn't a deployed country" in {
    SsoController.validatedCitySlug("https://evil.example.com", Some("london")) shouldBe empty
  }

  "SsoController.ssoStart" should "first send a signed-in visitor to the far side for a binding, minting nothing" in {
    val (ctl, repository, _) = fixture()
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase").withSession("userId" -> userId))

    status(result) shouldBe SEE_OTHER
    redirectLocation(result).value shouldBe s"$UkBase/auth/sso/finish"
  }

  it should "hand a signed-in visitor over with a one-shot code bound to the browser that asked" in {
    val (ctl, repository, codes) = fixture()
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase&bind=that-browser").withSession("userId" -> userId))

    val location = redirectLocation(result).value
    location should startWith (s"$UkBase/auth/sso/finish?code=")
    val code = queryParams(location)("code")
    codes.redeem(code, Some("another-browser")) shouldBe empty
    val again = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase&bind=that-browser").withSession("userId" -> userId))
    codes.redeem(queryParams(redirectLocation(again).value)("code"), Some("that-browser")).value shouldBe userId
  }

  // The Showtimes countries share one origin and so the cookie: there is nothing
  // to hand over, and the far side's sibling is not this origin.
  it should "send a signed-in visitor straight there when the target shares this origin" in {
    val (ctl, repository, _) = fixture()
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoStart()(
      EdgeRequest(s"/auth/sso/start?to=$UkBase&city=london", host = "showtimes.cc")
        .withSession("userId" -> userId))

    redirectLocation(result).value shouldBe s"$UkBase/london/"
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
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoStart()(
      FakeRequest("GET", "/auth/sso/start?to=https://evil.example.com").withSession("userId" -> userId))

    status(result) shouldBe BAD_REQUEST
  }

  // THE BUG THIS PINS. A picked city used to be dropped on this hop — a
  // signed-out visitor who picked "London" from kinowo.net landed on
  // `$UkBase/` (the UK's own picker) rather than on London itself.
  it should "send a signed-out visitor with a picked city straight to that city" in {
    val (ctl, _, _) = fixture()

    val result = ctl.ssoStart()(FakeRequest("GET", s"/auth/sso/start?to=$UkBase&city=london"))

    redirectLocation(result).value shouldBe s"$UkBase/london/"
  }

  // The visitor named the country even when the city is not one of its own: the
  // fallback is that country's picker (`pick=city`), not its bare landing, where
  // a `city` cookie from an earlier visit would bounce them somewhere they did
  // not ask for.
  it should "fall back to the target's picker when the picked city doesn't belong to it" in {
    val (ctl, repository, _) = fixture()

    val result = ctl.ssoStart()(FakeRequest("GET", s"/auth/sso/start?to=$UkBase&city=warszawa"))
    redirectLocation(result).value shouldBe s"$UkBase/${LandingController.PickCityQuery}"

    val userId = signedIn(repository, "alice@example.com", Now)
    val signedInResult = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase&city=warszawa").withSession("userId" -> userId))
    redirectLocation(signedInResult).value should include(s"${LandingController.PickCityParam}=${LandingController.PickCityValue}")
  }

  it should "carry a signed-in visitor's picked city onward to the finish leg" in {
    val (ctl, repository, _) = fixture()
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase&city=london").withSession("userId" -> userId))

    redirectLocation(result).value should include(s"city=london")
  }

  /** A finish request carrying a code bound to the browser it comes from. */
  private def boundFinish(codes: AuthExchangeCodes, userId: String, extra: String = "") =
    FakeRequest("GET", s"/auth/sso/finish?code=${codes.mint(userId, Some("this-browser"))}$extra")
      .withSession(SsoController.SsoBindingKey -> "this-browser")

  "SsoController.ssoFinish" should "sign the visitor in and land them on this country's home" in {
    val (ctl, repository, codes) = fixture()
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoFinish()(boundFinish(codes, userId))

    status(result) shouldBe SEE_OTHER
    session(result).get("userId").value shouldBe userId
    session(result).get(SsoController.SsoBindingKey) shouldBe empty
  }

  // Codeless is the first leg: this browser is given a binding, and sent back to
  // the domain that holds the session to have a code minted for it.
  it should "give a codeless arrival a binding and send it back to the other domain for its code" in {
    val (ctl, _, _) = fixtureFor(models.Country.UnitedKingdom)

    val result = ctl.ssoFinish()(
      EdgeRequest("/uk/auth/sso/finish?city=london", host = "showtimes.cc"))

    val binding  = session(result).get(SsoController.SsoBindingKey).value
    val location = redirectLocation(result).value
    location should startWith (s"$PlBase/auth/sso/start?")
    queryParams(location) shouldBe Map("to" -> UkBase, "bind" -> binding, "city" -> "london")
    session(result).get("userId") shouldBe empty
  }

  // An unbound code — the kind the native apps' deep link carries — is not a
  // handoff code, and this browser cannot spend it either.
  it should "refuse an unbound code" in {
    val (ctl, repository, codes) = fixture()
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoFinish()(
      FakeRequest("GET", s"/auth/sso/finish?code=${codes.mint(userId)}")
        .withSession(SsoController.SsoBindingKey -> "this-browser"))

    session(result).get("userId") shouldBe empty
  }

  // THE LOGIN-CSRF HOLE THE BINDING LEFT OPEN. Anyone can mint an UNBOUND code
  // for their own account — start a native sign-in (`?platform=ios`) in a
  // desktop browser and read it off the `kinowo://auth-done?code=…` it lands
  // on. A victim who follows `/auth/sso/finish?code=<that>` holds no binding at
  // all, and "no binding" must not match "minted without one".
  it should "refuse an unbound code from a browser holding no binding" in {
    val (ctl, repository, codes) = fixture()
    val mallory = signedIn(repository, "mallory@example.com", Now)

    val result = ctl.ssoFinish()(FakeRequest("GET", s"/auth/sso/finish?code=${codes.mint(mallory)}"))

    session(result).get("userId") shouldBe empty
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

  // THE BUG THIS PINS, on the receiving leg: a validated `city=` lands the
  // visitor on that exact city rather than on this country's bare home page.
  it should "land a validated city straight on that city instead of the home page" in {
    val (ctl, repository, codes) = fixtureFor(models.Country.UnitedKingdom)
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoFinish()(boundFinish(codes, userId, "&city=london"))

    redirectLocation(result).value shouldBe "/uk/london/"
    session(result).get("userId").value shouldBe userId
  }

  it should "fall back to the home page when the city doesn't belong to THIS deployment's country" in {
    val (ctl, repository, codes) = fixtureFor(models.Country.UnitedKingdom)
    val userId = signedIn(repository, "alice@example.com", Now)

    val result = ctl.ssoFinish()(boundFinish(codes, userId, "&city=warszawa"))

    redirectLocation(result).value shouldBe routes.LandingController.index().url
  }

  // THE WHOLE POINT, end to end: two controllers standing in for the two pods
  // either side of the domain boundary. They share a user repository and a code
  // store — which is what `MONGODB_USERS_DB` buys — and nothing else, no cookie
  // among them. An in-process code cache would fail exactly here, and used to.
  "A visitor switching country across the domain boundary" should "arrive signed in" in {
    val (poland, uk, repository) = pods()
    val userId = signedIn(repository, "alice@example.com", Now)

    session(handedOver(poland, uk, userId, s"to=$UkBase").arrival).get("userId").value shouldBe userId
  }

  // THE BUG THIS PINS, end to end: a signed-in visitor who picked a specific
  // city (not just a country) across the domain boundary arrives on that
  // city, not on the UK pod's own picker — through every leg of the handoff.
  it should "arrive on the exact city that was picked, not that country's picker" in {
    val (poland, uk, repository) = pods()
    val userId = signedIn(repository, "alice@example.com", Now)

    val arrival = handedOver(poland, uk, userId, s"to=$UkBase&city=london").arrival

    redirectLocation(arrival).value shouldBe "/uk/london/"
    session(arrival).get("userId").value shouldBe userId
  }

  it should "not be able to do it twice with the same code" in {
    val (poland, uk, repository) = pods()
    val userId  = signedIn(repository, "alice@example.com", Now)
    val handoff = handedOver(poland, uk, userId, s"to=$UkBase")

    // A replayed link — shoulder-surfed, or sitting in someone's history — is
    // worth nothing, even in the browser it was minted for.
    session(handoff.arrival).get("userId").value shouldBe userId
    session(uk.ssoFinish()(FakeRequest("GET", s"/uk/auth/sso/finish?code=${handoff.code}")
      .withSession(SsoController.SsoBindingKey -> handoff.binding))).get("userId") shouldBe empty
  }

  /** Two pods either side of the domain boundary. They share a user repository
   *  and a code store — which is what `MONGODB_USERS_DB` buys — and nothing else,
   *  no cookie among them. An in-process code cache would fail exactly here, and
   *  used to. */
  private def pods(): (SsoController, SsoController, InMemoryUserRepository) = {
    val repository = new InMemoryUserRepository
    val store      = new InMemoryAuthExchangeCodeStore
    def pod(country: models.Country) = new SsoController(
      Helpers.stubControllerComponents(), repository, new AuthExchangeCodes(store, fixedClk), country)
    (pod(models.Country.Poland), pod(models.Country.UnitedKingdom), repository)
  }

  private case class Handoff(arrival: scala.concurrent.Future[play.api.mvc.Result], code: String, binding: String)

  private def arriving(url: String) = {
    val origin = ForwardedUrl.originOf(url)
    val (scheme, host) = origin.split("://") match { case Array(sc, h) => (sc, h) }
    FakeRequest("GET", url.drop(origin.length)).withHeaders("X-Forwarded-Proto" -> scheme, "X-Forwarded-Host" -> host)
  }

  /** Every leg of a handoff from Poland (`kinowo.net`, where `userId` is signed
   *  in) to the UK (`showtimes.cc/uk`, where this browser holds no session yet),
   *  followed as the browser would follow the redirects; the final landing. */
  private def handedOver(poland: SsoController, uk: SsoController, userId: String, startQuery: String): Handoff = {
    val start  = poland.ssoStart()(arriving(s"$PlBase/auth/sso/start?$startQuery").withSession("userId" -> userId))
    val ask    = uk.ssoFinish()(arriving(redirectLocation(start).value))
    val onUk   = session(ask)
    val minted = poland.ssoStart()(arriving(redirectLocation(ask).value).withSession("userId" -> userId))
    Handoff(
      arrival = uk.ssoFinish()(arriving(redirectLocation(minted).value).withSession(onUk.data.toSeq*)),
      code    = queryParams(redirectLocation(minted).value)("code"),
      binding = onUk(SsoController.SsoBindingKey))
  }

  // THE BUG THIS PINS — login CSRF. Anyone signed in can run the handoff in
  // THEIR OWN browser, stop at the redirect, and send the finish link to
  // somebody else; following it used to sign the victim in as the attacker, so
  // whatever the victim then hid or saved landed in the attacker's account. A
  // code is now bound to the receiving browser, and only that browser can spend it.
  it should "not sign one browser in with a handoff code minted for another" in {
    val repository = new InMemoryUserRepository
    val store      = new InMemoryAuthExchangeCodeStore
    def pod(country: models.Country): SsoController = new SsoController(
      Helpers.stubControllerComponents(), repository, new AuthExchangeCodes(store, fixedClk), country)

    val mallory = signedIn(repository, "mallory@example.com", Now)
    val handoff = pod(models.Country.Poland).ssoStart()(
      FakeRequest("GET", s"/auth/sso/start?to=$UkBase&bind=mallorys-browser").withSession("userId" -> mallory))
    val code = queryParams(redirectLocation(handoff).value).get("code")

    val victim = pod(models.Country.UnitedKingdom).ssoFinish()(
      FakeRequest("GET", s"/uk/auth/sso/finish?code=${code.getOrElse("")}")
        .withSession(SsoController.SsoBindingKey -> "victims-browser"))
    session(victim).get("userId") shouldBe empty

    val noBinding = pod(models.Country.UnitedKingdom).ssoFinish()(
      FakeRequest("GET", s"/uk/auth/sso/finish?code=${code.getOrElse("")}"))
    session(noBinding).get("userId") shouldBe empty
  }

  private def queryParams(url: String): Map[String, String] =
    url.dropWhile(_ != '?').drop(1).split("&").toSeq.map(_.split("=", 2)).collect {
      case Array(k, v) => k -> java.net.URLDecoder.decode(v, "UTF-8")
    }.toMap
}
