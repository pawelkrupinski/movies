package controllers

import models.User
import play.api.Logging
import play.api.mvc._
import services.auth.AuthExchangeCodes
import services.users.UserRepository

import java.util.UUID

/**
 * Hands a live session to a deployment the session cookie cannot reach, and takes it away again.
 *
 *   - GET `/auth/sso/start?to=<base url>` — on the deployment the visitor is
 *     signed in to: send the browser to `to`'s `finish` for a binding, and —
 *     back with `&bind=…` — mint a one-shot code for that binding alone.
 *   - GET `/auth/sso/finish` — on the deployment being handed the session:
 *     codeless, stamp a random binding into this browser's session and ask
 *     back; with `?code=…`, spend it only if the bindings match, set `userId`,
 *     land on that country's home. The binding is what stops a signed-in
 *     attacker's code from signing a victim in (login CSRF).
 *   - GET `/auth/sso/logout` (and its `confirm` page) — the far half of
 *     [[AuthController.logout]].
 *
 * These legs exist for ONE boundary. `/uk`, `/de` and `/us` share an origin and
 * so share the session cookie outright (`AppLoader.mountedAt` leaves its path at
 * the host root); Poland is on `kinowo.net`, a different registrable domain,
 * where no cookie can follow. `to` is matched against the deployed countries'
 * own base URLs and nothing else, so it cannot be pointed anywhere off-brand.
 * [[AuthController]] starts the pairing leg after a sign-in and the far logout
 * after a sign-out; this controller answers them.
 *
 * Session key used: `ssoBinding` — set by a codeless `/auth/sso/finish`, spent
 * by the next one that carries a code.
 */
class SsoController(
  cc:             ControllerComponents,
  userRepository: UserRepository,
  // The one-shot codes a handoff carries across the domain boundary.
  exchangeCodes:  AuthExchangeCodes,
  // The country THIS deployment serves (see [[AuthController]]): where a finished
  // handoff lands, and the language of the logout question.
  country:        models.Country
) extends AbstractController(cc) with Logging {

  /** Hand this visitor's signed-in session to another country's deployment.
   *
   *  Only needed across an ORIGIN boundary — the Showtimes countries share one
   *  and so share the cookie — so a `to` on this request's own origin is just
   *  the plain link, and the switcher does not have to know which pairs those are.
   *
   *  Signed OUT is not an error: there is nothing to hand over, so this is just
   *  the link the switcher would have followed anyway, and the visitor lands
   *  where they asked to go. An unknown `to` IS an error, and a loud one: it can
   *  only be a hand-crafted URL, since the only thing that builds these is a
   *  `<select>` of the deployed countries.
   *
   *  TWO LEGS, because the code must be bound to the browser that will spend it
   *  (see [[ssoFinish]]). Without `bind` this only sends the visitor to the far
   *  side's finish, codeless, to be given a binding there; it comes back here
   *  with `bind` and only then is a code minted, for that binding alone.
   *
   *  The code rides in the query string, which is the one thing worth being
   *  uncomfortable about — mitigated by making it single-use, two minutes long,
   *  bound to one browser, and redeemed by a URL that only ever answers with a
   *  redirect, so no page ever renders (and no subresource sends a `Referer`)
   *  while it is still in the address bar. */
  def ssoStart(): Action[AnyContent] = Action { request =>
    SignedInUser.answering(SsoController.handoffTarget(request.getQueryString("to")) match {
      case None =>
        logger.warn(s"SSO start refused: '${request.getQueryString("to").getOrElse("")}' is not a deployed country.")
        BadRequest("Unknown country")
      case Some(target) => handOffTo(target, request)
    })
  }

  /** [[ssoStart]] toward an allowlisted `target`: its landing when there is no
   *  session to hand over (or none needed), else the next leg of the handoff. */
  private def handOffTo(target: String, request: RequestHeader): Result = {
    // Rides ALONGSIDE `to` rather than inside it: `to` is matched against the
    // deployed base URLs verbatim, so a query string on it is not a deployed
    // country. Carried across the hop so a signed-in visitor switching country
    // reaches the far landing knowing they chose it — same as the signed-out
    // switch, which navigates there directly. A `city=` the target does not
    // have counts too: the visitor still named the country, so the fallback is
    // its picker rather than a bare landing a stale `city` cookie would bounce.
    val namedCountry = LandingController.picksCity(request) ||
      request.getQueryString(LandingController.CityParam).exists(_.nonEmpty)
    val pick = if (namedCountry) LandingController.PickCityQuery else ""
    // A picker that already knows WHICH city was picked hands its slug
    // through `city=` instead of the bare "ask again" `pick=city` marker,
    // re-validated here against the TARGET country's own cities — never
    // trusted verbatim, since it arrives on a query string a visitor
    // controls — so the handoff below lands on the exact city rather than
    // dropping the visitor at that country's picker.
    val citySlug = SsoController.validatedCitySlug(target, request.getQueryString(LandingController.CityParam))
    val onward = citySlug.map(LandingController.CityParam -> _).toSeq ++
      (if (citySlug.isEmpty && pick.nonEmpty) Seq(LandingController.PickCityParam -> LandingController.PickCityValue) else Nil) ++
      SsoController.switchTarget(request.getQueryString("next")).map("next" -> _)
    val landing = Redirect(citySlug.map(s => s"$target/$s/").getOrElse(s"$target/$pick"))
    SignedInUser(request, userRepository) match {
      // NOTHING TO HAND OVER, and the marker matters. This endpoint is now
      // also reached unprompted, by a signed-out arrival probing whether the
      // visitor has a session on the other domain; sending it back to a bare
      // landing would leave the page it lands on unable to tell a first visit
      // from one that has just been answered, and it would probe again. The
      // probe's own cookie guard is the durable half; this is what holds when
      // a browser is refusing cookies, which is also a browser that can never
      // hold a session and must not be put in a loop chasing one.
      case None => landing
      // Same origin: the cookie is already there, and the far side's sibling
      // is not this origin, so a binding round trip would go astray.
      case Some(_) if ForwardedUrl.originOf(target) == ForwardedUrl.base(request) => landing
      case Some(user) => handOffSession(user, target, onward, request)
    }
  }

  /** The two legs of a signed-in handoff: without `bind`, send the browser to the
   *  far side's finish to be given a binding; back with one, mint a code for it. */
  private def handOffSession(user: User, target: String, onward: Seq[(String, String)], request: RequestHeader): Result =
    request.getQueryString(SsoController.BindParam).filter(_.nonEmpty) match {
      case None =>
        Redirect(s"$target${SsoController.SsoFinishPath}${AuthController.query(onward)}")
      case Some(binding) =>
        // The Location carries a live code: nothing may keep a copy of it.
        val code = exchangeCodes.mint(user.id, Some(binding))
        AuthController.uncacheable(Redirect(s"$target${SsoController.SsoFinishPath}${AuthController.query(("code" -> code) +: onward)}"))
    }

  /** Receive a session handed over by [[ssoStart]] on another domain.
   *
   *  CODELESS, this is the first leg: stamp a fresh random binding into THIS
   *  browser's session here and send it back to the other domain's `ssoStart`
   *  carrying that binding, so the code it mints can only be spent by a request
   *  whose session holds the same value. That is the login-CSRF defence: a
   *  signed-in attacker can still mint a code for their own account, but only
   *  bound to their own browser — a victim sent the finish link holds a
   *  different binding (or none) and lands signed out.
   *
   *  A code that does not redeem lands the visitor on this country's home page
   *  signed out rather than on an error: by the time they are here they have
   *  already left the page they came from, and the only thing they can do about
   *  a stale code is sign in again — which is exactly what the home page offers.
   *  The warning is for us, not them. */
  def ssoFinish(): Action[AnyContent] = Action { request =>
    val here = ForwardedUrl.base(request)
    (request.getQueryString("code"), models.DeploymentOrigins.siblingOfOrigin(here)) match {
      case (None, Some(sibling)) => askForBinding(sibling, here, request)
      case (code, _)             => spendHandoffCode(code, request)
    }
  }

  /** The codeless first leg of [[ssoFinish]]: a fresh binding in this browser's
   *  session, and back to the sibling's `ssoStart` carrying it. */
  private def askForBinding(sibling: String, here: String, request: RequestHeader): Result = {
    val binding = UUID.randomUUID().toString
    val self    = here + request.path.stripSuffix(SsoController.SsoFinishPath)
    val onward  = Seq(LandingController.CityParam, LandingController.PickCityParam, "next")
      .flatMap(key => request.getQueryString(key).map(key -> _))
    AuthController.uncacheable(Redirect(s"$sibling${SsoController.SsoStartPath}${AuthController.query(
      Seq("to" -> self, SsoController.BindParam -> binding) ++ onward)}")
      .withSession(request.session + (SsoController.SsoBindingKey -> binding)))
  }

  /** The second leg of [[ssoFinish]]: spend `code` against this browser's binding
   *  and land signed in — or signed out, when it does not redeem. */
  private def spendHandoffCode(code: Option[String], request: RequestHeader): Result = {
    val home = ssoFinishLanding(request)
    // Spent either way: a binding is good for one handoff, like its code.
    // `exchangeCodes.redeemHandoff` refuses a browser holding no binding at all.
    val binding = request.session.get(SsoController.SsoBindingKey)
    val cleared = request.session - SsoController.SsoBindingKey
    code.flatMap(exchangeCodes.redeemHandoff(_, binding)).flatMap(userRepository.findById) match {
      case None =>
        logger.warn("SSO handoff arrived without a code redeemable by this browser — landing signed out.")
        AuthController.uncacheable(home.withSession(cleared))
      case Some(user) =>
        // Deliberately does NOT pair onwards: this IS the pairing leg, and a
        // second hop from here is the loop.
        AuthController.uncacheable(home.withSession(SignedInUser.establish(cleared, user)))
    }
  }

  // `next` is where the visitor was actually going — set when this is the
  // pairing leg of a sign-in that happened on the other domain, so they land
  // back where they started rather than on this country's home page. Matched
  // against the deployed countries and nothing else: it is a redirect target
  // named in a URL, which is an open redirect the moment it is trusted.
  // A country switch that came through the handover still owes its landing the
  // marker: the visitor chose this country and should be asked which city,
  // not bounced to a cookie'd one — UNLESS `ssoStart` already knew which city
  // and handed its slug through `city=`, re-validated here against THIS
  // deployment's own cities (this process serves exactly `country`), in
  // which case the visitor lands directly on it instead of being asked again.
  private def ssoFinishLanding(request: RequestHeader): Result = {
    val pick = if (LandingController.picksCity(request)) LandingController.PickCityQuery else ""
    val citySlug = request.getQueryString(LandingController.CityParam).filter(country.bySlug.contains)
    Redirect(
      citySlug.map(s => s"${country.pathPrefix}/$s/").getOrElse(
        SsoController.switchTarget(request.getQueryString("next")).map(_ + "/")
          .getOrElse(routes.LandingController.index().url) + pick))
  }

  /** The far half of [[AuthController.logout]]: drop the session on this domain and send the
   *  visitor back where they pressed the button.
   *
   *  Never propagates onwards — this IS the propagation — so the pair cannot
   *  ping-pong. Reachable by GET because it is the second leg of a redirect —
   *  which any page could otherwise embed, so the route is `siteonly` and
   *  `CrossSiteWriteFilter` sends it on to the same place WITHOUT signing out
   *  unless it came from one of our own pages. */
  def ssoLogout(): Action[AnyContent] = Action { request =>
    AuthController.signedOut(Redirect(SsoController.ssoLogoutOnward(request.getQueryString("next"))).withNewSession)
  }

  /** The question a refused far half of a logout asks a visitor still signed in
   *  here (see `ssoLogoutConfirm.scala.html`): sign out here too — a click on this
   *  page, which `CrossSiteWriteFilter` can prove is ours — or go on signed in.
   *  Only a validated `next` is carried; one not signed in here is just sent on. */
  def ssoLogoutConfirm(next: Option[String]): Action[AnyContent] = Action { request =>
    val onward    = SsoController.ssoLogoutOnward(next)
    val validNext = SsoController.ssoLogoutNext(next)
    PerUserResponse(request.session.get(SignedInUser.UserIdKey) match {
      case None    => Redirect(onward)
      case Some(_) =>
        val signOut = routes.SsoController.ssoLogout().url + AuthController.query(validNext.map("next" -> _).toSeq)
        Ok(views.html.ssoLogoutConfirm(signOut, onward)(using deploymentMessages))
    })
  }

  private val deploymentMessages: play.api.i18n.Messages =
    cc.messagesApi.preferred(Seq(play.api.i18n.Lang(country.language.getLanguage)))
}

object SsoController {

  /** The SSO legs' paths, as the OTHER domain has to spell them.
   *
   *  Literals rather than reverse routes for the same reason as
   *  [[callbackPath]]: these address a DIFFERENT deployment, whose mount point is
   *  not ours, and the reverse route would helpfully prepend ours.
   *  `AuthCallbackRelaySpec` pins them against a root-mounted reverse route. */
  val SsoStartPath  = "/auth/sso/start"
  val SsoFinishPath = "/auth/sso/finish"
  val SsoLogoutPath = "/auth/sso/logout"

  /** Session key (on the RECEIVING domain) and query parameter (on the way back
   *  to the handing-over one) for the value that binds a handoff code to one
   *  browser — see [[SsoController.ssoFinish]]. */
  val SsoBindingKey = "ssoBinding"
  val BindParam     = "bind"

  /** Where [[SsoController.ssoStart]] may send a handoff: a deployed country's
   *  base URL (the country switch), or a deployed ORIGIN — the pairing leg of a
   *  sign-in asks from the far domain's root, where its `/auth` routes live.
   *  Still an exact-match allowlist of addresses we deploy, never a shape test. */
  private[controllers] def handoffTarget(to: Option[String]): Option[String] =
    switchTarget(to).orElse(
      to.map(_.trim.stripSuffix("/")).filter(models.DeploymentOrigins.deployedOrigins.contains))

  /** Where the far half of a logout sends the visitor: `next` when it is one
   *  of the deployed countries' base URLs, else this deployment's landing.
   *  Shared with `CrossSiteWriteFilter`, which sends a REFUSED far half to the
   *  same place, just without the sign-out. */
  def ssoLogoutOnward(next: Option[String]): String =
    ssoLogoutNext(next).map(_ + "/").getOrElse(routes.LandingController.index().url)

  /** A logout leg's `next`, when it is one of the deployed countries' base URLs —
   *  the only `next` a leg, or the question a refused leg asks, carries on. */
  def ssoLogoutNext(next: Option[String]): Option[String] = switchTarget(next)

  /** Where [[SsoController.ssoStart]] is willing to send a session: an EXACT
   *  match against a deployed country's own base URL (`Country.webUrl`), and
   *  nothing else.
   *
   *  An allowlist rather than a validated URL because this redirect carries a
   *  credential. Any rule of the shape "same host" or "https and one of our
   *  domains" is one open-redirect away from handing a live sign-in code to
   *  somebody else's server; a list of the addresses we actually deploy is
   *  not. A trailing slash is tolerated because the switcher's `<option>` values
   *  are base URLs and callers append to them.
   *
   *  Pure, so the refusal can be asserted without a request. */
  private[controllers] def switchTarget(to: Option[String]): Option[String] =
    to.map(_.trim.stripSuffix("/")).filter(_.nonEmpty)
      .flatMap(candidate => models.Country.switchable.flatMap(_.webUrl).find(_ == candidate))

  /** A `city=` query value passed to [[SsoController.ssoStart]], trusted only
   *  once it names a real city of the country `target` (one of
   *  [[switchTarget]]'s own allowlisted base URLs) belongs to — never taken
   *  verbatim, since it arrives on a query string a visitor controls. `None`
   *  for a missing, unknown, or mismatched slug, which callers fall back from
   *  rather than propagate.
   *
   *  Pure, so the validation can be asserted without a request. */
  private[controllers] def validatedCitySlug(target: String, slug: Option[String]): Option[String] =
    for {
      s       <- slug
      country <- models.Country.switchable.find(_.webUrl.contains(target))
      if country.bySlug.contains(s)
    } yield s
}
