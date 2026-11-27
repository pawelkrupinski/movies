package controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.util.UUID

import scala.util.{Failure, Success, Try}

import models.User
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.*
import play.api.mvc.Results.*
import services.auth.{OauthAccounts, OauthProfile}

/**
 * What the sign-in flows share: the `state` a web flow starts with and the country it names, the callback
 * URL handed to the provider, the session keys a flow carries, the cross-domain hop through the sibling
 * deployment, the one way a provider's profile becomes a signed-in account, and the answer a native
 * sign-in gives. [[OauthSignInController]] (the browser's provider flow), [[NativeSignInController]] (the
 * apps' token and code exchange) and [[SessionController]] (who is signed in, sign-out) are the three flows
 * that use it, each with only the collaborators it needs.
 *
 * Stateless: every function takes what it needs, so no flow can lean on another's state.
 */
object SignInFlow extends Logging {

  /** Who a signed-in response says is signed in — the one shape `/api/me`,
   *  `/auth/token` and `/auth/exchange` all answer with. */
  private[controllers] def userJson(user: User): JsValue = Json.obj(
    "displayName" -> user.displayName,
    "email"       -> user.email,
    "avatarUrl"   -> user.avatarUrl,
    "provider"    -> user.provider
  )

  /** What a refused `POST /auth/token` tells the client — the same words
   *  whatever went wrong; the reason goes to the server log. */
  val TokenRejected = "token not accepted"

  /** The `state` a flow starts with: a random nonce, and the code of the country
   *  that started it.
   *
   *  The NONCE is the security half and is unchanged — it is compared against the
   *  copy in the browser's own session cookie, so it still binds the callback to
   *  the browser that began the flow. The country code is a routing hint and
   *  nothing more: it is read only to pick one of the deployments we ship,
   *  never trusted as a URL, so a hand-edited `state` can send a callback to
   *  another of our own countries and nowhere else. */
  private[controllers] def newState(country: models.Country): String =
    s"${UUID.randomUUID()}.${country.code}"

  /** The country a `state` says started this flow, or `None` when it does not say
   *  — a `state` minted before this shape existed, still in flight across a
   *  deploy, or one somebody made up. Callers treat `None` as "finish it here",
   *  which is what those flows did before and what a fabricated one deserves. */
  private[controllers] def stateCountry(state: String): Option[models.Country] =
    Some(state.lastIndexOf('.')).filter(_ >= 0)
      .map(at => state.substring(at + 1))
      .flatMap(models.Country.byCode)

  /** The path the provider redirects to. Deliberately the ROOT path rather than
   *  this deployment's mounted one: `showtimes.cc/auth/google/callback` is the
   *  registered URL for every country, and the process that answers it is the one
   *  mounted at `/`. `AuthControllerCallbackUrlSpec` pins it against the reverse
   *  route of a root-mounted deployment so the literal cannot drift from the
   *  routes file. */
  private[controllers] def callbackPath(provider: String): String = s"/auth/$provider/callback"

  /** The `redirect_uri` for a request that arrived on `requestOrigin` — the apex
   *  on any origin we deploy, the caller's own address anywhere else. */
  private[controllers] def callbackUrlFor(provider: String, requestOrigin: String): String =
    (if (models.DeploymentOrigins.deployedOrigins.contains(requestOrigin)) models.DeploymentOrigins.oauthCallbackOrigin
     else requestOrigin) + callbackPath(provider)

  /** The provider's own query, forwarded verbatim to the deployment that can
   *  finish the flow.
   *
   *  Everything is carried, not just `code` and `state`: a provider that comes
   *  back with `error=access_denied` has no code, and the deployment holding the
   *  session is the one that should say so. Re-encoded rather than passed as a
   *  raw string so a value containing `&` cannot split into extra parameters. */
  private[controllers] def relayQuery(request: RequestHeader): String =
    query(request.queryString.toSeq.sortBy(_._1).flatMap { case (key, values) => values.map(key -> _) })

  /** Session key holding a native flow's `challenge` from start to callback. */
  val MobileChallengeKey = "mobileChallenge"

  /** Every session key a flow carries from start to callback. */
  private[controllers] val FlowKeys = Seq("oauthState", "oauthProvider", "oauthStateTimestamp", "mobileClient", MobileChallengeKey)

  /** `params` as a query string, keys and values encoded; empty for none. */
  private[controllers] def query(params: Seq[(String, String)]): String =
    if (params.isEmpty) ""
    else params.map { case (k, v) => s"${URLEncoder.encode(k, UTF_8)}=${URLEncoder.encode(v, UTF_8)}" }.mkString("?", "&", "")

  /** A landing as an absolute URL on `origin`.
   *
   *  Against the ORIGIN, because a relative landing came from a reverse route
   *  and already carries this deployment's mount point. Resolving `/uk/` against
   *  the COUNTRY's base URL — which itself ends in `/uk` — yields
   *  `showtimes.cc/uk/uk/`, which is not a deployed country, so the far side's
   *  allowlist refuses it and the visitor is handed to the wrong site entirely.
   *  That shipped, as a logout on any Showtimes country landing on kinowo.net.
   *
   *  Pure, because the reverse routes a spec sees are mounted at `/` however the
   *  deployment is configured, so the doubling is unreachable through a request. */
  private[controllers] def absoluteLanding(origin: String, landing: String): String =
    if (landing.startsWith("http")) landing else origin + landing

  /** `profile`'s account (through `accounts`) signed in and answered by `signedIn`. A failure — the
   *  profile itself (a provider's exchange), or the account it names (no email,
   *  an unwritable store) — is logged and answered with `failed`. */
  private[controllers] def signIn(accounts: OauthAccounts, provider: String, profile: => OauthProfile, failed: Result)(
    signedIn: User => Result): Result =
    Try(accounts.signIn(provider, profile)) match {
      case Failure(exception) =>
        logger.error(s"Sign-in through $provider failed: ${exception.getMessage}", exception)
        failed
      case Success(user) => signedIn(user)
    }

  /** `landing` by way of the other domain's `path`, which sends the visitor on to
   *  it (made absolute — see [[SignInFlow.absoluteLanding]]) — or `landing`
   *  itself when this origin has no sibling. */
  private[controllers] def viaSibling(path: String, landing: String, request: RequestHeader): String = {
    val origin = ForwardedUrl.base(request)
    models.DeploymentOrigins.siblingOfOrigin(origin) match {
      case None          => landing
      case Some(sibling) => s"$sibling$path${query(Seq("next" -> absoluteLanding(origin, landing)))}"
    }
  }

  /** A native sign-in's answer: who, plus a fresh session cookie naming them. */
  private[controllers] def signedInJson(user: User): Result =
    Ok(userJson(user)).withSession(SignedInUser.establish(Session.emptyCookie, user))

  /** The `redirect_uri` handed to the provider, and handed back at exchange —
   *  the two have to be byte-identical or the exchange is a
   *  `redirect_uri_mismatch`.
   *
   *  ONE per provider for the whole project: every deployed country names the
   *  apex, so the consoles hold two URLs rather than two per country. Off a
   *  deployed origin (localhost, a preview) it is the caller's own address
   *  instead — nothing has registered the apex on their behalf, and pointing a
   *  developer's sign-in at production would be worse than useless.
   *
   *  See `ForwardedUrl` for why the origin comes from the forwarded headers. */
  private[controllers] def callbackUrl(provider: String, request: RequestHeader): String =
    callbackUrlFor(provider, ForwardedUrl.base(request))
}
