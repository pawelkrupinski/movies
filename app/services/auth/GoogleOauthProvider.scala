package services.auth

import play.api.libs.json.Json
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
 * Google OAuth2 (authorization-code flow) — `openid+email+profile`
 * scopes return everything we need (`sub`, `email`, `name`,
 * `picture`) from a single `/userinfo` call.
 *
 * Two HTTP hops on callback:
 *
 *   1. POST `https://oauth2.googleapis.com/token`
 *      `application/x-www-form-urlencoded`
 *      `grant_type=authorization_code&code=…&client_id=…&client_secret=…&redirect_uri=…`
 *      → `{"access_token":"…","id_token":"…","scope":"…",…}`
 *
 *   2. GET `https://www.googleapis.com/oauth2/v3/userinfo`
 *      `Authorization: Bearer <access_token>`
 *      → `{"sub":"118…","email":"…","name":"…","picture":"…"}`
 *
 * The `id_token` from step 1 carries the same claims and would let us
 * skip step 2, but it's a signed JWT that needs verification against
 * Google's rotating JWKS — a non-trivial dependency for a one-line
 * round-trip win. The /userinfo call is one extra HTTP request and
 * we already pay it once per login (sessions persist).
 */
class GoogleOauthProvider(http: HttpFetch, clientId: String, clientSecret: String) extends OauthProvider {
  import GoogleOauthProvider._

  def name: String = "google"

  def authUrl(state: String, redirectUri: String): String = {
    val base = Seq(
      "client_id"     -> clientId,
      "redirect_uri"  -> redirectUri,
      "response_type" -> "code",
      "scope"         -> "openid email profile",
      "state"         -> state,
      "prompt"        -> "select_account"
    )
    val params = if (isPrivateIp(redirectUri)) {
      base ++ Seq("device_id" -> clientId, "device_name" -> "local-dev")
    } else base
    AuthEndpoint + "?" + formEncode(params)
  }

  def exchangeCode(code: String, redirectUri: String): OauthProfile = {
    val tokenBody = formEncode(Seq(
      "grant_type"    -> "authorization_code",
      "code"          -> code,
      "client_id"     -> clientId,
      "client_secret" -> clientSecret,
      "redirect_uri"  -> redirectUri
    ))
    val tokenJson   = http.post(TokenEndpoint, tokenBody, "application/x-www-form-urlencoded")
    val accessToken = (Json.parse(tokenJson) \ "access_token").asOpt[String]
      .getOrElse(throw new RuntimeException(s"Google token response missing access_token: ${tokenJson.take(200)}"))

    val userinfoJson = http.get(s"$UserinfoEndpoint?access_token=${urlEncode(accessToken)}")
    val js           = Json.parse(userinfoJson)
    OauthProfile(
      sub         = (js \ "sub").asOpt[String]
                     .getOrElse(throw new RuntimeException(s"Google userinfo missing sub: ${userinfoJson.take(200)}")),
      email       = (js \ "email").asOpt[String],
      displayName = (js \ "name").asOpt[String],
      avatarUrl   = (js \ "picture").asOpt[String]
    )
  }
}

object GoogleOauthProvider {
  val AuthEndpoint     = "https://accounts.google.com/o/oauth2/v2/auth"
  val TokenEndpoint    = "https://oauth2.googleapis.com/token"
  val UserinfoEndpoint = "https://www.googleapis.com/oauth2/v3/userinfo"

  private val PrivateIpPattern =
    """https?://(?:10\.|172\.(?:1[6-9]|2\d|3[01])\.|192\.168\.|127\.)""".r.unanchored

  def isPrivateIp(uri: String): Boolean = PrivateIpPattern.findFirstIn(uri).isDefined

  def formEncode(pairs: Seq[(String, String)]): String =
    pairs.map { case (k, v) => s"${urlEncode(k)}=${urlEncode(v)}" }.mkString("&")

  def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
