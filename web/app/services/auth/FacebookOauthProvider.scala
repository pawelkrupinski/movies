package services.auth

import play.api.libs.json.Json
import tools.HttpFetch

import services.auth.GoogleOauthProvider.{formEncode, urlEncode}

/**
 * Facebook OAuth (Facebook Login) — same authorization-code flow as
 * Google, two API quirks worth noting:
 *
 *   - Facebook's token exchange is a GET (with credentials in the
 *     query string), not a POST form. The response is still JSON.
 *   - The userinfo equivalent is `/me?fields=id,name,email,picture`.
 *     `picture` is a NESTED object (`{data: {url: "…"}}`) — different
 *     from Google's flat `picture` string — so the parse has an extra
 *     `\` step.
 *
 * Email scope must be explicitly requested AND the user has to accept
 * it on the consent screen. About a third of FB users decline; that's
 * why `OauthProfile.email` is `Option`. We don't fail without email —
 * we just store `None` and the user can still favourite / hide.
 *
 * `formEncode` + `urlEncode` come from `GoogleOauthProvider`'s
 * companion — these two providers are the only OAuth callers and
 * sharing the URL-build helpers avoids a third copy.
 */
class FacebookOauthProvider(http: HttpFetch, appId: String, appSecret: String) extends OauthProvider {
  import FacebookOauthProvider._

  def name: String = "facebook"

  def authUrl(state: String, redirectUri: String): String =
    AuthEndpoint + "?" + formEncode(Seq(
      "client_id"    -> appId,
      "redirect_uri" -> redirectUri,
      "response_type"-> "code",
      "scope"        -> "email,public_profile",
      "state"        -> state
    ))

  def exchangeCode(code: String, redirectUri: String): OauthProfile = {
    val tokenUrl = TokenEndpoint + "?" + formEncode(Seq(
      "client_id"     -> appId,
      "client_secret" -> appSecret,
      "redirect_uri"  -> redirectUri,
      "code"          -> code
    ))
    val tokenJson   = http.get(tokenUrl)
    val accessToken = (Json.parse(tokenJson) \ "access_token").asOpt[String]
      .getOrElse(throw new RuntimeException(s"Facebook token response missing access_token: ${tokenJson.take(200)}"))

    val userinfoUrl = UserinfoEndpoint +
      s"?fields=id,name,email,picture&access_token=${urlEncode(accessToken)}"
    val js = Json.parse(http.get(userinfoUrl))
    OauthProfile(
      sub         = (js \ "id").asOpt[String]
                     .getOrElse(throw new RuntimeException(s"Facebook /me missing id: ${js.toString().take(200)}")),
      email       = (js \ "email").asOpt[String],
      displayName = (js \ "name").asOpt[String],
      avatarUrl   = (js \ "picture" \ "data" \ "url").asOpt[String]
    )
  }
}

object FacebookOauthProvider {
  val AuthEndpoint     = "https://www.facebook.com/v18.0/dialog/oauth"
  val TokenEndpoint    = "https://graph.facebook.com/v18.0/oauth/access_token"
  val UserinfoEndpoint = "https://graph.facebook.com/v18.0/me"
}
