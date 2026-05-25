package services.auth

import play.api.libs.json.Json
import tools.HttpFetch

class FacebookTokenValidator(http: HttpFetch, appId: String, appSecret: String) {

  def validate(accessToken: String): OauthProfile = {
    val debugUrl = s"https://graph.facebook.com/debug_token" +
      s"?input_token=${GoogleOauthProvider.urlEncode(accessToken)}" +
      s"&access_token=${GoogleOauthProvider.urlEncode(s"$appId|$appSecret")}"
    val debugJs = Json.parse(http.get(debugUrl))
    val data = (debugJs \ "data")
    val isValid = (data \ "is_valid").asOpt[Boolean].getOrElse(false)
    if (!isValid) throw new RuntimeException("Facebook token is not valid")
    val tokenAppId = (data \ "app_id").asOpt[String].getOrElse("")
    if (tokenAppId != appId)
      throw new RuntimeException(s"Facebook token app_id mismatch: expected $appId, got $tokenAppId")

    val profileUrl = s"https://graph.facebook.com/v18.0/me" +
      s"?fields=id,name,email,picture&access_token=${GoogleOauthProvider.urlEncode(accessToken)}"
    val js = Json.parse(http.get(profileUrl))
    OauthProfile(
      sub         = (js \ "id").asOpt[String]
                      .getOrElse(throw new RuntimeException("Facebook /me missing id")),
      email       = (js \ "email").asOpt[String],
      displayName = (js \ "name").asOpt[String],
      avatarUrl   = (js \ "picture" \ "data" \ "url").asOpt[String]
    )
  }
}
