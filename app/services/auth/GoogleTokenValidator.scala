package services.auth

import play.api.libs.json.Json
import tools.HttpFetch

class GoogleTokenValidator(http: HttpFetch, clientId: String) {

  def validate(idToken: String): OauthProfile = {
    val body = http.get(s"https://oauth2.googleapis.com/tokeninfo?id_token=${GoogleOauthProvider.urlEncode(idToken)}")
    val js = Json.parse(body)
    val aud = (js \ "aud").asOpt[String]
      .getOrElse(throw new RuntimeException("Google tokeninfo missing aud"))
    if (aud != clientId)
      throw new RuntimeException(s"Google tokeninfo aud mismatch: expected $clientId, got $aud")
    OauthProfile(
      sub         = (js \ "sub").asOpt[String]
                      .getOrElse(throw new RuntimeException("Google tokeninfo missing sub")),
      email       = (js \ "email").asOpt[String],
      displayName = (js \ "name").asOpt[String],
      avatarUrl   = (js \ "picture").asOpt[String]
    )
  }
}
