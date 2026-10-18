package services.auth

/** Who a native app's provider token says is signing in (`POST /auth/token`).
 *
 *  Apple always sends an identity token. Google and Facebook send either a token
 *  for their validator or, with a `redirectUri`, an authorization code the app's
 *  own flow obtained — exchanged through the same [[OauthProvider]] the web flow uses.
 *  A provider that is not configured, or not known, throws like a refused token. */
final class NativeTokenVerifier(providers: Map[String, OauthProvider],
                                google: Option[GoogleTokenValidator],
                                facebook: Option[FacebookTokenValidator],
                                apple: Option[AppleTokenValidator]) {

  def profile(provider: String, token: String, fullName: Option[String], redirectUri: Option[String]): OauthProfile =
    provider match {
      case "apple"    => configured(apple, "Apple").validate(token, fullName)
      case "google"   => redirectUri.fold(configured(google, "Google").validate(token))(exchange("google", "Google", token, _))
      case "facebook" => redirectUri.fold(configured(facebook, "Facebook").validate(token))(exchange("facebook", "Facebook", token, _))
      case other      => throw new RuntimeException(s"Unknown provider: $other")
    }

  private def exchange(provider: String, label: String, code: String, redirectUri: String): OauthProfile =
    configured(providers.get(provider), label).exchangeCode(code, redirectUri)

  private def configured[A](validator: Option[A], label: String): A =
    validator.getOrElse(throw new RuntimeException(s"$label not configured"))
}
