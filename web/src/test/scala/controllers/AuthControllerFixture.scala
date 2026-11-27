package controllers

import java.time.Clock

import play.api.libs.json.JsValue
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.auth.{AuthExchangeCodes, NativeTokenVerifier, OauthAccounts, OauthProvider}
import services.users.UserRepository

/** The three sign-in controllers a spec drives as one: each route of the sign-in surface under the name it always had. */
final class AuthControllers(val web: OauthSignInController, val native: NativeSignInController, val session: SessionController) {
  def start(provider: String): Action[AnyContent]    = web.start(provider)
  def callback(provider: String): Action[AnyContent] = web.callback(provider)
  def token(): Action[JsValue]                       = native.token()
  def exchange(): Action[JsValue]                    = native.exchange()
  def me(): Action[AnyContent]                       = session.me()
  def logout(): Action[AnyContent]                   = session.logout()
}

/** The specs' one way to build the sign-in controllers: their collaborators wired as the composition root wires them
 *  (`modules.webwiring.AuthWiring`) -- the account a profile signs in as over the same users and clock. */
object AuthControllerFixture {
  def apply(cc: ControllerComponents, providers: Map[String, OauthProvider], userRepository: UserRepository,
            exchangeCodes: AuthExchangeCodes, country: models.Country, clock: Clock,
            nativeTokens: NativeTokenVerifier = NativeTokenVerifier.Unconfigured): AuthControllers = {
    val accounts = new OauthAccounts(userRepository, clock)
    new AuthControllers(
      new OauthSignInController(cc, providers, accounts, exchangeCodes, country, clock),
      new NativeSignInController(cc, userRepository, accounts, exchangeCodes, nativeTokens),
      new SessionController(cc, userRepository))
  }
}
