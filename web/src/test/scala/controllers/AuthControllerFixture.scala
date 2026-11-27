package controllers

import java.time.Clock

import play.api.mvc.ControllerComponents
import services.auth.{AuthExchangeCodes, NativeTokenVerifier, OauthAccounts, OauthProvider}
import services.users.UserRepository

/** The three sign-in controllers one deployment serves, over the same users, codes and clock; a spec reaches the
 *  route it exercises on the real controller (`pod.web.callback`, `pod.native.exchange`, `pod.session.me`). */
final case class SignInControllers(web: OauthSignInController, native: NativeSignInController, session: SessionController)

/** The specs' one way to build the sign-in controllers: their collaborators wired as the composition root wires them
 *  (`modules.webwiring.AuthWiring`) -- the account a profile signs in as over the same users and clock. */
object AuthControllerFixture {
  def apply(cc: ControllerComponents, providers: Map[String, OauthProvider], userRepository: UserRepository,
            exchangeCodes: AuthExchangeCodes, country: models.Country, clock: Clock,
            nativeTokens: NativeTokenVerifier = NativeTokenVerifier.Unconfigured): SignInControllers = {
    val accounts = new OauthAccounts(userRepository, clock)
    SignInControllers(
      new OauthSignInController(cc, providers, accounts, exchangeCodes, country, clock),
      new NativeSignInController(cc, userRepository, accounts, exchangeCodes, nativeTokens),
      new SessionController(cc, userRepository))
  }
}
