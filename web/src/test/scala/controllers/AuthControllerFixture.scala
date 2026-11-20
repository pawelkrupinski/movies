package controllers

import java.time.Clock

import play.api.mvc.ControllerComponents
import services.auth.{AuthExchangeCodes, NativeTokenVerifier, OauthAccounts, OauthProvider}
import services.users.UserRepository

/** The specs' one way to build an [[AuthController]]: its collaborators wired as the composition root wires them
 *  (`modules.webwiring.AuthWiring`) -- the account a profile signs in as over the same users and clock. */
object AuthControllerFixture {
  def apply(cc: ControllerComponents, providers: Map[String, OauthProvider], userRepository: UserRepository,
            exchangeCodes: AuthExchangeCodes, country: models.Country, clock: Clock,
            nativeTokens: NativeTokenVerifier = NativeTokenVerifier.Unconfigured): AuthController =
    new AuthController(cc, providers, userRepository, new OauthAccounts(userRepository, clock), exchangeCodes, country, clock,
      nativeTokens)
}
