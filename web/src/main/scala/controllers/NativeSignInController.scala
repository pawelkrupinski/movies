package controllers

import scala.util.{Failure, Success, Try}

import models.User
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.*
import services.auth.{AuthExchangeCodes, NativeTokenVerifier, OauthAccounts}
import services.users.UserRepository

/**
 * The native apps' sign-in: a provider token in (`POST /auth/token`) and the one-shot code their
 * `kinowo://` deep link carried back from a browser flow (`POST /auth/exchange`), each answered with the
 * user plus a fresh session cookie — `PerUserResponse`, like every response here that names someone.
 * The browser's own flow is [[OauthSignInController]]'s; what they share is [[SignInFlow]].
 */
class NativeSignInController(
  cc:            ControllerComponents,
  userRepository: UserRepository,
  // The account a provider's profile signs in as.
  accounts:      OauthAccounts,
  exchangeCodes: AuthExchangeCodes,
  // Who a native app's provider token names (`POST /auth/token`).
  nativeTokens:  NativeTokenVerifier = NativeTokenVerifier.Unconfigured
) extends AbstractController(cc) with Logging {

  /** Native sign-in: a provider token in, the user plus a session cookie out —
   *  `PerUserResponse`, like every response here that names someone. */
  def token(): Action[JsValue] = Action(parse.json) { request => PerUserResponse(signInWithToken(request.body)) }

  private def signInWithToken(body: JsValue): Result =
    ((body \ "provider").asOpt[String], (body \ "token").asOpt[String]) match {
      case (None, _) => BadRequest(Json.obj("error" -> "missing provider"))
      case (_, None) => BadRequest(Json.obj("error" -> "missing token"))
      case (Some(provider), Some(token)) =>
        Try(nativeTokens.profile(provider, token, (body \ "fullName").asOpt[String], (body \ "redirectUri").asOpt[String])) match {
          case Failure(exception) =>
            logger.warn(s"Token validation failed for $provider: ${exception.getMessage}", exception)
            Unauthorized(Json.obj("error" -> SignInFlow.TokenRejected))
          case Success(profile) =>
            SignInFlow.signIn(accounts, provider, profile, failed = InternalServerError(Json.obj("error" -> "Couldn't complete sign-in.")))(SignInFlow.signedInJson)
        }
    }

  def exchange(): Action[JsValue] = Action(parse.json) { request => PerUserResponse(redeemExchangeCode(request.body)) }

  private def redeemExchangeCode(body: JsValue): Result = {
    val user = for {
      userId <- (body \ "code").asOpt[String].flatMap(exchangeCodes.redeem(_, verifier = (body \ "verifier").asOpt[String]))
                  .toRight("invalid or expired code")
      user   <- userRepository.findById(userId).toRight("user not found")
    } yield user
    user.fold(error => Unauthorized(Json.obj("error" -> error)), SignInFlow.signedInJson)
  }
}
