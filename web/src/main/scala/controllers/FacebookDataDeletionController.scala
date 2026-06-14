package controllers

import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc._
import services.auth.FacebookSignedRequest
import services.users.{AccountDeletion, UserRepository}

/**
 * Facebook's "Data Deletion Request Callback" (Meta app dashboard →
 * Facebook Login → Settings → User Data Deletion).
 *
 * When a user removes our app from their Facebook account, Facebook
 * POSTs a `signed_request` to `POST /facebook/data-deletion`. We verify
 * it against the app secret, look up the local user by their Facebook
 * id (`provider=facebook`, `providerSub=<user_id>`), delete their row +
 * state, and return the JSON receipt Meta requires:
 *
 *   { "url": "<status page>", "confirmation_code": "<code>" }
 *
 * `GET /facebook/data-deletion/status` renders that human-readable
 * status page. Deletion is synchronous and already complete by the time
 * we respond, so the page is a static confirmation keyed by the echoed
 * code — we deliberately keep no record to look up (we just deleted
 * everything we held for that user).
 *
 * Wired only when `FACEBOOK_APP_SECRET` is set; absent it, the callback
 * returns 503 so a misconfigured deploy fails loudly rather than
 * silently accepting requests it cannot verify.
 */
class FacebookDataDeletionController(
  cc:              ControllerComponents,
  appSecret:       Option[String],
  userRepository:        UserRepository,
  accountDeletion: AccountDeletion
) extends AbstractController(cc) with Logging {

  def callback(): Action[AnyContent] = Action { request =>
    appSecret match {
      case None =>
        logger.error("Facebook data-deletion callback hit but FACEBOOK_APP_SECRET is unset")
        ServiceUnavailable(Json.obj("error" -> "Facebook integration not configured"))
      case Some(secret) =>
        request.body.asFormUrlEncoded.flatMap(_.get("signed_request")).flatMap(_.headOption) match {
          case None =>
            logger.warn("Facebook data-deletion callback missing signed_request")
            BadRequest(Json.obj("error" -> "missing signed_request"))
          case Some(signed) =>
            FacebookSignedRequest.parse(signed, secret) match {
              case Left(reason) =>
                logger.warn(s"Rejected Facebook data-deletion callback: $reason")
                BadRequest(Json.obj("error" -> reason))
              case Right(parsed) =>
                userRepository.findByProviderSub("facebook", parsed.userId) match {
                  case Some(user) =>
                    accountDeletion.delete(user.id)
                    logger.info(s"Facebook data-deletion: removed account ${user.id} (fb ${parsed.userId})")
                  case None =>
                    logger.info(s"Facebook data-deletion: no local account for fb ${parsed.userId} — nothing to delete")
                }
                val statusUrl = ForwardedUrl.base(request) +
                  routes.FacebookDataDeletionController.status(parsed.userId).url
                Ok(Json.obj("url" -> statusUrl, "confirmation_code" -> parsed.userId))
            }
        }
    }
  }

  def status(code: String): Action[AnyContent] = Action {
    Ok(views.html.facebookDataDeletion(code))
  }
}
