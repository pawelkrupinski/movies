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
 * POSTimestamp a `signed_request` to `POST /facebook/data-deletion`. We verify
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

  /** `GET /facebook/data-deletion` — the human-readable deletion instructions,
   *  served on the SAME path as the POST callback above.
   *
   *  Meta's dashboard has two data-deletion fields: a callback it POSTs a
   *  `signed_request` to, and an "instructions URL" it FETCHES and refuses to
   *  save unless it answers. A POST-only route gave a bare 404 to the second and
   *  to anyone pasting the URL into a browser, which reads as a broken deployment
   *  rather than as the wrong verb. Answering both verbs on one path means either
   *  field can take the same URL.
   *
   *  Unlike [[callback]] this does NOT depend on `FACEBOOK_APP_SECRET`: it
   *  discloses nothing and Meta fetches it while reviewing an app that may not be
   *  fully configured yet, so a 503 here would block the review it exists for.
   *
   *  Language comes from the LINK (`?lang=`) with the deployment's own as the
   *  fallback, exactly as [[LegalController.privacy]] does — the reviewer reading
   *  it is rarely in the country whose deployment answers. */
  def instructions(lang: Option[String]): Action[AnyContent] = Action {
    val language = lang.map(_.trim.toLowerCase)
      .filter(_.nonEmpty)
      .getOrElse(models.Country.fromEnv.language.getLanguage)
    Ok(if (language == "pl") views.html.facebookDataDeletionInstructions()
       else views.html.facebookDataDeletionInstructionsEn())
  }

  def status(code: String): Action[AnyContent] = Action {
    if (models.Country.fromEnv.language.getLanguage == "pl") Ok(views.html.facebookDataDeletion(code))
    else Ok(views.html.facebookDataDeletionEn(code))
  }
}
