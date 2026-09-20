package controllers

import models.UserState
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.users.{AccountDeletion, UserStateRepository}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.time.format.DateTimeFormatter

/**
 * REST endpoint for the authenticated user's personalization state —
 * hidden films and (for legacy clients only — see `hiddenFilms()`)
 * disabled cinemas.
 *
 * `get()`/`put()` are the original pair: full-state, no conditional-GET
 * support, kept running unchanged for clients that still call them.
 * `hiddenFilms()` is the hiddenFilms-only successor, with `ETag`/
 * `Last-Modified` + 304 support — the read half of a granular replacement;
 * per-title hide/unhide/clear-all write endpoints land alongside it next.
 *
 * Shape (both directions, legacy):
 *   { "hiddenFilms":     [titles…],
 *     "disabledCinemas": [cinema display names…] }
 */
class UserStateController(
  cc:              ControllerComponents,
  userStateRepository:   UserStateRepository,
  accountDeletion: AccountDeletion
) extends AbstractController(cc) {
  import UserStateController._

  // Every action here answers about ONE person, so every answer says so — see
  // `PerUserResponse`. Since the HTML pages stopped carrying a signed-in visitor
  // at all, these endpoints and `/api/me` are the ENTIRE per-user surface, and a
  // cached copy of one is the whole privacy failure the split was meant to end.
  def get(): Action[AnyContent] = Action { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        val state = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
        Ok(toJson(state))
    })
  }

  /** `GET /api/me/hidden-films` — the hiddenFilms-only successor to `get()`.
   *  Conditional: a request already holding the current content (`If-None-Match`)
   *  or a still-current freshness bound (`If-Modified-Since`) gets a bodiless
   *  `304`, following the precedence HTTP requires — `If-None-Match`, when
   *  present, is authoritative and `If-Modified-Since` is not even consulted
   *  (RFC 7232 §3.3), since only the ETag reflects hiddenFilms specifically:
   *  `updatedAt` is still the whole row's timestamp, so an old client's legacy
   *  `PUT /api/me/state` touching only disabledCinemas would otherwise look
   *  like a hiddenFilms change too.
   *
   *  disabledCinemas is deliberately absent from both the payload and the
   *  validators — it stopped being a server-synced field (kept device-local
   *  from here on); `get()`/`put()` still carry it for whatever legacy clients
   *  still send it.
   */
  def hiddenFilms(): Action[AnyContent] = Action { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        val state      = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
        val body       = hiddenFilmsJson(state.hiddenFilms)
        val etag       = hiddenFilmsETag(body)
        val lastModified = httpDate(state.updatedAt)
        val validators = Seq("ETag" -> etag, "Last-Modified" -> lastModified)

        val notModified = request.headers.get("If-None-Match") match {
          case Some(ifNoneMatch) => ifNoneMatch.contains(etag)
          case None              =>
            request.headers.get("If-Modified-Since").exists { ims =>
              scala.util.Try(DateTimeFormatter.RFC_1123_DATE_TIME.parse(ims))
                .map(Instant.from)
                .toOption
                .exists(!state.updatedAt.isAfter(_))
            }
        }

        if (notModified) NotModified.withHeaders(validators*)
        else Ok(body).withHeaders(validators*)
    })
  }

  def put(): Action[JsValue] = Action(parse.json) { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        // PUT is a partial update over the stored row (see fromJson): fields
        // the body omits keep their stored value, so a client that only
        // models some of the sets can't wipe the others.
        val base = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
        fromJson(base, request.body) match {
          case Left(reason) => BadRequest(Json.obj("error" -> reason))
          case Right(state) =>
            userStateRepository.upsert(state)
            Ok(toJson(state))
        }
    })
  }

  /** Hard-delete the user's row + state row, drop their session.
   *  GDPR-aligned: after this call we hold no row keyed by this user's
   *  id. The browser's localStorage is left alone — it's per-device, the
   *  user can clear it themselves; we don't have a server-side handle to
   *  do it.
   *
   *  Anonymous → 401. Authenticated → delete both rows (via the shared
   *  `AccountDeletion`, same path Facebook's callback uses) + return 204
   *  with the session cleared. The response carries no body so a fetch
   *  call doesn't need a parser. */
  def deleteAccount(): Action[AnyContent] = Action { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        accountDeletion.delete(userId)
        NoContent.withNewSession
    })
  }
}

object UserStateController {

  /** Render a `UserState` to its wire JSON. Sorted lists at the wire
   *  edge so the response is deterministic (helps caching and makes
   *  spec assertions stable); the in-memory model stays a Set.
   */
  def toJson(state: UserState): JsValue = Json.obj(
    "hiddenFilms"     -> state.hiddenFilms.toSeq.sorted,
    "disabledCinemas" -> state.disabledCinemas.toSeq.sorted
  )

  /** Render just the hiddenFilms set — the `hiddenFilms()` action's body. */
  def hiddenFilmsJson(hiddenFilms: Set[String]): JsValue = Json.obj(
    "hiddenFilms" -> hiddenFilms.toSeq.sorted
  )

  /** Strong ETag over `body`'s bytes — a quoted 16-hex-char SHA-256 prefix,
   *  same shape as [[models.Catalog.etag]]. Computed per request (this is
   *  per-user state, not a per-build constant), but cheap: the body is a
   *  handful of titles, not the whole catalog.
   */
  def hiddenFilmsETag(body: JsValue): String =
    "\"" + MessageDigest.getInstance("SHA-256")
      .digest(Json.stringify(body).getBytes(StandardCharsets.UTF_8))
      .take(8)
      .map("%02x".format(_))
      .mkString + "\""

  /** `instant` as an RFC 7231 HTTP-date, for the `Last-Modified` header. */
  def httpDate(instant: Instant): String =
    DateTimeFormatter.RFC_1123_DATE_TIME.format(instant.atOffset(java.time.ZoneOffset.UTC))

  /** Parse a wire JSON into `UserState` as a PARTIAL update over `base`: a
   *  field present in the body overwrites that set, a field the body omits
   *  keeps `base`'s value (and a present-but-empty array clears it). This
   *  lets a client send only the fields it owns without re-shipping, and
   *  without wiping the ones it doesn't model — the rule that mattered when
   *  the web carried two fields the mobile apps did not, and that stays
   *  because the next such field should not have to rediscover it. Wrong
   *  shape (non-array value, non-string element) returns Left with a hint.
   */
  def fromJson(base: UserState, body: JsValue): Either[String, UserState] = {
    def stringSet(field: String, fallback: Set[String]): Either[String, Set[String]] =
      (body \ field).toOption match {
        case None                      => Right(fallback)
        case Some(jsArray) =>
          jsArray.asOpt[Seq[String]] match {
            case Some(seq) => Right(seq.toSet)
            case None      => Left(s"$field must be an array of strings")
          }
      }
    for {
      hf <- stringSet("hiddenFilms",     base.hiddenFilms)
      dc <- stringSet("disabledCinemas", base.disabledCinemas)
    } yield UserState(base.userId, hf, dc, Instant.now())
  }
}
