package controllers

import models.UserState
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc._
import services.metrics.LegacyUserStateMetrics
import services.users.{AccountDeletion, UserChangeTimeCache, UserRepository, UserStateRepository}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.time.format.DateTimeFormatter

/**
 * REST endpoint for the authenticated user's personalization state —
 * hidden films, (for legacy clients only — see `hiddenFilms()`) disabled
 * cinemas, and language.
 *
 * `get()`/`put()` are the original pair: full-state, no conditional-GET
 * support, kept running unchanged for clients that still call them —
 * `put()` also feeds `LegacyUserStateMetrics` on every call, the signal
 * that decides when it's safe to delete them. `hiddenFilms()` /
 * `hideFilm()` / `unhideFilm()` / `clearHiddenFilms()` are the granular,
 * per-country replacement for hiddenFilms specifically: `ETag`/
 * `Last-Modified` + 304 support on the read side, idempotent per-title
 * writes on the other. `language` has no granular successor — it's a
 * single explicit pick, not a set, so it stays on `get()`/`put()`.
 *
 * Shape (both directions, legacy):
 *   { "hiddenFilms":     [titles…],
 *     "disabledCinemas": [cinema display names…],
 *     "language":        "pl" | "en" | "de" | "es" | null }
 *
 * `language`, unlike the two sets, carries no union semantics: a client
 * either overwrites it with a pick of its own, or leaves it out of the
 * body to keep whatever is stored (same partial-update rule as the sets —
 * see `fromJson`).
 */
class UserStateController(
  cc:                   ControllerComponents,
  userStateRepository:   UserStateRepository,
  accountDeletion:      AccountDeletion,
  userChangeTimeCache:  UserChangeTimeCache,
  legacyUserStateMetrics: LegacyUserStateMetrics,
  userRepository:       UserRepository
) extends AbstractController(cc) {
  import UserStateController._

  // The signed-in visitor's id, or `None` for anonymous AND for a session
  // revoked since it was issued — routes every action here through the SAME
  // check `/api/me` uses (`SignedInUser`), rather than trusting the session
  // cookie's bare `userId` claim on its own. Trusting the claim alone (as
  // this controller did before 2026-09-20) meant a session `/api/me` had
  // already rejected — revoked, or its user row deleted — could still read
  // and write this endpoint's state indefinitely; `SignedInUser` is the one
  // place that knows how to tell a still-valid cookie from a stale one.
  private def signedInUserId(request: RequestHeader): Option[String] =
    SignedInUser(request, userRepository).map(_.id)

  // Every action here answers about ONE person, so every answer says so — see
  // `PerUserResponse`. Since the HTML pages stopped carrying a signed-in visitor
  // at all, these endpoints and `/api/me` are the ENTIRE per-user surface, and a
  // cached copy of one is the whole privacy failure the split was meant to end.
  def get(): Action[AnyContent] = Action { request =>
    PerUserResponse(signedInUserId(request) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        val state = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
        Ok(toJson(state))
    })
  }

  /** `GET /api/me/:country/hidden-films` — the hiddenFilms-only successor to
   *  `get()`, scoped to ONE country: unlike a cinema display name, a film
   *  title is not globally unique across countries, so the legacy single
   *  global `hiddenFilms` set (still served by `get()`/`put()`) can't be the
   *  per-country model's foundation — this reads `hiddenFilmsByCountry`
   *  instead. `country` is part of the path (not a query param) and must be
   *  a recognised code, or 400.
   *
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
   *
   *  `If-Modified-Since`-only requests get a further fast path: `userChangeTimeCache`
   *  may already prove nothing changed, answering the 304 with no read from
   *  storage at all. `If-None-Match` never takes this path — an opaque ETag
   *  string can't be verified against a bare timestamp, so it always reads
   *  through to `userStateRepository`.
   */
  def hiddenFilms(country: String): Action[AnyContent] = Action { request =>
    PerUserResponse((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)             => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None)       => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(userId), Some(country)) =>
        val ifNoneMatch     = request.headers.get("If-None-Match")
        val ifModifiedSince = request.headers.get("If-Modified-Since").flatMap(parseHttpDate)

        // Fast path: `If-None-Match` always needs real content to compare —
        // it's an opaque string, not derivable from a timestamp — so it's
        // excluded here. `If-Modified-Since` alone is exactly what
        // `userChangeTimeCache` answers: a 304 with ZERO reads from storage
        // when it can already prove nothing changed since then. The cache is
        // keyed by userId alone, not (userId, country) — it tracks the WHOLE
        // document's `updatedAt`, so a change to a DIFFERENT country's list
        // also (over-cautiously) invalidates this one's fast path. That's a
        // missed optimization, never a wrong answer: the cache only ever
        // proves "nothing at all changed", which safely implies "this
        // country's subset didn't either".
        val cacheProvenUnchanged =
          if (ifNoneMatch.isEmpty) ifModifiedSince.flatMap(ims => userChangeTimeCache.lastChangeAt(userId).filter(!_.isAfter(ims)))
          else None

        cacheProvenUnchanged match {
          case Some(lastChange) => NotModified.withHeaders("Last-Modified" -> httpDate(lastChange))
          case None              =>
            val state  = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
            val hidden = state.hiddenFilmsByCountry.getOrElse(country.code, Set.empty)
            val body   = hiddenFilmsJson(hidden)
            val etag   = hiddenFilmsETag(body)

            val notModified = ifNoneMatch match {
              case Some(inm) => inm.contains(etag)
              case None      => ifModifiedSince.exists(!state.updatedAt.isAfter(_))
            }

            if (notModified) NotModified.withHeaders("ETag" -> etag, "Last-Modified" -> httpDate(state.updatedAt))
            else respondWithHiddenFilms(hidden, state.updatedAt)
        }
    })
  }

  /** `PUT /api/me/:country/hidden-films/:title` — hide one film. Idempotent:
   *  hiding an already-hidden title is a no-op success, same 200 shape as
   *  every other outcome. `title` travels URL-encoded in the path (Play
   *  decodes it before this method ever sees it) — a client MUST
   *  percent-encode it the same way a query value would be (`encodeURIComponent`
   *  / `addingPercentEncoding` / `Uri.encode` — see each platform's existing
   *  share-link encoding for the precedent), since titles routinely carry
   *  spaces, non-ASCII text, and punctuation, occasionally even `/`.
   *
   *  Responds with the SAME shape as `hiddenFilms()`'s 200 — body, `ETag`,
   *  `Last-Modified` — so a client that just wrote doesn't need a follow-up
   *  GET to learn its new validators. */
  def hideFilm(country: String, title: String): Action[AnyContent] = Action { request =>
    PerUserResponse((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)       => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None) => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(userId), Some(c)) =>
        updateHiddenFilms(userId, c.code)(_ + title)
    })
  }

  /** `DELETE /api/me/:country/hidden-films/:title` — unhide one film.
   *  Idempotent: unhiding a title that was never hidden (or already unhidden)
   *  is a no-op success. See `hideFilm` for the encoding note and response shape. */
  def unhideFilm(country: String, title: String): Action[AnyContent] = Action { request =>
    PerUserResponse((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)       => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None) => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(userId), Some(c)) =>
        updateHiddenFilms(userId, c.code)(_ - title)
    })
  }

  /** `DELETE /api/me/:country/hidden-films` — unhide everything in ONE
   *  country. Other countries' buckets are untouched — this is deliberately
   *  narrower than "clear everything", matching how `hiddenFilms()` reads
   *  only one country at a time. */
  def clearHiddenFilms(country: String): Action[AnyContent] = Action { request =>
    PerUserResponse((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)       => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None) => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(userId), Some(c)) =>
        updateHiddenFilms(userId, c.code)(_ => Set.empty)
    })
  }

  /** Shared by `hideFilm`/`unhideFilm`/`clearHiddenFilms`: apply `f` to THIS
   *  country's bucket only and answer with the bucket as stored. Always writes
   *  (even when `f` is a no-op) — same "every write bumps `updatedAt`"
   *  behaviour the legacy PUT has. */
  private def updateHiddenFilms(userId: String, country: String)(f: Set[String] => Set[String]): Result =
    updateState(userId) { base =>
      Right(base.copy(hiddenFilmsByCountry =
        base.hiddenFilmsByCountry.updated(country, f(base.hiddenFilmsByCountry.getOrElse(country, Set.empty)))))
    }.fold(identity, state => respondWithHiddenFilms(state.hiddenFilmsByCountry.getOrElse(country, Set.empty), state.updatedAt))

  /** Every write's read-modify-write over the user's row: read it, apply
   *  `change`, and store the result ONLY if nothing else wrote the row in
   *  between (`replaceIfUnchanged`) — otherwise re-read and re-apply, so a
   *  concurrent request's write (another tab, the app, a hide in another
   *  country) is built on rather than overwritten by this request's stale copy.
   *  `Left` is `change`'s own refusal, or a 503 once [[MaxWriteAttempts]] races
   *  in a row have all been lost. */
  private def updateState(userId: String)(change: UserState => Either[Result, UserState]): Either[Result, UserState] = {
    @scala.annotation.tailrec
    def attempt(n: Int): Either[Result, UserState] = {
      val stored = userStateRepository.find(userId)
      change(stored.getOrElse(UserState.empty(userId))).map(_.copy(updatedAt = nextUpdatedAt(stored))) match {
        case Right(next) if !userStateRepository.replaceIfUnchanged(stored, next) =>
          if (n < MaxWriteAttempts) attempt(n + 1)
          else Left(ServiceUnavailable(Json.obj("error" -> "state is changing too fast — retry")))
        case outcome => outcome
      }
    }
    attempt(1)
  }

  /** The 200 shape `hiddenFilms()`'s non-304 branch and every write action
   *  share: the hiddenFilms-only body plus fresh `ETag`/`Last-Modified`. */
  private def respondWithHiddenFilms(hidden: Set[String], updatedAt: Instant): Result = {
    val body = hiddenFilmsJson(hidden)
    Ok(body).withHeaders("ETag" -> hiddenFilmsETag(body), "Last-Modified" -> httpDate(updatedAt))
  }

  def put(): Action[JsValue] = Action(parse.json) { request =>
    // Every call, regardless of outcome — even a 401 or a malformed body is
    // evidence SOMETHING out there still hits this URL, which is exactly what
    // decides whether it's safe to delete. See LegacyUserStateMetrics.
    legacyUserStateMetrics.recordPutCall()
    PerUserResponse(signedInUserId(request) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        // PUT is a partial update over the stored row (see fromJson): fields
        // the body omits keep their stored value, so a client that only
        // models some of the sets can't wipe the others.
        updateState(userId)(base => fromJson(base, request.body).left.map(reason => BadRequest(Json.obj("error" -> reason))))
          .fold(identity, state => Ok(toJson(state)))
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
    PerUserResponse(signedInUserId(request) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        accountDeletion.delete(userId)
        NoContent.withNewSession
    })
  }
}

object UserStateController {

  /** How many lost races [[UserStateController.updateState]] retries before
   *  giving up with a 503. A race needs another write to the SAME user's row
   *  inside one read-write round trip, so more than one in a row is already
   *  rare; five is a bound, not a tuning knob. */
  val MaxWriteAttempts = 5

  /** The `updatedAt` a write stamps — also the row's version for
   *  `replaceIfUnchanged`, so it must move even when two writes land in the
   *  same millisecond (Mongo stores `updatedAt` as a millisecond BSON date):
   *  now, or one millisecond past what was read, whichever is later. */
  def nextUpdatedAt(stored: Option[UserState], now: Instant = Instant.now()): Instant = {
    val tick = now.truncatedTo(java.time.temporal.ChronoUnit.MILLIS)
    stored.map(_.updatedAt.truncatedTo(java.time.temporal.ChronoUnit.MILLIS).plusMillis(1))
      .filter(_.isAfter(tick)).getOrElse(tick)
  }

  /** Render a `UserState` to its wire JSON. Sorted lists at the wire
   *  edge so the response is deterministic (helps caching and makes
   *  spec assertions stable); the in-memory model stays a Set.
   */
  def toJson(state: UserState): JsValue = Json.obj(
    "hiddenFilms"     -> state.hiddenFilms.toSeq.sorted,
    "disabledCinemas" -> state.disabledCinemas.toSeq.sorted,
    "language"        -> state.language
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

  /** The inverse of [[httpDate]], for `If-Modified-Since`. `None` on anything
   *  malformed — an unparseable validator is simply not "still current". */
  def parseHttpDate(value: String): Option[Instant] =
    scala.util.Try(DateTimeFormatter.RFC_1123_DATE_TIME.parse(value)).map(Instant.from).toOption

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
    // Present and a known code → overwrite; present and `null` → clear
    // (a client that wants to give up its pick sends this, though none do
    // today); absent → keep `base`'s value, same rule as the sets above.
    def language(fallback: Option[String]): Either[String, Option[String]] =
      (body \ "language").toOption match {
        case None            => Right(fallback)
        case Some(JsNull)    => Right(None)
        case Some(jsValue) =>
          jsValue.asOpt[String] match {
            case Some(code) if LanguageNames.Codes.contains(code) => Right(Some(code))
            case Some(code) => Left(s"language must be one of ${LanguageNames.Codes.mkString(", ")}, got $code")
            case None       => Left("language must be a string")
          }
      }
    for {
      hf   <- stringSet("hiddenFilms",     base.hiddenFilms)
      dc   <- stringSet("disabledCinemas", base.disabledCinemas)
      lang <- language(base.language)
      // `hiddenFilmsByCountry` is a field NEITHER this body's keys nor
      // `language`'s parsing can touch — it MUST be threaded through
      // explicitly, or every legacy PUT would silently wipe it back to the
      // constructor's default empty map.
      // `updatedAt` is left as read: the write stamps it (`updateState`).
    } yield UserState(base.userId, hf, dc, base.updatedAt, base.hiddenFilmsByCountry, lang)
  }
}
