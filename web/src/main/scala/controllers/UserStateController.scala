package controllers

import models.UserState
import play.api.Logging
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc._
import services.users.{AccountDeletion, HiddenFilmsChange, UserChangeTimeCache, UserRepository, UserStateRepository}

import java.time.Instant
import java.time.format.DateTimeFormatter

/**
 * REST endpoint for the authenticated user's personalization state — the
 * per-country hidden films and the language pick.
 *
 * `hiddenFilms()` / `hideFilm()` / `unhideFilm()` / `clearHiddenFilms()` are
 * the granular, per-country hidden-films API: `ETag`/`Last-Modified` + 304
 * support on the read side, idempotent per-title writes on the other.
 *
 * `get()`/`put()` (`/api/me/state`) carry the language pick, which has no
 * granular successor — it's a single explicit pick, not a set. They once
 * carried the global `hiddenFilms`/`disabledCinemas` sets too; every client
 * since app 2.0.9 (2026-09-20), and the web, sends `{"language": …}` alone, so
 * `put()` now writes language only and IGNORES any set an ancient build still
 * sends (see `put()`). `get()` still answers with the stored, now-frozen sets
 * beside the language, because the pre-2.0.9 iOS decoder required
 * `hiddenFilms` and a build that old may still be installed:
 *   { "hiddenFilms":     [titles…],                       // frozen, never written
 *     "disabledCinemas": [cinema display names…],         // frozen, never written
 *     "language":        "pl" | "en" | "de" | "es" | null }
 */
class UserStateController(
  cc:                   ControllerComponents,
  userStateRepository:   UserStateRepository,
  accountDeletion:      AccountDeletion,
  userChangeTimeCache:  UserChangeTimeCache,
  userRepository:       UserRepository,
  // Stamps every change; the stored stamps' newer-than rules compare against it.
  clock:                java.time.Clock
) extends AbstractController(cc) with Logging {
  import UserStateController._

  // The signed-in visitor's id, or `None` for anonymous AND for a session
  // revoked since it was issued — routes every action here through the SAME
  // check `/api/me` uses (`SignedInUser`), rather than trusting the session
  // cookie's bare `userId` claim on its own. Trusting the claim alone (as
  // this controller did before 2026-09-20) meant a session `/api/me` had
  // already rejected — revoked, or its user row deleted — could still read
  // and write this endpoint's state indefinitely; `SignedInUser` is the one
  // place that knows how to tell a still-valid cookie from a stale one.
  //
  // A user row that could not be READ is neither: `SignedInUser` throws, and
  // every action answers through `SignedInUser.answering`, a per-user 503.
  private def signedInUserId(request: RequestHeader): Option[String] =
    SignedInUser(request, userRepository).map(_.id)

  /** `onRead` over this user's stored state — an empty one when they have none yet — or
   *  a 503 when it could not be READ. The empty state is a real answer ("nothing hidden"),
   *  with a validator a client caches, so a store that failed must never produce it. */
  private def readState(userId: String)(onRead: UserState => Result): Result =
    scala.util.Try(userStateRepository.find(userId)) match {
      case scala.util.Success(stored) => onRead(stored.getOrElse(UserState.empty(userId, clock.instant())))
      case scala.util.Failure(e) =>
        logger.warn(s"UserStateController: state for $userId unreadable: ${e.getClass.getSimpleName}: ${e.getMessage}")
        ServiceUnavailable(Json.obj("error" -> "state unavailable"))
    }

  // Every action here answers about ONE person, so every answer says so — see
  // `PerUserResponse`. Since the HTML pages stopped carrying a signed-in visitor
  // at all, these endpoints and `/api/me` are the ENTIRE per-user surface, and a
  // cached copy of one is the whole privacy failure the split was meant to end.
  def get(): Action[AnyContent] = Action { request =>
    SignedInUser.answering(signedInUserId(request) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        readState(userId)(state => Ok(toJson(state)))
    })
  }

  /** `GET /api/me/:country/hidden-films` — the hiddenFilms-only successor to
   *  `get()`, scoped to ONE country: unlike a cinema display name, a film
   *  title is not globally unique across countries, so the legacy single
   *  global `hiddenFilms` set (still served, frozen, by `get()`) can't be the
   *  per-country model's foundation — this reads `hiddenFilmsByCountry`
   *  instead. `country` is part of the path (not a query param) and must be
   *  a recognised code, or 400.
   *
   *  Conditional: a request already holding the current content (`If-None-Match`)
   *  or a still-current freshness bound (`If-Modified-Since`) gets a bodiless
   *  `304`, following the precedence HTTP requires — `If-None-Match`, when
   *  present, is authoritative and `If-Modified-Since` is not even consulted
   *  (RFC 7232 §3.3), since only the ETag reflects hiddenFilms specifically:
   *  `updatedAt` is still the whole row's timestamp, so a language pick
   *  (`PUT /api/me/state`) would otherwise look like a hiddenFilms change too.
   *
   *  disabledCinemas is deliberately absent from both the payload and the
   *  validators — it stopped being a server-synced field (device-local now).
   *
   *  `If-Modified-Since`-only requests get a further fast path: `userChangeTimeCache`
   *  may already prove nothing changed, answering the 304 with no read from
   *  storage at all. `If-None-Match` never takes this path — an opaque ETag
   *  string can't be verified against a bare timestamp, so it always reads
   *  through to `userStateRepository`.
   */
  def hiddenFilms(country: String): Action[AnyContent] = Action { request =>
    SignedInUser.answering((signedInUserId(request), models.Country.byCode(country)) match {
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
            readState(userId) { state =>
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
   *  GET to learn its new validators.
   *
   *  Bounded: a title over [[UserStateController.MaxTitleLength]] is a 400, and
   *  a NEW title into a bucket already holding
   *  [[UserStateController.MaxHiddenPerCountry]] is a 413 — nothing written
   *  either way. The bound is checked by the store inside the same atomic
   *  write (see `HiddenFilmsChange.Hide`), so parallel hides can't overshoot it;
   *  the title missing from the bucket the store answers with is the refusal. */
  def hideFilm(country: String, title: String): Action[AnyContent] = Action { request =>
    SignedInUser.answering((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)       => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None) => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(_), Some(_)) if title.length > MaxTitleLength =>
        BadRequest(Json.obj("error" -> s"title longer than $MaxTitleLength characters"))
      case (Some(userId), Some(c)) =>
        changeHiddenFilms(userId, c.code, HiddenFilmsChange.Hide(title, MaxHiddenPerCountry)) { (hidden, updatedAt) =>
          if (hidden.contains(title)) respondWithHiddenFilms(hidden, updatedAt)
          else EntityTooLarge(Json.obj("error" -> s"at most $MaxHiddenPerCountry hidden films per country"))
        }
    })
  }

  /** `DELETE /api/me/:country/hidden-films/:title` — unhide one film.
   *  Idempotent: unhiding a title that was never hidden (or already unhidden)
   *  is a no-op success. See `hideFilm` for the encoding note and response shape. */
  def unhideFilm(country: String, title: String): Action[AnyContent] = Action { request =>
    SignedInUser.answering((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)       => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None) => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(userId), Some(c)) =>
        changeHiddenFilms(userId, c.code, HiddenFilmsChange.Unhide(title))(respondWithHiddenFilms)
    })
  }

  /** `DELETE /api/me/:country/hidden-films` — unhide everything in ONE
   *  country. Other countries' buckets are untouched — this is deliberately
   *  narrower than "clear everything", matching how `hiddenFilms()` reads
   *  only one country at a time. */
  def clearHiddenFilms(country: String): Action[AnyContent] = Action { request =>
    SignedInUser.answering((signedInUserId(request), models.Country.byCode(country)) match {
      case (None, _)       => Unauthorized(Json.obj("error" -> "not logged in"))
      case (Some(_), None) => BadRequest(Json.obj("error" -> s"unrecognised country '$country'"))
      case (Some(userId), Some(c)) =>
        changeHiddenFilms(userId, c.code, HiddenFilmsChange.Clear)(respondWithHiddenFilms)
    })
  }

  /** Shared by `hideFilm`/`unhideFilm`/`clearHiddenFilms`: one atomic write to
   *  THIS country's bucket only (see `UserStateRepository.changeHiddenFilms`),
   *  answered from the bucket and `updatedAt` the store holds right after it —
   *  never a copy computed here, which a concurrent write could already have
   *  moved past. An applied change always bumps `updatedAt`, even a no-op one —
   *  same "every write bumps it" behaviour the language PUT has. A 503 when the
   *  store could not write at all. */
  private def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange)(respond: (Set[String], Instant) => Result): Result =
    userStateRepository.changeHiddenFilms(userId, country, change, clock.instant()) match {
      case Some(state) => respond(state.hiddenFilmsByCountry.getOrElse(country, Set.empty), state.updatedAt)
      case None        => ServiceUnavailable(Json.obj("error" -> "hidden films could not be saved — retry"))
    }

  /** The 200 shape `hiddenFilms()`'s non-304 branch and every write action
   *  share: the hiddenFilms-only body plus fresh `ETag`/`Last-Modified`. */
  private def respondWithHiddenFilms(hidden: Set[String], updatedAt: Instant): Result = {
    val body = hiddenFilmsJson(hidden)
    Ok(body).withHeaders("ETag" -> hiddenFilmsETag(body), "Last-Modified" -> httpDate(updatedAt))
  }

  /** `PUT /api/me/state` — the language pick. `language` present and a known
   *  code sets it, an explicit `null` clears it, an unknown code or non-string
   *  is a 400; absent leaves the stored pick alone and writes nothing at all.
   *
   *  Every other key is IGNORED, including the retired `hiddenFilms` /
   *  `disabledCinemas` sets — never written, never validated. Only a pre-2.0.9
   *  app build still sends them, beside its language; refusing the body would
   *  lose that language too, for a set the server stopped keeping. The answer
   *  is the stored state after the write, same shape as `get()`. */
  def put(): Action[JsValue] = Action(parse.json) { request =>
    SignedInUser.answering(signedInUserId(request) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        languageFrom(request.body) match {
          case Left(reason)  => BadRequest(Json.obj("error" -> reason))
          case Right(None)   => readState(userId)(state => Ok(toJson(state)))
          case Right(Some(language)) =>
            userStateRepository.setLanguage(userId, language, clock.instant())
              .fold(ServiceUnavailable(Json.obj("error" -> "state could not be saved — retry")))(state => Ok(toJson(state)))
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
    SignedInUser.answering(signedInUserId(request) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        accountDeletion.delete(userId)
        NoContent.withNewSession
    })
  }
}

object UserStateController {

  /** Longest title a per-country hide accepts — far past any real one (the
   *  longest in the corpus is under 200 characters), short of anything that is
   *  just padding. Longer → 400. */
  val MaxTitleLength = 500

  /** Most titles one country's hidden-films bucket may hold. Hiding a NEW title
   *  past it → 413; re-hiding one already there and unhiding always succeed.
   *  A country shows a few hundred films a month, so a human never gets here;
   *  what it bounds is a script growing one row toward Mongo's 16 MB limit. */
  val MaxHiddenPerCountry = 5000

  /** Render a `UserState` to its wire JSON. Sorted lists at the wire
   *  edge so the response is deterministic (helps caching and makes
   *  spec assertions stable); the in-memory model stays a Set. The two
   *  global sets are frozen — kept only for the pre-2.0.9 decoders (see the
   *  class doc).
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
    tools.Digest.contentEtag(Json.stringify(body))

  /** `instant` as an RFC 7231 HTTP-date, for the `Last-Modified` header. */
  def httpDate(instant: Instant): String =
    DateTimeFormatter.RFC_1123_DATE_TIME.format(instant.atOffset(java.time.ZoneOffset.UTC))

  /** The inverse of [[httpDate]], for `If-Modified-Since`. `None` on anything
   *  malformed — an unparseable validator is simply not "still current". */
  def parseHttpDate(value: String): Option[Instant] =
    scala.util.Try(DateTimeFormatter.RFC_1123_DATE_TIME.parse(value)).map(Instant.from).toOption

  /** The language change a `PUT /api/me/state` body asks for: `None` when the
   *  body carries no `language` (keep the stored pick), `Some(None)` for an
   *  explicit `null` (clear it — no client sends one today), `Some(Some(code))`
   *  for a known code. Left with a hint for an unknown code or a non-string.
   *  Every other key is ignored — see `put()`. */
  def languageFrom(body: JsValue): Either[String, Option[Option[String]]] =
    (body \ "language").toOption match {
      case None          => Right(None)
      case Some(JsNull)  => Right(Some(None))
      case Some(jsValue) =>
        jsValue.asOpt[String] match {
          case Some(code) if LanguageNames.Codes.contains(code) => Right(Some(Some(code)))
          case Some(code) => Left(s"language must be one of ${LanguageNames.Codes.mkString(", ")}, got $code")
          case None       => Left("language must be a string")
        }
    }
}
