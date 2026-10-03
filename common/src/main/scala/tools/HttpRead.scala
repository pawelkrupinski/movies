package tools

import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.util.control.NonFatal

/**
 * The read every client makes through [[HttpFetch]], returning a [[ReadOutcome]] instead
 * of a bare body.
 *
 * A non-2xx is already typed at the leaf (`RealHttpFetch` throws [[HttpStatusException]])
 * and goes through [[ReadOutcome.classify]]: 404/410 is [[ReadOutcome.Absent]], anything
 * else [[ReadOutcome.Failed]]. What the leaf cannot see is a 2xx that is not the content
 * the endpoint serves — a Cloudflare / DataDome / Incapsula challenge, an HTML error page
 * where JSON was expected, an error document of the wrong shape — and that is what every
 * helper here checks before the caller's parser runs. A body that fails the check is
 * `Failed(UnexpectedBody)`, never `Absent`.
 *
 * The parser returns a [[ReadOutcome]] so it can say "the upstream answered none"
 * ([[ReadOutcome.none]]) — the only other road to `Absent`. A parser that THROWS is an
 * unexpected body too: that replaces the `Try(parse(body)).toOption` that read a changed
 * page format as "no data".
 */
object HttpRead {

  /** What a 2xx HTML page must contain to be the page the caller asked for. */
  final case class PageMarker(value: String) {
    def foundIn(body: String): Boolean = body.contains(value)
  }

  /** A body with no content check beyond "not a known challenge page". For plain-text
   *  endpoints and pages whose shape the parser itself validates. */
  def text[A](fetch: HttpFetch, url: String, headers: Map[String, String] = Map.empty)
             (parse: String => ReadOutcome[A]): ReadOutcome[A] =
    read(fetch, url, headers) { body =>
      ChallengePage.detect(body) match {
        case Some(challenge) => unexpected(url, s"a $challenge challenge page", body)
        case None            => parse(body)
      }
    }

  /** A page that must be there — a cinema's listing or one of its day pages: its body, and a
   *  throw for anything else, a 404 included (rethrown as the original status, so a gone page
   *  still reads as gone upstream) and a challenge page served with a 200. */
  def page(fetch: HttpFetch, url: String, headers: Map[String, String] = Map.empty): String =
    text(fetch, url, headers)(ReadOutcome.Answered(_)).required

  /** [[page]] for a page served in reply to a POST (a WordPress admin-ajax day, a GraphQL
   *  listing query): its body, and a throw for anything else, a challenge page included. */
  def postPage(fetch: HttpFetch, url: String, body: String, contentType: String = "application/json"): String =
    ReadOutcome.of(fetch.post(url, body, contentType)).flatMap { answer =>
      ChallengePage.detect(answer).fold[ReadOutcome[String]](ReadOutcome.Answered(answer))(vendor =>
        unexpected(url, s"a $vendor challenge page", answer))
    }.required

  /** [[page]]'s undecoded bytes, for a legacy single-byte site that ships no charset. The
   *  interstitials are ASCII, so they are recognised in any single-byte reading. */
  def pageBytes(fetch: HttpFetch, url: String): Array[Byte] =
    ReadOutcome.of(fetch.getBytes(url)).flatMap { bytes =>
      val ascii = new String(bytes, java.nio.charset.StandardCharsets.ISO_8859_1)
      ChallengePage.detect(ascii).fold[ReadOutcome[Array[Byte]]](ReadOutcome.Answered(bytes))(vendor =>
        unexpected(url, s"a $vendor challenge page", ascii))
    }.required

  /** A page's body, `None` when the upstream answers that it has none (404/410), and a
   *  throw for any other failure, a challenge page included — the shape a rating client
   *  that parses a page it may legitimately not find wants. */
  def pageOrNone(fetch: HttpFetch, url: String): Option[String] =
    text(fetch, url)(ReadOutcome.Answered(_)).toOptionOrThrow

  /** An HTML page that must carry `marker` — something only the real page has (its
   *  listing container, a `__NEXT_DATA__` script, a JSON-LD block) — and must not be a
   *  challenge page. */
  def html[A](fetch: HttpFetch, url: String, marker: PageMarker, headers: Map[String, String] = Map.empty)
             (parse: String => ReadOutcome[A]): ReadOutcome[A] =
    text(fetch, url, headers) { body =>
      if (marker.foundIn(body)) parse(body)
      else unexpected(url, s"no '${marker.value}' in the page", body)
    }

  /** A JSON endpoint whose root is an object. */
  def jsonObject[A](fetch: HttpFetch, url: String, headers: Map[String, String] = Map.empty)
                   (parse: JsObject => ReadOutcome[A]): ReadOutcome[A] =
    json[JsObject, A](fetch, url, headers, "an object") { case o: JsObject => o } (parse)

  /** A JSON endpoint whose root is an array. An empty array is NOT absent by itself —
   *  only the caller knows whether `[]` means "none" ([[ReadOutcome.none]]) for its API. */
  def jsonArray[A](fetch: HttpFetch, url: String, headers: Map[String, String] = Map.empty)
                  (parse: JsArray => ReadOutcome[A]): ReadOutcome[A] =
    json[JsArray, A](fetch, url, headers, "an array") { case a: JsArray => a } (parse)

  /** A JSON endpoint queried by POST (GraphQL) whose root is an object. */
  def postJsonObject[A](fetch: HttpFetch, url: String, body: String, contentType: String = "application/json")
                       (parse: JsObject => ReadOutcome[A]): ReadOutcome[A] =
    ReadOutcome.of(fetch.post(url, body, contentType)).flatMap(checkJsonObject(url, _)(parse))

  /** Check an already-fetched body as JSON of the expected root — for clients whose
   *  transport is not a plain `HttpFetch.get` (a POST, a cached detail fetch). */
  def checkJsonObject[A](url: String, body: String)(parse: JsObject => ReadOutcome[A]): ReadOutcome[A] =
    guarded(url, body)(asJson[JsObject, A](url, _, "an object") { case o: JsObject => o } (parse))

  def checkJsonArray[A](url: String, body: String)(parse: JsArray => ReadOutcome[A]): ReadOutcome[A] =
    guarded(url, body)(asJson[JsArray, A](url, _, "an array") { case a: JsArray => a } (parse))

  private def json[R <: JsValue, A](fetch: HttpFetch, url: String, headers: Map[String, String], rootName: String)
                                   (root: PartialFunction[JsValue, R])(parse: R => ReadOutcome[A]): ReadOutcome[A] =
    read(fetch, url, headers)(asJson(url, _, rootName)(root)(parse))

  private def asJson[R <: JsValue, A](url: String, body: String, rootName: String)
                                     (root: PartialFunction[JsValue, R])(parse: R => ReadOutcome[A]): ReadOutcome[A] =
    ChallengePage.detect(body) match {
      case Some(challenge) => unexpected(url, s"a $challenge challenge page where JSON was expected", body)
      case None =>
        val parsed = try Right(Json.parse(body)) catch { case NonFatal(e) => Left(e) }
        parsed match {
          case Left(e)                           => unexpected(url, s"not JSON (${e.getClass.getSimpleName})", body)
          case Right(value) if root.isDefinedAt(value) => parse(root(value))
          case Right(value)                      => unexpected(url, s"JSON root is not $rootName", value.toString)
        }
    }

  /** Fetch, then run `check` on the body; a parser that throws is an unexpected body. */
  private def read[A](fetch: HttpFetch, url: String, headers: Map[String, String])
                     (check: String => ReadOutcome[A]): ReadOutcome[A] =
    ReadOutcome.of(if (headers.isEmpty) fetch.get(url) else fetch.get(url, headers)) match {
      case ReadOutcome.Answered(body) => guarded(url, body)(check)
      case notAnswered: ReadOutcome.Absent => notAnswered
      case notAnswered: ReadOutcome.Failed => notAnswered
    }

  private def guarded[A](url: String, body: String)(check: String => ReadOutcome[A]): ReadOutcome[A] =
    try check(body)
    catch {
      case e: UnexpectedBodyException => ReadOutcome.Failed(ReadFailure.UnexpectedBody(e))
      // A parser that itself fetched (a follow-up page) failed on the wire, not on this body.
      case e @ (_: HttpStatusException | _: java.io.IOException) => ReadOutcome.fromFailure(e)
      case NonFatal(e) => unexpected(url, s"the parser threw ${e.getClass.getSimpleName}: ${e.getMessage}", body)
    }

  private def unexpected(url: String, why: String, body: String): ReadOutcome[Nothing] =
    ReadOutcome.unexpectedBody(url, why, body)
}

/**
 * The bot-protection interstitials a CDN serves with a 2xx (or that a proxy relays as
 * one) in place of the page. Matched on markers only the interstitial carries: a normal
 * page behind Cloudflare also loads `/cdn-cgi/challenge-platform/` as a beacon, so that
 * path alone is NOT a challenge (several real fixtures carry it).
 */
object ChallengePage {
  private val Signatures: Seq[(String, String)] = Seq(
    "window._cf_chl_opt"                  -> "Cloudflare",
    "<title>Just a moment...</title>"     -> "Cloudflare",
    "Attention Required! | Cloudflare"    -> "Cloudflare",
    "cf-browser-verification"             -> "Cloudflare",
    "captcha-delivery.com"                -> "DataDome",
    "_Incapsula_Resource"                 -> "Incapsula",
    "px-captcha"                          -> "PerimeterX"
  )

  /** The vendor whose challenge this body is, if it is one. */
  def detect(body: String): Option[String] =
    Signatures.collectFirst { case (marker, vendor) if body.contains(marker) => vendor }
}
