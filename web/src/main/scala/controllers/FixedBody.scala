package controllers

import org.apache.pekko.util.ByteString
import play.api.mvc.Results.{NotModified, Ok}
import play.api.mvc.{RequestHeader, Result}

import java.nio.charset.StandardCharsets

/**
 * A response body fixed for the life of the process — the apps' catalog, their
 * client-support minimums, a country's picker fragment — served under a content
 * ETag with conditional GET.
 *
 * Everything that depends only on the bytes is worked out ONCE, here: the ETag
 * ([[tools.Digest.contentEtag]], so a body's identity is its content and the same
 * bytes on every pod carry the same tag) and the gzipped copy. A request then costs
 * a header compare and, for the rare client refusing gzip, an encode of `text`.
 * The plain bytes are not held: `text` already is, and most callers keep it anyway
 * (`models.Catalog.json`, the landing's own-country picker).
 */
final class FixedBody(val text: String, val contentType: String) {

  val etag: String = tools.Digest.contentEtag(text)

  /** The ETag without its quotes — fit for a URL, which is where a versioned link
   *  to this body carries it. */
  def version: String = etag.stripPrefix("\"").stripSuffix("\"")

  val gzipped: ByteString = ResponseBody.text(text).gzipped

  /** What this value keeps on the heap beyond `text` itself, which its owner holds
   *  regardless: the gzipped copy. */
  def heldBytes: Long = gzipped.length.toLong

  /**
   * This body for `request`: a bodiless 304 when its `If-None-Match` offers our tag
   * ([[ConditionalResponse.offersValidator]] — lists and weak tags included), else a
   * 200, gzipped when the client takes it. `Content-Encoding` is set here, which is
   * also what keeps Play's `GzipFilter` from compressing it a second time.
   *
   * `cacheControl` is the caller's policy — `no-cache` for a URL whose content moves
   * between deploys, a long `immutable` for one whose URL names the content.
   */
  def serve(request: RequestHeader, cacheControl: String): Result = {
    // The 304 repeats the 200's `Vary` (RFC 9110 §15.4.5): one tag covers both
    // representations, so a cache must know the response varies by encoding.
    val validators = Seq("ETag" -> etag, "Cache-Control" -> cacheControl, "Vary" -> "Accept-Encoding")
    if (ConditionalResponse.offersValidator(request.headers.get("If-None-Match"), etag))
      NotModified.withHeaders(validators*)
    else if (AcceptEncoding.acceptsGzip(request.headers.get("Accept-Encoding")))
      Ok(gzipped).as(contentType).withHeaders(("Content-Encoding" -> "gzip") +: validators*)
    else
      Ok(ByteString.fromString(text, StandardCharsets.UTF_8)).as(contentType).withHeaders(validators*)
  }
}

object FixedBody {
  def json(text: String): FixedBody = new FixedBody(text, "application/json")
  def html(text: String): FixedBody = new FixedBody(text, "text/html; charset=utf-8")

  /** The apps' policy: revalidate on every use — the URL never changes, so only a
   *  conditional GET can learn that its content did. */
  val Revalidate = "no-cache"
}
