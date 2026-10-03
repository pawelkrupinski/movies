package controllers

import play.api.libs.json.{JsString, Writes}

/** A URL vetted to become a link or an image source on our pages, in the JSON API or in
 *  the JSON-LD: an http(s) address, spelled so from its first character.
 *
 *  Booking, cinema-page, rating and poster URLs come off other sites, and a `javascript:`
 *  (or `data:`, or ` javascript:` — browsers strip the leading space) one in an `href`
 *  would run as our origin when clicked; in the apps a custom-scheme or `intent:` URL
 *  would open another app. The constructor is private, so the only way to hold one is
 *  [[WebHref.of]]: a template or API field typed `WebHref` cannot be handed an unvetted
 *  string, and `RenderSafetyLintSpec` makes every data-driven `href`/`src` in a template
 *  go through one.
 *
 *  Renders as its URL (`toString`), so `href="@href"` escapes it like any other value. */
final class WebHref private (val url: String) extends AnyVal {
  override def toString: String = url

  /** The same address with every character outside the strict URL grammar
   *  percent-encoded ([[tools.AsciiUrl]]) — what the mobile decoders need. `AsciiUrl`
   *  leaves the scheme's ASCII alone, so the result is still http(s). */
  def asciiEncoded: WebHref = new WebHref(tools.AsciiUrl.encode(url))
}

object WebHref {

  def of(url: String): Option[WebHref] = Option.when(accepts(url))(new WebHref(url))

  /** Whether `url` may become a [[WebHref]]: http(s), from its first character. */
  def accepts(url: String): Boolean =
    url.regionMatches(true, 0, "https://", 0, 8) || url.regionMatches(true, 0, "http://", 0, 7)

  implicit val writes: Writes[WebHref] = Writes(href => JsString(href.url))
}
