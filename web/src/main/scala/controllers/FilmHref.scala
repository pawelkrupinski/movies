package controllers

import models.City

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.annotation.targetName

/** Build the `/{city}/film?title=…` URL used to deep-link a single film page
 *  from anywhere in the app — the main repertoire list, per-cinema page, debug
 *  view, og:url meta tag in the film page itself. Centralised here so the
 *  encoding rule lives in one place; previously inlined in three templates
 *  with `URLEncoder.encode(title, "UTF-8")` repeated verbatim.
 *
 *  The city comes in implicitly so call sites in city-scoped templates (which
 *  carry an implicit `City`) read `FilmHref(title)` unchanged. The explicit
 *  overload exists for the debug page, which lists the global corpus and must
 *  deep-link each row into a city the film actually plays in (the /film page is
 *  city-scoped) rather than the city the debug page is served under. */
object FilmHref {
  def apply(title: String)(implicit city: City): String = apply(title, city)

  @targetName("applyForCity")
  def apply(title: String, city: City): String =
    s"/${city.slug}/film?title=${encodeTitle(title)}"

  /** The server-rendered Open Graph card image (1200×630 PNG) for a film,
   *  emitted as `og:image` / `twitter:image`. Same city-scoped, `%20`-encoded
   *  form as [[apply]] so Facebook's scraper follows it — only the path
   *  segment differs. */
  def ogImage(title: String)(implicit city: City): String =
    s"/${city.slug}/film/og-image?title=${encodeTitle(title)}"

  // `URLEncoder.encode` is form-urlencoded (spaces → `+`). Browsers accept
  // both in query strings, but some link-preview scrapers (Facebook's among
  // them) flag `+` as "URL malformed" and refuse to follow. Swap to the RFC
  // 3986 form (`%20`) so the canonical URL we emit as og:url / og:image and
  // every `<a href>` in the app round-trips cleanly through every crawler.
  private def encodeTitle(title: String): String =
    URLEncoder.encode(title, StandardCharsets.UTF_8).replace("+", "%20")
}
