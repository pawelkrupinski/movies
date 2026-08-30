package controllers

import models.City

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.annotation.targetName

/** Build the `/{city}/film/{slug}` URL used to deep-link a single film page
 *  from anywhere in the app — the main repertoire list, per-cinema page, debug
 *  view, og:url meta tag in the film page itself, sitemap. Centralised here so
 *  the addressing rule lives in one place; previously inlined in three
 *  templates with `URLEncoder.encode(title, "UTF-8")` repeated verbatim.
 *
 *  The slug is the canonical address. The older `?title=…` query form ([[legacy]])
 *  is still routed — old links, shared URLs, and installed app builds carry it —
 *  but `MovieController.film` answers it with a 301 to the slug so search
 *  engines consolidate on one address per film.
 *
 *  Slugs are lossy, so they are resolved by RE-SLUGGING the titles a city is
 *  showing and comparing (`MovieControllerService.filmBySlug`), never by
 *  reversing the fold. Two distinct titles can therefore share one slug; the
 *  resolver breaks that tie deterministically.
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
    slugOf(title).fold(legacy(title, city))(slug => s"${prefix(city)}/${city.slug}/film/$slug")

  /** Where the deployment serving `city` is MOUNTED — empty for a country that
   *  owns its domain (`kinowo.net/poznan/…`), a country segment for one that
   *  shares the brand domain (`showtimes.cc/uk/kent/…`).
   *
   *  Taken from the CITY rather than from the router's published prefix or the
   *  process environment, because it is the same fact read from the one thing
   *  these URLs are already scoped by: a deployment only ever links to cities of
   *  the country it serves, so the city's country IS the mount point, and the
   *  addressing rule stays pure and testable for every country at once. */
  private def prefix(city: City): String = city.country.pathPrefix

  /** The title's URL slug, or `None` when it folds to nothing addressable (a
   *  title that is entirely punctuation, or in a script the fold doesn't cover).
   *  Callers that must not emit an empty path segment branch on this — the
   *  legacy redirect in particular, which would otherwise 301 to itself. */
  def slugOf(title: String): Option[String] = Option(tools.Slugify(title)).filter(_.nonEmpty)

  /** The pre-slug query form. Still served (301 → the slug address) so links
   *  minted before the switch keep resolving, and still the only address for a
   *  title with no usable slug. */
  def legacy(title: String, city: City): String =
    s"${prefix(city)}/${city.slug}/film?title=${encodeTitle(title)}"

  /** The server-rendered Open Graph card image (1200×630 PNG) for a film,
   *  emitted as `og:image` / `twitter:image`. Stays on the `%20`-encoded query
   *  form rather than following the page to a slug: the card is an asset, not
   *  an indexable page, so it gains nothing from a readable address, and
   *  keeping the URL stable means the previews Facebook and friends have
   *  already cached don't all miss at once. */
  def ogImage(title: String)(implicit city: City): String =
    s"${prefix(city)}/${city.slug}/film/og-image?title=${encodeTitle(title)}"

  // `URLEncoder.encode` is form-urlencoded (spaces → `+`). Browsers accept
  // both in query strings, but some link-preview scrapers (Facebook's among
  // them) flag `+` as "URL malformed" and refuse to follow. Swap to the RFC
  // 3986 form (`%20`) so the canonical URL we emit as og:url / og:image and
  // every `<a href>` in the app round-trips cleanly through every crawler.
  private def encodeTitle(title: String): String =
    URLEncoder.encode(title, StandardCharsets.UTF_8).replace("+", "%20")
}
