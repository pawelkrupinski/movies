package controllers

import models.City

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.annotation.targetName

/** Build the `/{city}/movie/{slug}` URL used to deep-link a single film page
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
 *  Slugs are lossy and irreversible, so a film's address is ASSIGNED over the
 *  whole corpus by [[services.readmodel.FilmSlugs]] — two genuinely different
 *  films can share a title ("Zaproszenie" 1986 and 2026), and folding each
 *  title in isolation left one of them with no address at all. Every call site
 *  that has a [[FilmSchedule]] passes the slug it was assigned ([[forSlug]]);
 *  [[apply]] re-folds the title for the few that don't (the debug page's
 *  corpus rows), which is still right for the ~99% of titles nothing collides
 *  with.
 *
 *  The city comes in implicitly so call sites in city-scoped templates (which
 *  carry an implicit `City`) read `FilmHref(title)` unchanged. The explicit
 *  overload exists for the debug page, which lists the global corpus and must
 *  deep-link each row into a city the film actually plays in (the /movie page is
 *  city-scoped) rather than the city the debug page is served under. */
object FilmHref {
  def apply(title: String)(implicit city: City): String = apply(title, city)

  @targetName("applyForCity")
  def apply(title: String, city: City): String = forSlug(slugOf(title), title, city)

  /** The URL for a film whose address was already assigned. `slug` is `None`
   *  for a title that folds to nothing addressable, which falls back to the
   *  query form exactly as [[apply]] does. */
  def forSlug(slug: Option[String], title: String)(implicit city: City): String = forSlug(slug, title, city)

  @targetName("forSlugInCity")
  def forSlug(slug: Option[String], title: String, city: City): String =
    slug.fold(legacy(title, city))(s => s"${CityPath(city)}/movie/$s")

  /** The title's URL slug, or `None` when it folds to nothing addressable (a
   *  title that is entirely punctuation, or in a script the fold doesn't cover).
   *  Callers that must not emit an empty path segment branch on this — the
   *  legacy redirect in particular, which would otherwise 301 to itself. */
  def slugOf(title: String): Option[String] = Option(tools.Slugify(title)).filter(_.nonEmpty)

  /** The pre-slug query form. Still served (301 → the slug address) so links
   *  minted before the switch keep resolving, and still the only address for a
   *  title with no usable slug. */
  def legacy(title: String, city: City): String =
    s"${CityPath(city)}/movie?title=${encodeTitle(title)}"

  /** The server-rendered Open Graph card image (1200×630 PNG) for a film,
   *  emitted as `og:image` / `twitter:image`. Stays on the `%20`-encoded query
   *  form rather than following the page to a slug: the card is an asset, not
   *  an indexable page, so it gains nothing from a readable address, and
   *  keeping the URL stable means the previews Facebook and friends have
   *  already cached don't all miss at once. */
  def ogImage(title: String)(implicit city: City): String =
    s"${CityPath(city)}/movie/og-image?title=${encodeTitle(title)}"

  // `URLEncoder.encode` is form-urlencoded (spaces → `+`). Browsers accept
  // both in query strings, but some link-preview scrapers (Facebook's among
  // them) flag `+` as "URL malformed" and refuse to follow. Swap to the RFC
  // 3986 form (`%20`) so the canonical URL we emit as og:url / og:image and
  // every `<a href>` in the app round-trips cleanly through every crawler.
  private def encodeTitle(title: String): String =
    URLEncoder.encode(title, StandardCharsets.UTF_8).replace("+", "%20")
}
