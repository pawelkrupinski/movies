package services.enrichment

import tools.{HttpFetch, RealHttpFetch}

import java.text.Normalizer
import scala.util.Try

/**
 * Tries to resolve a film title to its canonical Metacritic page URL.
 * Metacritic's slug convention is `/movie/<lowercase-hyphenated-title>/`.
 * Returns None when no candidate slug 200s — callers must NOT substitute a
 * search URL: search URLs are unstable, get cached for years, and the view
 * layer synthesises one on the fly for display when `metacriticUrl` is None.
 *
 * Validation is a single GET — Metacritic returns clean 200 / 404 without
 * Cloudflare interfering.
 */
class MetacriticClient(http: HttpFetch = new RealHttpFetch()) {
  import MetacriticClient._

  /** Canonical movie page (validated 200) or None.
   *
   *  When `fallback` is supplied, we try it after the primary title 404s — used
   *  by the enrichment pipeline to retry with `cleanTitle` when TMDB's
   *  `originalTitle` produces an unusable slug (Japanese / Cyrillic / etc.) or
   *  when TMDB matched the wrong film. The fallback is skipped when it matches
   *  the primary (case-insensitive).
   */
  def urlFor(title: String, fallback: Option[String] = None): Option[String] =
    canonicalUrl(title)
      .orElse(fallback.filterNot(_.equalsIgnoreCase(title)).flatMap(canonicalUrl))

  /** Canonical URL ONLY if any candidate returns 200; otherwise None. Tries
   *  the primary slug first and a leading-article-stripped variant second
   *  (some titles index without the article on Metacritic).
   */
  def canonicalUrl(title: String): Option[String] =
    candidateSlugs(title).iterator
      .map(s => s"$Site/movie/$s")
      .find(url => Try(http.get(url)).isSuccess)

  def candidateSlugs(title: String): Seq[String] = {
    val primary = MetacriticClient.slugify(title)
    // Empty slug means the title was entirely non-alphanumeric (CJK, etc.).
    // Probing `/movie/` would hit MC's movie index page (200) and the caller
    // would store a bogus URL.
    if (primary.isEmpty) Seq.empty
    else primary +: MetacriticClient.dropLeadingArticle(primary, '-').toSeq
  }
}

object MetacriticClient {
  private val Site = "https://www.metacritic.com"

  /**
   * Metacritic-style slug: lowercase, accents stripped, apostrophes dropped
   * (so "Schindler's List" → "schindlers-list"). `!` is preserved — MC keeps
   * it in slugs ("airplane!", "moulin-rouge!", "yu-gi-oh!-the-dark-side-of-
   * dimensions"). All other non-alphanumerics collapse to a single hyphen.
   */
  def slugify(title: String): String = {
    val unaccented = Normalizer.normalize(title, Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
      .replace('ł', 'l').replace('Ł', 'l')
    unaccented.toLowerCase
      .replaceAll("[''']", "")        // drop apostrophes (straight + curly)
      .replaceAll("[^a-z0-9!]+", "-") // preserve !, everything else → hyphen
      .replaceAll("^-+|-+$", "")
  }

  /** Some films index without their leading "the"/"a"/"an" (more common on
   *  RT, but happens on Metacritic too). Returns the de-articled slug only
   *  when the leading article is present, so callers can decide whether to
   *  also probe the variant.
   */
  def dropLeadingArticle(slug: String, sep: Char): Option[String] = {
    val prefixes = Seq(s"the$sep", s"a$sep", s"an$sep")
    prefixes.collectFirst { case p if slug.startsWith(p) => slug.drop(p.length) }
  }
}
