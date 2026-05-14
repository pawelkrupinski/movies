package services.enrichment

import tools.{HttpFetch, RealHttpFetch}

import java.text.Normalizer
import scala.util.Try

/**
 * Tries to resolve a film title to its canonical Rotten Tomatoes page URL.
 * RT's slug convention is `/m/<lowercase_underscored_title>`. Returns None
 * when no candidate slug 200s — callers must NOT substitute a search URL:
 * search URLs are unstable, get cached for years, and the view layer
 * synthesises one on the fly for display when `rottenTomatoesUrl` is None.
 *
 * RT's slug rules are less predictable than Metacritic's (some pages drop
 * separators entirely, others append year suffixes for disambiguation), so
 * expect a higher None rate.
 */
class RottenTomatoesClient(http: HttpFetch = new RealHttpFetch()) {
  import RottenTomatoesClient._

  /** Canonical movie page (validated 200) or None. */
  def urlFor(title: String): Option[String] = canonicalUrl(title)

  /** Canonical URL ONLY if any candidate returns 200; otherwise None. RT
   *  frequently drops the leading "the"/"a"/"an" article (e.g. The Sting is
   *  at /m/sting, not /m/the_sting), so we try the de-articled variant as a
   *  second probe before giving up.
   */
  def canonicalUrl(title: String): Option[String] =
    candidateSlugs(title).iterator
      .map(s => s"$Site/m/$s")
      .find(url => Try(http.get(url)).isSuccess)

  def candidateSlugs(title: String): Seq[String] = {
    val primary = RottenTomatoesClient.slugify(title)
    // Empty slug (CJK / Cyrillic / etc.) would probe `/m/` which on RT is the
    // movies landing page — a 200 that would corrupt the stored URL.
    if (primary.isEmpty) Seq.empty
    else primary +: MetacriticClient.dropLeadingArticle(primary, '_').toSeq
  }
}

object RottenTomatoesClient {
  private val Site = "https://www.rottentomatoes.com"

  /**
   * RT-style slug: lowercase, accents stripped, apostrophes dropped, all
   * other non-alphanumerics collapsed to a single underscore.
   */
  def slugify(title: String): String = {
    val unaccented = Normalizer.normalize(title, Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
      .replace('ł', 'l').replace('Ł', 'l')
    unaccented.toLowerCase
      .replaceAll("[''']", "")
      .replaceAll("[^a-z0-9]+", "_")
      .replaceAll("^_+|_+$", "")
  }
}
