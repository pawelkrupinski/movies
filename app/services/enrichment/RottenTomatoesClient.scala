package services.enrichment

import org.jsoup.Jsoup
import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.text.Normalizer
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Tries to resolve a film title to its canonical Rotten Tomatoes page URL and,
 * once resolved, scrape the Tomatometer (critic) percentage off that page.
 *
 * RT's slug convention is `/m/<lowercase_underscored_title>`. Returns None
 * for both URL and score when no canonical page can be located — callers must
 * NOT substitute a search URL: search URLs are unstable, get cached for years,
 * and the view layer synthesises one on the fly for display when
 * `rottenTomatoesUrl` is None.
 *
 * Resolution order in `urlFor` (mirrors `MetacriticClient`):
 *   1. Slug probe on the primary title (and de-articled variant — RT often
 *      drops the leading "the"/"a"/"an", e.g. The Sting → /m/sting).
 *   2. Slug probe on the `fallback` title (e.g. cleanTitle when TMDB's
 *      `original_title` slugs poorly — Japanese, Cyrillic, wrong-language).
 *   3. Last resort: scrape RT's `/search?search={title}` HTML and pick the
 *      best `/m/{slug}` link by exact-title match (year as tie-breaker).
 *      Same conservative bar as MC — partial matches lose to None.
 *
 * `scoreFor(url)` GETs the canonical movie page and parses the schema.org
 * `aggregateRating.ratingValue` out of the embedded
 * `<script type="application/ld+json">` block. That JSON is RT's published
 * structured-data signal for the Tomatometer percentage and is more stable
 * than scraping the visual scoreboard markup.
 */
class RottenTomatoesClient(http: HttpFetch = new RealHttpFetch()) {
  import RottenTomatoesClient._

  /** Canonical movie page (validated 200) or None. `year` is the FILM's
   *  release year — used by the search-scrape fallback to score candidates.
   *  Pass TMDB's `releaseYear`, not the cinema's `releaseYear` (the cinema
   *  reports the *screening* year, often a 2020+ anniversary date for a film
   *  actually released decades earlier). */
  def urlFor(
    title:    String,
    fallback: Option[String] = None,
    year:     Option[Int]    = None
  ): Option[String] = {
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    canonicalUrl(title, year)
      .orElse(effectiveFallback.flatMap(canonicalUrl(_, year)))
      .orElse(searchAndPickBest(title, year))
      .orElse(effectiveFallback.flatMap(t => searchAndPickBest(t, year)))
  }

  /** Canonical URL ONLY if any candidate returns 200; otherwise None. RT
   *  frequently drops the leading "the"/"a"/"an" article (e.g. The Sting is
   *  at /m/sting, not /m/the_sting), so we try the de-articled variant as a
   *  second probe before giving up.
   *
   *  When `year` is provided we try the `slug_year` variant BEFORE the plain
   *  slug. RT uses year-suffix disambiguation aggressively: for newer films,
   *  /m/<slug> often points to a stub or older film while /m/<slug>_<year>
   *  carries the actual Tomatometer (regression: "A Private Life" 2025 —
   *  /m/a_private_life is a stub, /m/a_private_life_2025 is the real page).
   */
  def canonicalUrl(title: String, year: Option[Int] = None): Option[String] =
    candidateSlugs(title, year).iterator
      .map(s => s"$Site/m/$s")
      .find(url => Try(http.get(url)).isSuccess)

  def candidateSlugs(title: String, year: Option[Int] = None): Seq[String] = {
    val primary = RottenTomatoesClient.slugify(title)
    // Empty slug (CJK / Cyrillic / etc.) would probe `/m/` which on RT is the
    // movies landing page — a 200 that would corrupt the stored URL.
    if (primary.isEmpty) Seq.empty
    else {
      val deArticled = MetacriticClient.dropLeadingArticle(primary, '_')
      // Order: year-suffixed first (when year given), then plain, then
      // year-suffixed de-articled, then plain de-articled.
      val baseSlugs = primary +: deArticled.toSeq
      year match {
        case Some(y) => baseSlugs.flatMap(s => Seq(s"${s}_$y", s)).distinct
        case None    => baseSlugs
      }
    }
  }

  /** Scrape RT's HTML search page and pick the best `/m/{slug}` link by
   *  title + year. Returns None when the request fails or no candidate scores
   *  well enough.
   */
  def searchAndPickBest(title: String, year: Option[Int]): Option[String] = {
    if (title.trim.isEmpty) return None
    val encoded = URLEncoder.encode(title, StandardCharsets.UTF_8)
    val searchUrl = s"$Site/search?search=$encoded"
    Try(http.get(searchUrl)).toOption.flatMap { html =>
      val hits = parseSearchResults(html)
      pickBestSearchHit(hits, title, year).map(h => s"$Site/m/${h.slug}")
    }
  }

  /** Parse RT search results out of the HTML. Each result is a
   *  `<search-page-media-row release-year="…" tomatometer-score="…">` custom
   *  element containing a child `<a data-qa="info-name" href=".../m/{slug}">`
   *  with the title text. Cards without a `/m/{slug}` link (RT mixes
   *  celebrities + TV elsewhere) are skipped. */
  def parseSearchResults(html: String): Seq[SearchHit] = {
    val doc = Jsoup.parse(html)
    doc.select("search-page-media-row").asScala.toSeq.flatMap { row =>
      val link = row.select("a[data-qa=info-name]").asScala.headOption
      val href = link.map(_.attr("href")).getOrElse("")
      val slug = MoviePathSlug.findFirstMatchIn(href).map(_.group(1))
      val title = link.map(_.text().trim).getOrElse("")
      val year = Try(row.attr("release-year").toInt).toOption
      val score = Try(row.attr("tomatometer-score").toInt).toOption.filter(s => s >= 0 && s <= 100)
      slug.filter(_.nonEmpty).filter(_ => title.nonEmpty).map(s => SearchHit(s, title, year, score))
    }
  }

  /** Pick the best search hit conservatively. Same two acceptance bars as MC:
   *
   *   1. **Exact title match** (case-insensitive, whitespace-trimmed).
   *   2. **Modifier-suffix match** — title starts with the query and the next
   *      non-space char is a separator (`-`, `:`, `(`, `[`, …), catching
   *      re-release / restored / anniversary variants while rejecting
   *      different films that merely share a leading word.
   *
   *  We deliberately do NOT fall back to year-distance over arbitrary partial
   *  matches: RT search returns many unrelated films sharing a word — picking
   *  the year-closest among them produces confident-looking but wrong URLs.
   *  Better to store None and let the view layer synthesise a search link.
   *
   *  Among accepted candidates, year-distance to the requested year breaks
   *  ties.
   */
  def pickBestSearchHit(
    hits:  Seq[SearchHit],
    query: String,
    year:  Option[Int]
  ): Option[SearchHit] = {
    val q = query.toLowerCase.trim
    if (hits.isEmpty || q.isEmpty) None
    else {
      val exact = hits.filter(_.title.toLowerCase.trim == q)
      val modifier = hits.filter(h => MetacriticClient.isModifierSuffix(h.title, q))
      val candidates =
        if (exact.nonEmpty) exact
        else if (modifier.nonEmpty) modifier
        else Seq.empty
      candidates
        .sortBy(h => year.flatMap(y => h.year.map(hy => math.abs(hy - y))).getOrElse(Int.MaxValue))
        .headOption
    }
  }

  /** Tomatometer percentage for a canonical /m/ page, or None on miss /
   *  fetch failure / non-canonical URL. Refuses search URLs explicitly —
   *  scoring a search-result page makes no sense and would silently return
   *  nonsense if RT ever started embedding aggregate ratings there. */
  def scoreFor(url: String): Option[Int] = {
    if (!url.contains("/m/")) None
    else Try(http.get(url)).toOption.flatMap(parseScore)
  }

  /** Extract the Tomatometer percentage from RT's `<script type="application/
   *  ld+json">` block. The block is a `Movie` schema.org object with an
   *  `aggregateRating.ratingValue` string ("94" for The Dark Knight). Out-of-
   *  range or non-numeric values return None so we never persist garbage. */
  def parseScore(html: String): Option[Int] = {
    val doc = Jsoup.parse(html)
    doc.select("script[type=application/ld+json]").asScala.iterator
      .map(_.data())
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>
          (js \ "aggregateRating" \ "ratingValue").asOpt[JsValue].flatMap {
            case JsString(s) => Try(s.toInt).toOption
            case JsNumber(n) => Try(n.toInt).toOption
            case _           => None
          }
        }
      }
      .find(score => score >= 0 && score <= 100)
  }
}

object RottenTomatoesClient {
  private val Site          = "https://www.rottentomatoes.com"
  private val MoviePathSlug = "/m/([^/?#]+)".r

  case class SearchHit(slug: String, title: String, year: Option[Int], tomatometerScore: Option[Int])

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
