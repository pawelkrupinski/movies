package services.enrichment

import org.jsoup.Jsoup
import play.api.libs.json.{JsValue, Json}
import tools.{HttpFetch, RealHttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.text.Normalizer
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Tries to resolve a film title to its canonical Metacritic page URL.
 * Metacritic's slug convention is `/movie/<lowercase-hyphenated-title>/`.
 * Returns None when no probe + scrape strategy yields a canonical URL —
 * callers must NOT substitute a search URL: search URLs are unstable, get
 * cached for years, and the view layer synthesises one on the fly for display
 * when `metacriticUrl` is None.
 *
 * Resolution order in `urlFor`:
 *   1. Slug probe on the primary title (and de-articled variant).
 *   2. Slug probe on the `fallback` title (e.g. cleanTitle when TMDB's
 *      original_title slugs poorly — Japanese, Cyrillic, wrong-language).
 *   3. Last resort: scrape MC's `/search/{title}/?category=2` HTML and pick
 *      the best `/movie/{slug}` link by title + year. Necessary for films
 *      whose canonical slug deviates from MC's published convention (subtitle
 *      stripped, year suffix appended, etc.).
 */
class MetacriticClient(http: HttpFetch = new RealHttpFetch()) {
  import MetacriticClient._

  /** Canonical movie page (validated 200) or None. `year` is the FILM's
   *  release year — used by the search-scrape fallback to score candidates.
   *  Pass TMDB's `releaseYear` rather than the cinema's `releaseYear` field
   *  (the cinema reports the *screening* year, often a 2020+ anniversary
   *  date for a film actually released decades earlier). */
  def urlFor(
    title:    String,
    fallback: Option[String] = None,
    year:     Option[Int]    = None
  ): Option[String] = {
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    canonicalUrl(title)
      .orElse(effectiveFallback.flatMap(canonicalUrl))
      .orElse(searchAndPickBest(title, year))
      .orElse(effectiveFallback.flatMap(t => searchAndPickBest(t, year)))
  }

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
    if (primary.isEmpty) Seq.empty
    else primary +: MetacriticClient.dropLeadingArticle(primary, '-').toSeq
  }

  /** Scrape MC's HTML search page and pick the best `/movie/{slug}` link by
   *  title + year. Returns None when the request fails or no candidate scores
   *  well enough. Useful when MC's canonical slug doesn't match `slugify`'s
   *  prediction — e.g. "Alienoid: The Return to the Future" lives at
   *  /movie/alienoid (subtitle stripped); "Annie (2014)" disambiguates with
   *  a year suffix.
   */
  def searchAndPickBest(title: String, year: Option[Int]): Option[String] = {
    if (title.trim.isEmpty) return None
    val encoded = URLEncoder.encode(title, StandardCharsets.UTF_8)
    val searchUrl = s"$Site/search/$encoded/?category=2"
    Try(http.get(searchUrl)).toOption.flatMap { html =>
      val hits = parseSearchResults(html)
      pickBestSearchHit(hits, title, year).map(h => s"$Site/movie/${h.slug}")
    }
  }

  /** Parse MC search results out of the HTML. Each result is a
   *  `<a class="c-search-item search-item__content" href="/movie/{slug}/">`
   *  containing a `<p class="c-search-item__title">` and a release date string
   *  (e.g. "May 27, 2022") from which we extract the year. */
  def parseSearchResults(html: String): Seq[SearchHit] = {
    val doc = Jsoup.parse(html)
    doc.select("a.c-search-item.search-item__content").asScala.toSeq.flatMap { a =>
      val href = a.attr("href")
      val slug = if (href.startsWith("/movie/")) Some(href.stripPrefix("/movie/").stripSuffix("/")) else None
      val title = a.select("p.c-search-item__title").text().trim
      // Year appears inside the card as part of a date like "May 27, 2022".
      // Take the first 4-digit 19xx/20xx run we find.
      val year = YearRegex.findFirstIn(a.text()).map(_.toInt)
      slug.filter(_.nonEmpty).filter(_ => title.nonEmpty).map(s => SearchHit(s, title, year))
    }
  }

  /** Pick the best search hit conservatively. Two acceptance bars:
   *
   *   1. **Exact title match** (case-insensitive, whitespace-trimmed) — the
   *      query equals the candidate's title.
   *   2. **Modifier-suffix match** — the candidate's title starts with the
   *      query AND the very next non-space character is a separator
   *      (`-`, `:`, `(`, `[`, …), not another word. This catches re-release /
   *      restoration / anniversary variants ("I Vitelloni - Re-Release",
   *      "La Dolce Vita - Re-Release") while rejecting different films that
   *      merely begin with the same word ("Deaf President Now!" for query
   *      "Deaf", "La Grande Strada Azzurra" for "La Grande Arche").
   *
   *  We deliberately do NOT fall back to year-distance over arbitrary
   *  partial matches: MC search returns many unrelated films sharing a word
   *  or two, and picking the year-closest among them produces confident-
   *  looking but wrong URLs. Better to store None.
   *
   *  Among the accepted candidates, year-distance to the requested year
   *  breaks ties.
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

  // ── Metascore (critic aggregate score) ────────────────────────────────────

  /** Fetch the Metascore for an already-resolved MC movie page URL. Returns
   *  None when MC has no aggregated score yet ("tbd"), when the page can't
   *  be fetched, or when the JSON-LD doesn't include `aggregateRating`.
   *
   *  MC publishes the score in a `<script type="application/ld+json">`
   *  containing `aggregateRating.ratingValue` (0–100). Scraping that is
   *  far more stable than the visual HTML — the score block's CSS classes
   *  drift across redesigns. */
  def metascoreFor(movieUrl: String): Option[Int] =
    Try(http.get(movieUrl)).toOption.flatMap(MetacriticClient.parseMetascore)
}

object MetacriticClient {
  private val Site = "https://www.metacritic.com"
  private val YearRegex = "\\b(19\\d{2}|20\\d{2})\\b".r

  case class SearchHit(slug: String, title: String, year: Option[Int])

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

  /** Extract the Metascore (critic aggregate, 0–100) from a Metacritic movie
   *  page's HTML. Reads the `<script type="application/ld+json">` block,
   *  parses it, and returns `aggregateRating.ratingValue` as `Option[Int]`.
   *  Returns None when MC hasn't aggregated a score yet (the JSON-LD
   *  omits `aggregateRating`) or when parsing fails. */
  def parseMetascore(html: String): Option[Int] = {
    val doc     = Jsoup.parse(html)
    val scripts = doc.select("script[type=application/ld+json]").asScala
    scripts.iterator.flatMap { script =>
      Try(Json.parse(script.data())).toOption.iterator.flatMap { json =>
        (json \ "aggregateRating" \ "ratingValue").asOpt[JsValue].flatMap(extractInt)
      }
    }.toSeq.headOption
  }

  // ratingValue is usually an Int but JSON-LD spec allows string. Accept both.
  private def extractInt(v: JsValue): Option[Int] =
    v.asOpt[Int].orElse(v.asOpt[String].flatMap(s => Try(s.toInt).toOption))

  /** True when `title` starts with `query` and the *next* non-space character
   *  is punctuation — indicating a modifier suffix like " - Re-Release",
   *  ": Restored", " (Anniversary Edition)". False for "Deaf President Now!"
   *  vs "Deaf" (next char "P" is alphanumeric → different film), and for
   *  exact equals (caller treats those separately).
   *
   *  `query` is expected pre-lowercased + trimmed.
   */
  def isModifierSuffix(title: String, query: String): Boolean = {
    val t = title.toLowerCase.trim
    t.startsWith(query) && t != query && {
      val rest = t.drop(query.length).dropWhile(_.isWhitespace)
      rest.headOption.exists(c => !c.isLetterOrDigit)
    }
  }
}
