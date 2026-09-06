package services.enrichment

import org.jsoup.Jsoup
import services.enrichment.scraping.{JsonLdAggregateRating, RottenTomatoesScorecard}
import services.resolution.TitleMatch
import tools.{EnrichmentRead, HttpFetch, MemoizedHttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
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
 * A probed slug is REJECTED when its page release year conflicts with the
 * film's (shared [[MetacriticClient.yearsCompatible]] tolerance), so a bare
 * slug that 200s onto an unrelated same-named film from another decade is not
 * stored — the RT twin of MC's "Michael"/"The North" collisions.
 *
 * `scoreFor(url)` GETs the canonical movie page and parses the Tomatometer
 * percentage out of the embedded `media-scorecard-json` data island
 * (`criticsScore.score`), falling back to the schema.org
 * `aggregateRating.ratingValue` in the `<script type="application/ld+json">`
 * block for pages that still publish it — RT removed the JSON-LD rating from
 * most pages, leaving the scorecard island as the reliable signal.
 */
class RottenTomatoesClient(http: HttpFetch) {
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
  ): Option[String] = urlForAny(Seq(title), fallback, year)

  /** Resolve across several candidate titles, best-first, as ONE attempt.
   *
   *  `RottenTomatoesRatings` has three titles to try — TMDB's original title,
   *  its en-US title, and the US release title — and used to run [[urlFor]]
   *  once per title, so slugs an earlier title had already probed were probed
   *  again against the live site ("The Sting" and "Sting" both end at
   *  `/m/sting`). Threading all the titles through one call lets the whole
   *  attempt share a [[MemoizedHttpFetch]], which fetches each URL at most
   *  once.
   *
   *  The order is exactly what the chained [[urlFor]] calls produced — each
   *  title's full ladder in turn, `fallback` belonging to the first title — so
   *  this removes repeat requests without changing which page wins. */
  def urlForAny(
    titles:   Seq[String],
    fallback: Option[String] = None,
    year:     Option[Int]    = None
  ): Option[String] = {
    val attempt = new RottenTomatoesClient(new MemoizedHttpFetch(http))
    titles.iterator.zipWithIndex
      .flatMap { case (title, index) => attempt.probeLadder(title, if (index == 0) fallback else None, year) }
      .nextOption()
  }

  /** One title's ladder: its slug probes, the fallback's slug probes, then the
   *  search-page scrape for each. Private because the memo that makes the
   *  ladder cheap lives in [[urlForAny]] — calling this directly would bypass
   *  it. */
  private def probeLadder(title: String, fallback: Option[String], year: Option[Int]): Option[String] = {
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    canonicalUrl(title, year)
      .orElse(effectiveFallback.flatMap(canonicalUrl(_, year)))
      .orElse(searchAndPickBest(title, year))
      .orElse(effectiveFallback.flatMap(t => searchAndPickBest(t, year)))
  }

  /** Canonical URL ONLY if any candidate returns 200 AND its page year is
   *  compatible with the film's; otherwise None. RT frequently drops the
   *  leading "the"/"a"/"an" article (e.g. The Sting is at /m/sting, not
   *  /m/the_sting), so we try the de-articled variant as a second probe before
   *  giving up.
   *
   *  When `year` is provided we try the `slug_year` variant BEFORE the plain
   *  slug. RT uses year-suffix disambiguation aggressively: for newer films,
   *  /m/<slug> often points to a stub or older film while /m/<slug>_<year>
   *  carries the actual Tomatometer (regression: "A Private Life" 2025 —
   *  /m/a_private_life is a stub, /m/a_private_life_2025 is the real page).
   *
   *  The year check ([[MetacriticClient.yearsCompatible]], shared with MC)
   *  rejects a plain slug that 200s but points to an unrelated same-named film
   *  a different decade apart — the RT twin of the "Michael"/"The North" MC
   *  collisions. The year-suffixed variants already encode the right year, so
   *  the guard is a no-op for them and only bites the bare slug. */
  def canonicalUrl(title: String, year: Option[Int] = None): Option[String] =
    candidateSlugs(title, year).iterator
      .map(s => s"$Site/m/$s")
      .flatMap(url => EnrichmentRead.absentOnNotFound(http.get(url)).map(body => (url, body)))
      .find { case (_, body) => MetacriticClient.yearsCompatible(year, RottenTomatoesClient.parseReleaseYear(body)) }
      .map(_._1)

  def candidateSlugs(title: String, year: Option[Int] = None): Seq[String] = {
    val primary = RottenTomatoesClient.slugify(title)
    // Empty slug (CJK / Cyrillic / etc.) would probe `/m/` which on RT is the
    // movies landing page — a 200 that would corrupt the stored URL.
    if (primary.isEmpty) Seq.empty
    else {
      // Year-suffixed form first, then plain, per form — the shared ordering
      // rule (see TitleMatch.yearSuffixedFirst), which MC needs too.
      TitleMatch.yearSuffixedFirst(
        primary +: TitleMatch.dropLeadingArticle(primary, '_').toSeq, year, '_')
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
    EnrichmentRead.absentOnNotFound(http.get(searchUrl)).flatMap { html =>
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
    val document = Jsoup.parse(html)
    document.select("search-page-media-row").asScala.toSeq.flatMap { row =>
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
    val normalizedQuery = TitleMatch.fold(query)
    if (hits.isEmpty || normalizedQuery.isEmpty) None
    else {
      // Year-guard the EXACT matches, and require the hit to POSITIVELY agree —
      // see `MetacriticClient.yearConfirms`. This picker had no year guard at
      // all, so an exact same-title hit won on its title alone: that is how the
      // 2026 "Lalka" ended up on /m/lalka_1969, Has's 1968 film. A MODIFIER-suffix
      // hit ("<title> - Re-Release") is guarded only against a year that positively
      // names the ORIGINAL film — a re-issue's own gap from its film is expected and
      // correct, and an undated one says nothing. See the fallback below for the case
      // that leaves open and why no year test here can close it.
      val exactTitle = hits.filter(h => TitleMatch.exact(h.title, normalizedQuery))
      // Strict, INCLUDING the undated: "a hit that declines to say WHEN is not evidence
      // that it is the right film" — /m/lalka_1969 publishes no year on the page either,
      // which is exactly how the leak below reached prod. That rejection is deliberate
      // and its own test; do not relax it to rescue a modifier fallback.
      val exact      = exactTitle.filter(h => MetacriticClient.yearConfirms(year, h.year))
      val modifier   = hits.filter(h => TitleMatch.isModifierSuffix(h.title, normalizedQuery))
      // The modifier branch is a fallback for "no exact title here at all" — NOT for
      // "the exact titles were rejected". Falling through on a year rejection hands an
      // unguarded candidate the win the guard just denied a guarded one: a 2026 "Lalka"
      // whose exact hit carries no year would land on the 1968 ":  Restored" re-issue,
      // which is the very page the year guard was added to stop. An exact title that
      // fails the year means this film is not here.
      // The modifier fallback is for "RT has no exact title at all", never for "the
      // exact titles were rejected": a "- Restored" hit can carry the RE-RELEASE year and
      // so confirm against a query whose film it is not, which is why it must not be able
      // to rescue an exact title the year already rejected. An exact title that positively
      // disagreed means this film is not here.
      // A modifier hit dated at the ORIGINAL film's year is that film: RT lists
      // "Lalka - Restored" (1968) and nothing else, and handing it back for a 2026 query
      // is the /m/lalka_1969 leak by another door. Undated still passes — a re-issue that
      // declines to say when is what this arm is for.
      //
      // WHAT THIS CANNOT CLOSE, and no year test at this layer can: a modifier dated at
      // the RE-RELEASE year. "Top Gun" 2024 against "Top Gun - Re-Release" (2024) must be
      // ACCEPTED — it is the same film re-issued — and "Lalka" 2026 against
      // "Lalka - Restored" (2026) must be REJECTED, because that page is the 1968 film.
      // The two search results are identical in shape; only the PAGE could tell them
      // apart, and /m/lalka_1969 publishes no year either, which is how the leak reached
      // prod in the first place. Both cases are pinned by tests, so anyone tightening
      // this will see immediately which one they broke.
      val datedPlausibly = modifier.filter(h => h.year.isEmpty || MetacriticClient.yearConfirms(year, h.year))
      val candidates =
        if (exact.nonEmpty)          exact
        else if (exactTitle.isEmpty) datedPlausibly
        else                         Seq.empty
      candidates
        .sortBy(h => year.flatMap(y => h.year.map(hy => math.abs(hy - y))).getOrElse(Int.MaxValue))
        .headOption
    }
  }

  /** Tomatometer percentage for a canonical /m/ page, or None on miss /
   *  fetch failure / non-canonical URL. Refuses search URLs explicitly —
   *  scoring a search-result page makes no sense and would silently return
   *  nonsense if RT ever started embedding aggregate ratings there. */
  def scoreFor(url: String): Option[Int] = scoreAndYearFor(url).flatMap(_._1)

  /** The Tomatometer AND the year the page claims, off ONE fetch. The year is
   *  what tells a caller that a STORED url isn't this film's after all —
   *  re-resolution can't say so, because it writes only when it finds something
   *  and a wrong url therefore just survives. See `RottenTomatoesRatings`. */
  def scoreAndYearFor(url: String): Option[(Option[Int], Option[Int])] = {
    if (!url.contains("/m/")) None
    else Try(http.get(url)).toOption.map(body => (parseScore(body), RottenTomatoesClient.parseReleaseYear(body)))
  }

  /** Extract the Tomatometer percentage off an RT movie page.
   *
   *  RT dropped `aggregateRating.ratingValue` from the JSON-LD on most pages,
   *  so the primary signal is now the `media-scorecard-json` data island's
   *  `criticsScore.score` ("94" for The Dark Knight); the schema.org
   *  `aggregateRating.ratingValue` is the fallback for pages that still carry
   *  it. Out-of-range or non-numeric values return None so we never persist
   *  garbage. */
  def parseScore(html: String): Option[Int] =
    RottenTomatoesScorecard.criticsScore(html)
      .orElse(JsonLdAggregateRating.parseInt(html))
      .filter(score => score >= 0 && score <= 100)
}

object RottenTomatoesClient {
  private val Site          = "https://www.rottentomatoes.com"
  private val MoviePathSlug = "/m/([^/?#]+)".r

  // RT dropped JSON-LD `datePublished`. Its hydration JSON carries the film's
  // ORIGIN year as `"releaseYear":"1974"` — the only field to trust. The sibling
  // `"releaseDate":"<Mon DD, YYYY>"` is the theatrical/RE-release date: for an
  // old film it's a much later restoration ("The Conversation" (1974) → "Oct 1,
  // 2011"; Wajda's "Brzezina" (1970) → "Aug 25, 2018"), so reading it would
  // wrongly reject the real film. We therefore key the guard ONLY on
  // `releaseYear`; when a page omits it (some newer films do), we report no year
  // and the guard abstains rather than guess from the unreliable `releaseDate`.
  private val ReleaseYear = "\"releaseYear\":\"((?:19|20)\\d{2})\"".r

  case class SearchHit(slug: String, title: String, year: Option[Int], tomatometerScore: Option[Int])

  /** The film's origin release year off an RT movie page (its `releaseYear`
   *  field), or None when the page carries none. Feeds
   *  [[MetacriticClient.yearsCompatible]] so a slug that 200s but points to a
   *  different same-named film is rejected; a missing `releaseYear` makes the
   *  guard abstain (we never guess off the re-release `releaseDate`). */
  def parseReleaseYear(html: String): Option[Int] =
    ReleaseYear.findFirstMatchIn(html).map(_.group(1).toInt)

  /**
   * RT-style slug: lowercase, accents stripped, apostrophes dropped, all
   * other non-alphanumerics collapsed to a single underscore.
   */
  def slugify(title: String): String =
    TextNormalization.deburr(title).toLowerCase
      .replaceAll("[''']", "")
      .replaceAll("[^a-z0-9]+", "_")
      .replaceAll("^_+|_+$", "")
}
