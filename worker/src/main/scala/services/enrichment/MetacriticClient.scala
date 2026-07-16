package services.enrichment

import org.jsoup.Jsoup
import services.enrichment.scraping.JsonLdAggregateRating
import tools.{HttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
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
 *
 * The DE-ARTICLED slug variant is REJECTED when its `datePublished` year
 * conflicts with the film's release year (both known, differing by more than a
 * year). Without this guard a de-articled slug collides with an unrelated
 * same-named film: "The North" (2026) has no `/movie/the-north` page, so the
 * de-articled variant probes `/movie/north` — Rob Reiner's 1994 "North" — and
 * 200s. The year check catches that and lets the caller store None (view
 * synthesises a search link). The primary full-title slug is NOT year-checked:
 * legitimate films drift several years between their origin release and MC's US
 * date, and the full title is strong enough evidence on its own.
 */
class MetacriticClient(http: HttpFetch) {
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
  ): Option[String] = resolve(title, fallback, year).map(_.url)

  /** Like [[urlFor]] but also carries the page's Metascore WHEN the resolving
   *  fetch already had the movie page in hand — the slug-probe path GETs the
   *  movie page to validate it (200), so it parses the score from that same
   *  body and the caller can skip a redundant second GET to read the score.
   *
   *  `metascore` is None when the URL was reached via the search-scrape
   *  fallback (which fetched the SEARCH page, not the movie page) or when the
   *  movie page carried no aggregated score — in both cases the caller must
   *  read the score separately via [[metascoreFor]]. */
  def resolve(
    title:    String,
    fallback: Option[String] = None,
    year:     Option[Int]    = None
  ): Option[Resolved] = {
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    canonicalResolve(title, year)
      .orElse(effectiveFallback.flatMap(t => canonicalResolve(t, year)))
      .orElse(searchAndPickBest(title, year).map(Resolved(_, None)))
      .orElse(effectiveFallback.flatMap(t => searchAndPickBest(t, year)).map(Resolved(_, None)))
  }

  /** Canonical URL ONLY if any candidate returns 200; otherwise None. Tries
   *  the primary slug first and a leading-article-stripped variant second
   *  (some titles index without the article on Metacritic).
   */
  def canonicalUrl(title: String): Option[String] = canonicalResolve(title).map(_.url)

  /** Like [[canonicalUrl]] but keeps the validated page's parsed Metascore so
   *  the caller need not re-fetch the same page to read it. The first candidate
   *  slug that returns 200 (and, for the de-articled variant, whose page year
   *  is compatible with `year`) wins; its body is parsed for `aggregateRating`
   *  on the spot (None when the page has no score yet). Lazy: a 200 on the
   *  primary slug short-circuits before the de-articled variant is probed.
   *
   *  The year guard applies ONLY to the de-articled variant. The primary
   *  full-title slug is strong evidence, so it's accepted on a 200 regardless
   *  of year — real films drift by several years between a festival/origin
   *  release (what TMDB dates) and Metacritic's US date ("Picnic at Hanging
   *  Rock" is 1975 on TMDB, 1979 on MC), and rejecting those would drop good
   *  links. The de-articled slug is a WEAK guess — a bare common word ("north",
   *  "stranger") collides with unrelated films far too easily — so we accept it
   *  only when the page's `datePublished` year corroborates the film (or either
   *  year is unknown). That's what stops "The North" (2026) from matching the
   *  de-articled `/movie/north` (Rob Reiner's 1994 film). */
  def canonicalResolve(title: String, year: Option[Int] = None): Option[Resolved] =
    candidateSlugs(title).iterator.zipWithIndex
      .flatMap { case (slug, idx) =>
        // idx 0 is the primary full-title slug (no year guard); later slugs are
        // de-articled variants that must clear the year check.
        val requireYear = if (idx == 0) None else year
        Try(http.get(s"$Site/movie/$slug")).toOption
          .filter(body => MetacriticClient.yearsCompatible(requireYear, MetacriticClient.parseReleaseYear(body)))
          .map(body => Resolved(s"$Site/movie/$slug", MetacriticClient.parseMetascore(body)))
      }
      .nextOption()

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
    val document = Jsoup.parse(html)
    document.select("a.c-search-item.search-item__content").asScala.toSeq.flatMap { a =>
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
    val normalizedQuery = MetacriticClient.foldDashes(query.toLowerCase.trim)
    if (hits.isEmpty || normalizedQuery.isEmpty) None
    else {
      val exact = hits.filter(h => MetacriticClient.foldDashes(h.title.toLowerCase.trim) == normalizedQuery)
      val modifier = hits.filter(h => MetacriticClient.isModifierSuffix(h.title, normalizedQuery))
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

  // Unicode dash variants (hyphen-minus aside): hyphen, non-breaking hyphen,
  // figure dash, en dash, em dash, horizontal bar, minus sign. Cinemas and the
  // rating sources disagree on which one a title uses ("Chainsaw Man – The
  // Movie" vs "Chainsaw Man - The Movie"), so fold them all to ASCII '-' before
  // comparing titles. Shared across the title matchers of MC/RT (search-hit
  // acceptance), Filmweb (`normalizeTitle`), and IMDb (suggestion-title
  // disambiguation) so one rule governs dash equivalence everywhere.
  private val DashVariants: Set[Char] = Set('‐', '‑', '‒', '–', '—', '―', '−')

  /** Fold every Unicode dash variant in `s` to ASCII '-'. Case- and
   *  diacritic-preserving — callers lowercase/deburr separately. */
  private[enrichment] def foldDashes(s: String): String =
    if (s.exists(DashVariants)) s.map(c => if (DashVariants(c)) '-' else c) else s

  /** A resolved Metacritic movie page, plus its Metascore when the resolving
   *  fetch already downloaded the movie page (the slug probe validates the page
   *  with a GET, so its body yields the score for free). `metascore` is None
   *  when the page was reached via the search fallback or carried no score —
   *  see [[MetacriticClient.resolve]]. */
  case class Resolved(url: String, metascore: Option[Int])

  /**
   * Metacritic-style slug: lowercase, accents stripped, apostrophes dropped
   * (so "Schindler's List" → "schindlers-list"). `!` is preserved — MC keeps
   * it in slugs ("airplane!", "moulin-rouge!", "yu-gi-oh!-the-dark-side-of-
   * dimensions"). All other non-alphanumerics collapse to a single hyphen.
   */
  def slugify(title: String): String =
    TextNormalization.deburr(title).toLowerCase
      .replaceAll("[''']", "")        // drop apostrophes (straight + curly)
      .replaceAll("[^a-z0-9!]+", "-") // preserve !, everything else → hyphen
      .replaceAll("^-+|-+$", "")

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
  def parseMetascore(html: String): Option[Int] = JsonLdAggregateRating.parseInt(html)

  /** The film's release year off an MC movie page's JSON-LD `datePublished`.
   *  Used only to reject a probed page whose year contradicts the film we're
   *  resolving — see [[yearsCompatible]]. */
  def parseReleaseYear(html: String): Option[Int] = JsonLdAggregateRating.datePublishedYear(html)

  /** How far a probed page's release year may sit from the film's before we
   *  treat it as a different film. 1 absorbs legitimate cross-region release
   *  drift (a film premiering late one year, opening early the next) without
   *  admitting an unrelated same-named film decades apart. */
  private val YearMatchTolerance = 1

  /** True when the film's year and a probed page's year are compatible — i.e.
   *  we have NO positive evidence they're different films. Only a conflict of
   *  BOTH known years beyond [[YearMatchTolerance]] returns false; a missing
   *  year on either side is treated as compatible (we never had grounds to
   *  reject). */
  def yearsCompatible(filmYear: Option[Int], pageYear: Option[Int]): Boolean =
    (filmYear, pageYear) match {
      case (Some(f), Some(p)) => math.abs(f - p) <= YearMatchTolerance
      case _                  => true
    }

  /** True when `title` starts with `query` and the *next* non-space character
   *  is punctuation — indicating a modifier suffix like " - Re-Release",
   *  ": Restored", " (Anniversary Edition)". False for "Deaf President Now!"
   *  vs "Deaf" (next char "P" is alphanumeric → different film), and for
   *  exact equals (caller treats those separately).
   *
   *  `query` is expected pre-lowercased + trimmed. Both sides are dash-folded
   *  so an en-dash title still prefix-matches a hyphen query (and vice versa).
   */
  def isModifierSuffix(title: String, query: String): Boolean = {
    val normalizedQuery = foldDashes(query)
    val normalizedTitle = foldDashes(title.toLowerCase.trim)
    normalizedTitle.startsWith(normalizedQuery) && normalizedTitle != normalizedQuery && {
      val rest = normalizedTitle.drop(normalizedQuery.length).dropWhile(_.isWhitespace)
      rest.headOption.exists(c => !c.isLetterOrDigit)
    }
  }
}
