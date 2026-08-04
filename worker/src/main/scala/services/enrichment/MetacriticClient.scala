package services.enrichment

import org.jsoup.Jsoup
import services.enrichment.scraping.JsonLdAggregateRating
import tools.{EnrichmentRead, HttpFetch, MemoizedHttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

/**
 * Tries to resolve a film title to its canonical Metacritic page URL.
 * Metacritic's slug convention is `/movie/<lowercase-hyphenated-title>/`.
 * Returns None when no probe + scrape strategy yields a canonical URL —
 * callers must NOT substitute a search URL: search URLs are unstable, get
 * cached for years, and the view layer synthesises one on the fly for display
 * when `metacriticUrl` is None.
 *
 * Resolution order, per candidate title:
 *   1. Slug probe on the primary title (and de-articled variant).
 *   2. Slug probe on the `fallback` title (e.g. cleanTitle when TMDB's
 *      original_title slugs poorly — Japanese, Cyrillic, wrong-language).
 *   3. Last resort: scrape MC's `/search/{title}/?category=2` HTML and pick
 *      the best `/movie/{slug}` link by title + year. Necessary for films
 *      whose canonical slug deviates from MC's published convention (subtitle
 *      stripped, year suffix appended, etc.).
 *
 * [[resolveAcross]] runs that ladder over several candidate titles as ONE
 * attempt sharing a fetch memo — how `MetascoreRatings` tries TMDB's original,
 * English and US titles without re-probing the slugs they have in common.
 * [[requestUrl]] governs the exact URL each probe GETs.
 *
 * A probed slug is REJECTED when its page `datePublished` year conflicts with
 * the film's release year (both known, more than [[YearMatchTolerance]] apart).
 * This guards EVERY candidate — primary and de-articled — because a plain title
 * slug collides with an unrelated same-named film just as a de-articled one
 * does: "Michael" (the 2026 biopic) slugs to `/movie/michael`, which is the
 * 1996 comedy; "The North" (2026) de-articles to `/movie/north`, the 1994 film.
 * On a conflict the caller stores None and the view synthesises a search link.
 * The tolerance is wide so legitimate origin-vs-US-date drift still resolves.
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
  ): Option[Resolved] = resolveAcross(Seq(title), fallback, year)

  /** Resolve across several candidate titles, best-first, as ONE attempt.
   *
   *  `MetascoreRatings` has three titles to try — TMDB's original title, its
   *  en-US title, and the US release title — and used to run [[resolve]] once
   *  per title. Each run walked its own ladder against the live site, so slugs
   *  the earlier titles had already probed were probed again: "The Sting" and
   *  "Sting" both end at `/movie/sting`. Threading all the titles through one
   *  call lets the whole attempt share a [[MemoizedHttpFetch]], which fetches
   *  each URL at most once.
   *
   *  The order is exactly what the chained [[resolve]] calls produced — each
   *  title's full ladder in turn, `fallback` belonging to the first title — so
   *  this removes repeat requests without changing which page wins. */
  def resolveAcross(
    titles:    Seq[String],
    fallback:  Option[String] = None,
    year:      Option[Int]    = None,
    directors: Set[String]    = Set.empty
  ): Option[Resolved] = {
    val attempt = new MetacriticClient(new MemoizedHttpFetch(http))
    titles.iterator.zipWithIndex
      .flatMap { case (title, index) => attempt.probeLadder(title, if (index == 0) fallback else None, year, directors) }
      .nextOption()
  }

  /** One title's ladder: its slug probes, the fallback's slug probes, then the
   *  search-page scrape for each. Private because the memo that makes the
   *  ladder cheap lives in [[resolveAcross]] — calling this directly would
   *  bypass it. */
  private def probeLadder(title: String, fallback: Option[String], year: Option[Int], directors: Set[String]): Option[Resolved] = {
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    canonicalResolve(title, year, directors)
      .orElse(effectiveFallback.flatMap(t => canonicalResolve(t, year, directors)))
      .orElse(searchAndPickBest(title, year).flatMap(url => verified(url, directors)))
      .orElse(effectiveFallback.flatMap(t => searchAndPickBest(t, year)).flatMap(url => verified(url, directors)))
  }

  /** Read a search-derived page and keep it only if it is the right film.
   *
   *  The search path used to return its pick unread, so a page that title+year
   *  could not separate from the real one was stored unchecked — which is how
   *  Michel Franco's "Dreams" ended up on Haugerud's `/movie/dreams-drommer`,
   *  both being 2025 films of that name. Reading the page also yields its
   *  Metascore, which this path previously left as None. */
  private def verified(url: String, directors: Set[String]): Option[Resolved] =
    if (directors.isEmpty) Some(Resolved(url, None))   // nothing to check it against
    else EnrichmentRead.absentOnNotFound(http.get(MetacriticClient.requestUrl(url))) match {
      // Read and CONTRADICTED — a different film, drop it.
      case Some(body) if !MetacriticClient.directorsCompatible(directors, JsonLdAggregateRating.directorNames(body)) => None
      // Read and consistent — keep it, and take the Metascore while we have the page.
      case Some(body) => Some(Resolved(url, MetacriticClient.parseMetascore(body)))
      // Could not read it. Silence is not contradiction: the search already
      // matched this page on title and year, so keep it exactly as before rather
      // than letting an unreadable page throw a good link away.
      case None => Some(Resolved(url, None))
    }

  /** Canonical URL ONLY if any candidate returns 200; otherwise None. Tries
   *  the primary slug first and a leading-article-stripped variant second
   *  (some titles index without the article on Metacritic).
   */
  def canonicalUrl(title: String): Option[String] = canonicalResolve(title).map(_.url)

  /** Like [[canonicalUrl]] but keeps the validated page's parsed Metascore so
   *  the caller need not re-fetch the same page to read it. The first candidate
   *  slug that returns 200 AND whose page year is compatible with `year` wins;
   *  its body is parsed for `aggregateRating` on the spot (None when the page
   *  has no score yet). Lazy: a compatible 200 on the primary slug
   *  short-circuits before the de-articled variant is probed.
   *
   *  The year guard (see [[yearsCompatible]]) applies to EVERY candidate slug,
   *  primary and de-articled alike, because a plain title slug collides with an
   *  unrelated same-named film just as a de-articled one does: "Michael" (the
   *  2026 biopic) slugs to `/movie/michael`, which is the 1996 comedy; "The
   *  North" (2026) de-articles to `/movie/north`, the 1994 film. Its tolerance
   *  is wide enough to keep legitimate cross-region drift (a film's TMDB origin
   *  year vs Metacritic's later US date — "Picnic at Hanging Rock" is 1975 vs
   *  1979) while rejecting the decade-plus gaps that mark a different film. */
  def canonicalResolve(title: String, year: Option[Int] = None, directors: Set[String] = Set.empty): Option[Resolved] =
    candidateSlugs(title, year).iterator
      .flatMap { slug =>
        // 404 = "that slug isn't a film", which is what the ladder probes for, so
        // it drops through to the next candidate. A block/throttle/5xx aborts the
        // ladder instead of quietly reporting "no Metacritic page" — a failed read
        // is not an answer. See tools.EnrichmentRead.
        EnrichmentRead.absentOnNotFound(http.get(MetacriticClient.requestUrl(s"$Site/movie/$slug")))
          .filter(body => MetacriticClient.yearsCompatible(year, MetacriticClient.parseReleaseYear(body)))
          // Title and year are not always enough to name a film: Metacritic
          // carries two 2025 "Dreams", Franco's and Haugerud's, and the slug
          // probe hit whichever it hit. The page names its own director.
          .filter(body => MetacriticClient.directorsCompatible(directors, JsonLdAggregateRating.directorNames(body)))
          .map(body => Resolved(s"$Site/movie/$slug", MetacriticClient.parseMetascore(body)))
      }
      .nextOption()

  /** Slugs to probe, best-first. When the film's year is known the year-suffixed
   *  variant of each form is tried BEFORE its bare form — see
   *  [[MetacriticClient.yearSuffixedFirst]] for why that ordering is load-bearing. */
  def candidateSlugs(title: String, year: Option[Int] = None): Seq[String] = {
    val primary = MetacriticClient.slugify(title)
    if (primary.isEmpty) Seq.empty
    else MetacriticClient.yearSuffixedFirst(
      primary +: MetacriticClient.dropLeadingArticle(primary, '-').toSeq, year, '-')
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
    EnrichmentRead.absentOnNotFound(http.get(searchUrl)).flatMap { html =>
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
      // Year-guard the EXACT matches only. Same title + distant year means a
      // different film (a namesake or remake) — that is how Welles' "The Trial"
      // (1962) ended up on a 2023 page and Zulawski's "Possession" (1981) on a
      // 2008 one. This search path is where a slug REJECTED by the year guard
      // lands, so leaving it unguarded let the rejected film back in sideways.
      //
      // A MODIFIER-suffix hit ("I Vitelloni - Re-Release", "<title>: Restored")
      // is the opposite case: it is explicitly the SAME film re-issued, so a
      // large gap is EXPECTED — I Vitelloni is 1953 and its re-release entry
      // 2024, 71 years apart and correct. Guarding those would reject every
      // anniversary screening, so it deliberately stays unguarded.
      val exact = hits
        .filter(h => MetacriticClient.foldDashes(h.title.toLowerCase.trim) == normalizedQuery)
        .filter(h => MetacriticClient.yearsCompatible(year, h.year))
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
    EnrichmentRead.absentOnNotFound(http.get(MetacriticClient.requestUrl(movieUrl))).flatMap(MetacriticClient.parseMetascore)
}

object MetacriticClient {
  private val Site = "https://www.metacritic.com"

  /** The form of an MC movie URL we actually GET.
   *
   *  Metacritic 301-redirects `/movie/<slug>` to `/movie/<slug>/` — for a real
   *  page and a nonexistent one alike — so probing the slash-less form costs
   *  TWO round trips per candidate, and a failed resolution probes up to eight
   *  of them. Measured against the live site on 2026-07-30: every slash-less
   *  `/movie/<slug>` answered 301, the trailing-slash form answered directly.
   *  `RealHttpFetch` follows redirects, so the wasted hop never showed up as an
   *  error — only as latency (`McRating` p95 sat at 15-24s against `RtRating`'s
   *  2.7s).
   *
   *  We keep STORING and displaying the slash-less URL — that is what every
   *  existing row and the read model already carry — and add the slash only
   *  here, at the request boundary. That way rows written long before this
   *  change stop paying the redirect too, with no data migration. */
  private[enrichment] def requestUrl(movieUrl: String): String =
    if (movieUrl.endsWith("/")) movieUrl else s"$movieUrl/"
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

  /** Could a page naming `theirs` be the film whose directors are `ours`?
   *
   *  Silent unless BOTH sides name somebody — a page that lists no director, or a
   *  film we hold none for, is not evidence of anything and must not be rejected.
   *  Names are compared by their word tokens so ordering and punctuation don't
   *  matter ("Zhang Yimou" / "Yimou Zhang"), and one name's tokens CONTAINED in
   *  another's counts, so a co-director credit still meets a single name. */
  def directorsCompatible(ours: Set[String], theirs: Set[String]): Boolean = {
    // Fold the spelling before comparing. Rating sites and TMDB render the same
    // person differently: Metacritic writes "Ken'ichirô Akimoto" where TMDB has
    // "Kenichiro Akimoto" — an apostrophe that must not split the name in two, and
    // a circumflex that must not make it a different word. Dropping apostrophes
    // and folding diacritics first makes both read as {kenichiro, akimoto}.
    def tokens(name: String): Set[String] = {
      // `\u0142` is its own codepoint, not a base letter plus a mark, so NFD leaves it
      // alone and "Micha\u0142" would never meet "Michal" \u2014 the same special case the
      // corpus's own `TitleNormalizer` carries.
      val folded = java.text.Normalizer
        .normalize(name.toLowerCase.replace("'", "").replace("\u2019", ""), java.text.Normalizer.Form.NFD)
        .replaceAll("\\p{M}", "")
        .replace("\u0142", "l")
      folded.split("[^\\p{L}\\p{N}]+").filter(_.length >= 2).toSet
    }
    if (ours.isEmpty || theirs.isEmpty) true
    else {
      val a = ours.map(tokens).filter(_.nonEmpty)
      val b = theirs.map(tokens).filter(_.nonEmpty)
      a.isEmpty || b.isEmpty || a.exists(x => b.exists(y => x.subsetOf(y) || y.subsetOf(x)))
    }
  }

  /** How far a probed page's release year may sit from the film's before we
   *  treat it as a different film. Set generously (15y) to absorb legitimate
   *  gaps between a film's TMDB origin year and a rating site's later US /
   *  regional release date (festival premiere → wide release, foreign film →
   *  delayed US date — "Picnic at Hanging Rock" is 1975 vs Metacritic's 1979),
   *  while still catching the decade-plus gaps that mark a genuinely different
   *  same-named film ("Michael" 2026 biopic vs the 1996 comedy at the same
   *  slug; "The North" 2026 vs the 1994 film). Observed legitimate drift tops
   *  out around 4 years and observed collisions start around 26, so a threshold
   *  in the wide gap between them is what does the separating — the exact value
   *  is not delicate. Shared by [[MetacriticClient]] and [[RottenTomatoesClient]]. */
  private val YearMatchTolerance = 15

  /** True when the film's year and a probed page's year are compatible — i.e.
   *  we have NO positive evidence they're different films. Only a conflict of
   *  BOTH known years beyond [[YearMatchTolerance]] returns false; a missing
   *  year on either side is treated as compatible (we never had grounds to
   *  reject). Shared decision for Metacritic and Rotten Tomatoes slug probes. */
  /** Interleave `base` slug forms with their `<slug><sep><year>` variants, each
   *  year-suffixed form immediately BEFORE its bare form. No year → unchanged.
   *
   *  Both Metacritic and Rotten Tomatoes disambiguate same-titled films with a
   *  year suffix, and for a NEW film the bare slug is routinely the older
   *  namesake: `/movie/the-odyssey` is Jerome Salle's Cousteau biopic while
   *  Nolan's 2026 film is `/movie/the-odyssey-2026`. Probing bare-first stored
   *  the wrong film — and the year guard could not catch it, because that page
   *  serves `datePublished: "0000-00-00"`, which parses to no year at all and so
   *  is "compatible" with everything. Trying the year-suffixed form first is what
   *  actually separates them; the guard only rejects what it can disprove.
   *
   *  Shared by both clients (RT with '_', MC with '-') so the ordering rule has
   *  one definition. */
  def yearSuffixedFirst(base: Seq[String], year: Option[Int], separator: Char): Seq[String] =
    year.fold(base)(y => base.flatMap(s => Seq(s"$s$separator$y", s)).distinct)

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
