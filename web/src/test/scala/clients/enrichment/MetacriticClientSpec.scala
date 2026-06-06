package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.MetacriticClient
import tools.GetOnlyHttpFetch

class MetacriticClientSpec extends AnyFlatSpec with Matchers {

  "slugify" should "lowercase + hyphenate a plain title" in {
    MetacriticClient.slugify("The Dark Knight") shouldBe "the-dark-knight"
  }

  it should "drop apostrophes rather than splitting on them" in {
    // Metacritic's actual slug for this film is "schindlers-list", not
    // "schindler-s-list" — apostrophes are stripped, not separated.
    MetacriticClient.slugify("Schindler's List") shouldBe "schindlers-list"
  }

  it should "strip accents (NFD) and Polish ł" in {
    MetacriticClient.slugify("Diabeł ubiera się u Prady 2") shouldBe "diabel-ubiera-sie-u-prady-2"
  }

  it should "treat colons and other punctuation as separators" in {
    MetacriticClient.slugify("Spider-Man: Across the Spider-Verse") shouldBe
      "spider-man-across-the-spider-verse"
  }

  it should "collapse runs of separators and trim edges (preserving !)" in {
    // Note: ! is now preserved per MC's actual slug rule. Trailing whitespace
    // and the double-! both collapse but the ! stays.
    MetacriticClient.slugify("  Top   Gun!! ") shouldBe "top-gun!!"
  }

  // Regression: Metacritic keeps `!` in slugs ("airplane!", "moulin-rouge!",
  // "yu-gi-oh!-the-dark-side-of-dimensions"). Stripping it produces 404s.
  it should "preserve '!' in slugs" in {
    MetacriticClient.slugify("Yu-Gi-Oh! The Dark Side of Dimensions") shouldBe
      "yu-gi-oh!-the-dark-side-of-dimensions"
    MetacriticClient.slugify("Airplane!")    shouldBe "airplane!"
    MetacriticClient.slugify("Moulin Rouge!") shouldBe "moulin-rouge!"
  }

  // Stub fetch that throws for any URL containing one of the 404 fragments.
  private def stub(notFound: Set[String]) = new GetOnlyHttpFetch {
    def get(url: String): String =
      if (notFound.exists(url.contains)) throw new RuntimeException("HTTP 404")
      else "OK"
  }

  "canonicalUrl" should "return Some when the canonical URL responds 200" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.canonicalUrl("The Dark Knight") shouldBe
      Some("https://www.metacritic.com/movie/the-dark-knight")
  }

  it should "return None when the canonical URL 404s" in {
    val c = new MetacriticClient(stub(Set("/movie/")))
    c.canonicalUrl("Foo Bar") shouldBe None
  }

  "urlFor" should "return None when canonical 404s (search URLs are never persisted)" in {
    val c = new MetacriticClient(stub(Set("/movie/")))
    c.urlFor("Foo Bar") shouldBe None
  }

  it should "return canonical when validated" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.urlFor("The Dark Knight") shouldBe
      Some("https://www.metacritic.com/movie/the-dark-knight")
  }

  it should "try the slug with 'the-' stripped when the primary 404s" in {
    val c = new MetacriticClient(stub(Set("/movie/the-dark-knight")))
    c.urlFor("The Dark Knight") shouldBe
      Some("https://www.metacritic.com/movie/dark-knight")
  }

  // Regression: TMDB's `original_title` for CJK / Cyrillic films is in the
  // production-language script. slugify strips everything non-Latin and
  // collapses to "". Without this guard, `canonicalUrl` would probe
  // `/movie/` (MC's movie index, status 200) and store that as the canonical
  // URL — so every CJK-original film got the same bogus link in production.
  "candidateSlugs" should "return empty when the title slugs to an empty string (CJK / Cyrillic only)" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.candidateSlugs("")               shouldBe empty
    c.candidateSlugs("大紅燈籠高高掛") shouldBe empty
    c.candidateSlugs("遊☆戯☆王")     shouldBe empty
  }

  it should "return the primary slug plus de-articled variant when an article is present" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.candidateSlugs("The Sting") shouldBe Seq("the-sting", "sting")
  }

  it should "return just the primary slug when no article is present" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.candidateSlugs("Inception") shouldBe Seq("inception")
  }

  // Regression: same bug surfaced via canonicalUrl rather than candidateSlugs.
  "canonicalUrl" should "return None when the title slugs to empty (bogus /movie/ probe protection)" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.canonicalUrl("大紅燈籠高高掛") shouldBe None
  }

  // New: cleanTitle fallback. TMDB's `original_title` is the production-
  // language title which may not match MC's English slug — pass the row's
  // cleanTitle as a fallback so re-enrichment can recover.
  "urlFor with fallback" should "use the fallback when the primary 404s but the fallback resolves" in {
    // Primary "Foreign Title" 404s and has no leading article (so the
    // de-articled variant isn't tried); the fallback Polish-aware title
    // resolves to the canonical page.
    val c = new MetacriticClient(stub(Set("/movie/foreign-title")))
    c.urlFor("Foreign Title", Some("Raise the Red Lantern")) shouldBe
      Some("https://www.metacritic.com/movie/raise-the-red-lantern")
  }

  it should "prefer the primary canonical when it resolves (fallback is ignored)" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.urlFor("Inception", Some("Wrong Title")) shouldBe
      Some("https://www.metacritic.com/movie/inception")
  }

  it should "skip the fallback when it matches the primary case-insensitively (no duplicate probe)" in {
    // If we accidentally probe the same URL twice, the assertion will still
    // pass — but the call would waste a network round-trip. The behavioural
    // assertion is just: same canonical out as the no-fallback form.
    val c = new MetacriticClient(stub(Set.empty))
    c.urlFor("Inception", Some("INCEPTION")) shouldBe
      Some("https://www.metacritic.com/movie/inception")
  }

  it should "return None when neither primary nor fallback resolve and the search page is empty" in {
    // Stub 404s every probe AND returns an empty search page → no fallback path.
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else "<html><body></body></html>"  // search page with no items
    })
    c.urlFor("foo", Some("bar")) shouldBe None
  }

  // ── Search-page scrape (last-resort fallback) ─────────────────────────────
  //
  // The captured fixture (test/resources/fixtures/metacritic/search_top_gun.html)
  // is a real MC search response trimmed to the first 5 result cards. Layout:
  //   <a class="c-search-item search-item__content" href="/movie/{slug}/">
  //     <p class="c-search-item__title">{title}</p>
  //     … release date string (e.g. "May 27, 2022") with year embedded …
  //   </a>

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  private val SearchTopGunFixture = "/fixtures/metacritic/search_top_gun.html"

  "parseSearchResults" should "extract (slug, title, year) for every search-item card in MC's HTML" in {
    val c = new MetacriticClient(stub(Set.empty))
    val hits = c.parseSearchResults(loadFixture(SearchTopGunFixture))
    hits.map(_.slug)  shouldBe Seq("top-gun-maverick", "top-gun", "top-five", "the-old-man-the-gun", "topsy-turvy")
    hits.map(_.title) shouldBe Seq("Top Gun: Maverick", "Top Gun", "Top Five", "The Old Man & the Gun", "Topsy-Turvy")
    hits.map(_.year)  shouldBe Seq(Some(2022), Some(1986), Some(2014), Some(2018), Some(1999))
  }

  "pickBestSearchHit" should "prefer an exact-title match over partial matches" in {
    val c = new MetacriticClient(stub(Set.empty))
    val hits = c.parseSearchResults(loadFixture(SearchTopGunFixture))
    // Querying "Top Gun" with no year hint: "Top Gun" (1986) is the only exact
    // match; "Top Gun: Maverick" is partial and must lose despite its later year.
    c.pickBestSearchHit(hits, "Top Gun", None).map(_.slug) shouldBe Some("top-gun")
  }

  it should "accept titles that start with the query when the next char is a separator (re-release / restored)" in {
    val c = new MetacriticClient(stub(Set.empty))
    // Real MC pattern: anniversary screenings live under "<title> - Re-Release"
    // slugs. Acceptable because the next non-space char after the query is "-".
    val hits = Seq(
      MetacriticClient.SearchHit("i-vitelloni-re-release", "I Vitelloni - Re-Release", Some(2024)),
      MetacriticClient.SearchHit("la-grande-strada-azzurra", "La Grande Strada Azzurra", Some(1957))
    )
    c.pickBestSearchHit(hits, "I Vitelloni", Some(1953)).map(_.slug) shouldBe Some("i-vitelloni-re-release")
  }

  // Regression: a previous looser policy accepted any title that *started*
  // with the query, so "Deaf President Now!" was returned for query "Deaf"
  // (different film entirely — they just share a first word). The modifier-
  // suffix rule rejects this because the next non-space char after "Deaf" is
  // "P" (alphanumeric → continuation of a new word, not a modifier).
  it should "reject prefix matches where the next char is alphanumeric (different film, not a modifier)" in {
    val c = new MetacriticClient(stub(Set.empty))
    val hits = Seq(
      MetacriticClient.SearchHit("deaf-president-now!", "Deaf President Now!", Some(2024))
    )
    c.pickBestSearchHit(hits, "Deaf", None) shouldBe None
  }

  // Regression: without this guard, querying "L'Inconnu de la Grande Arche"
  // returned "La Grande Strada Azzurra" (year-closest partial match) because
  // the picker fell back to ANY hit when no exact/prefix match existed. Real
  // MC search results often contain unrelated films sharing a word — we must
  // return None rather than guess.
  it should "return None when nothing exact-matches or modifier-suffix-matches the query (no wild guessing)" in {
    val c = new MetacriticClient(stub(Set.empty))
    val hits = c.parseSearchResults(loadFixture(SearchTopGunFixture))
    // None of the fixture entries begin with "Inconnu de la Grande Arche".
    c.pickBestSearchHit(hits, "Inconnu de la Grande Arche", Some(2025)) shouldBe None
  }

  it should "return None for an empty hit list" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.pickBestSearchHit(Seq.empty, "anything", None) shouldBe None
  }

  // urlFor's full chain falls through to search when every slug probe 404s.
  "urlFor" should "fall through to the search-page scrape when slug probes 404" in {
    val fixture = loadFixture(SearchTopGunFixture)
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else if (url.contains("/search/")) fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    // Primary slug "top-gun" 404s; the search scrape sees Top Gun (1986) as
    // the only exact-title match and returns it.
    c.urlFor("Top Gun") shouldBe Some("https://www.metacritic.com/movie/top-gun")
  }

  it should "pass year through so re-release tie-breaks work" in {
    // Hand-crafted scenario: two re-release entries for the same canonical
    // title, different years. Year=2024 should pick the 2024 re-release.
    val html =
      """<html><body>
        |<a href="/movie/i-vitelloni-re-release/" class="c-search-item search-item__content">
        |  <p class="c-search-item__title">I Vitelloni - Re-Release</p>
        |  <div>Apr 12, 2024</div>
        |</a>
        |<a href="/movie/i-vitelloni-restored/" class="c-search-item search-item__content">
        |  <p class="c-search-item__title">I Vitelloni - Restored</p>
        |  <div>Mar 03, 2018</div>
        |</a>
        |</body></html>""".stripMargin
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else if (url.contains("/search/")) html
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("I Vitelloni", year = Some(2024)) shouldBe
      Some("https://www.metacritic.com/movie/i-vitelloni-re-release")
    c.urlFor("I Vitelloni", year = Some(2018)) shouldBe
      Some("https://www.metacritic.com/movie/i-vitelloni-restored")
  }

  // ── Metascore parsing ─────────────────────────────────────────────────────
  //
  // MC publishes the critic Metascore (0–100) in a JSON-LD block on every
  // movie page: `<script type="application/ld+json">{… "aggregateRating":
  // {"ratingValue":85, …}}</script>`. Fixtures are trimmed real MC pages
  // (just the head + the JSON-LD block) captured live.

  private val MovieWithMetascoreFixture    = "/fixtures/metacritic/movie_the_dark_knight.html"
  private val MovieWithoutMetascoreFixture = "/fixtures/metacritic/movie_girl_climber.html"

  "parseMetascore" should "read aggregateRating.ratingValue from a real MC movie page" in {
    // The Dark Knight: MC Metascore 85 at time of capture.
    MetacriticClient.parseMetascore(loadFixture(MovieWithMetascoreFixture)) shouldBe Some(85)
  }

  it should "return None when MC hasn't aggregated a score (no aggregateRating in JSON-LD)" in {
    // Girl Climber: 2025 indie, no critic consensus yet — JSON-LD omits
    // `aggregateRating` entirely.
    MetacriticClient.parseMetascore(loadFixture(MovieWithoutMetascoreFixture)) shouldBe None
  }

  it should "return None for HTML that has no JSON-LD script (e.g. a 404 page)" in {
    MetacriticClient.parseMetascore("<html><body>Page not found</body></html>") shouldBe None
  }

  it should "return None when the JSON-LD has aggregateRating but ratingValue is missing" in {
    val html =
      """<html><head><script type="application/ld+json">{
        |"@type":"Movie","aggregateRating":{"@type":"AggregateRating","reviewCount":3}
        |}</script></head><body></body></html>""".stripMargin
    MetacriticClient.parseMetascore(html) shouldBe None
  }

  "metascoreFor" should "fetch the URL and return the parsed score" in {
    val fixture = loadFixture(MovieWithMetascoreFixture)
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url == "https://www.metacritic.com/movie/the-dark-knight") fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.metascoreFor("https://www.metacritic.com/movie/the-dark-knight") shouldBe Some(85)
  }

  it should "return None when the HTTP fetch fails" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String = throw new RuntimeException("HTTP 404")
    })
    c.metascoreFor("https://www.metacritic.com/movie/whatever") shouldBe None
  }
}
