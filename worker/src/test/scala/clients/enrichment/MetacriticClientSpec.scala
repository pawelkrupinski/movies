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

  // A minimal MC movie page: JSON-LD carrying a name, release date and score,
  // exactly the fields the resolver reads to validate a probed page.
  private def moviePage(name: String, year: Int, score: Int): String =
    s"""<html><head><script type="application/ld+json">{
       |"@type":"Movie","name":"$name","datePublished":"$year-07-22",
       |"aggregateRating":{"@type":"AggregateRating","ratingValue":$score}
       |}</script></head><body></body></html>""".stripMargin

  /** A real MC shape for a film with no release date: `datePublished` is the
   *  literal "0000-00-00", which yields NO parseable year — so the year guard
   *  treats the page as compatible with any film's year. */
  private def undatedMoviePage(name: String): String =
    s"""<html><head><script type="application/ld+json">{
       |"@type":"Movie","name":"$name","datePublished":"0000-00-00"
       |}</script></head><body></body></html>""".stripMargin

  // Regression: "The North" (2026) has no /movie/the-north page, so the
  // de-articled variant probes /movie/north — Rob Reiner's 1994 "North", which
  // 200s. Without the year guard that unrelated film was stored as the link.
  // With the film's year known and conflicting, the probe is rejected and
  // (search empty) the whole resolve returns None.
  "urlFor with a year" should "reject a de-articled slug whose page year conflicts with the film's" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/the-north")) throw new RuntimeException("HTTP 404")
        else if (url.endsWith("/movie/north")) moviePage("North", 1994, 33)
        else if (url.contains("/search/")) "<html><body></body></html>"
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("The North", year = Some(2026)) shouldBe None
  }

  it should "accept a de-articled slug whose page year matches the film's" in {
    // Same probe shape, but now the film really is the 1994 "North" — the year
    // agrees, so the de-articled slug is accepted.
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/the-north")) throw new RuntimeException("HTTP 404")
        else if (url.endsWith("/movie/north")) moviePage("North", 1994, 33)
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("The North", year = Some(1994)) shouldBe
      Some("https://www.metacritic.com/movie/north")
  }

  it should "accept the primary full-title slug when its page year drifts within tolerance (cross-region release)" in {
    // The year guard's tolerance is wide enough to keep legitimate drift: "Picnic
    // at Hanging Rock" is 1975 on TMDB (Australian release) but 1979 on Metacritic
    // (US release) — the same film, 4 years apart, well inside tolerance.
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/picnic-at-hanging-rock")) moviePage("Picnic at Hanging Rock", 1979, 81)
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("Picnic at Hanging Rock", year = Some(1975)) shouldBe
      Some("https://www.metacritic.com/movie/picnic-at-hanging-rock")
  }

  // Regression: "Michael" (the 2026 biopic) slugs to /movie/michael — but that
  // page is the 1996 John Travolta comedy. This is a PRIMARY-slug collision (no
  // article to drop), so the guard must cover the primary slug too, not just the
  // de-articled variant. 30 years apart → rejected → search fallback empty → None.
  it should "reject the primary slug when its page year conflicts by decades (same-name different film)" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/michael")) moviePage("Michael", 1996, 38)
        else if (url.contains("/search/")) "<html><body></body></html>"
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("Michael", year = Some(2026)) shouldBe None
  }

  it should "accept a probed page when the film year is unknown (no grounds to reject)" in {
    // No year passed → the year guard never fires; behaviour matches the
    // pre-guard resolver.
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/north")) moviePage("North", 1994, 33)
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("North") shouldBe Some("https://www.metacritic.com/movie/north")
  }

  // Regression: TMDB's `original_title` for CJK / Cyrillic films is in the
  // production-language script. slugify strips everything non-Latin and
  // collapses to "". Without this guard, `canonicalUrl` would probe
  // `/movie/` (MC's movie index, status 200) and store that as the canonical
  // URL — so every CJK-original film got the same bogus link in production.
  // Regression, from prod (2026-07-19): Nolan's "The Odyssey" (2026). Metacritic
  // disambiguates same-titled films with a `-<year>` slug suffix, exactly as RT
  // does with `_<year>`. `/movie/the-odyssey` is Jerome Salle's Cousteau biopic
  // and — crucially — serves `datePublished: "0000-00-00"`, which parses to NO
  // year, so `yearsCompatible` had no grounds to reject it and we stored the
  // wrong film's page. The real film lives at `/movie/the-odyssey-2026` with a
  // Metascore of 89, and nothing in the ladder ever probed that slug.
  "urlFor with a year" should "prefer the year-suffixed slug, which Metacritic uses to disambiguate same-titled films" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/the-odyssey-2026")) moviePage("The Odyssey", 2026, 89)
        // The wrong film at the bare slug: no parseable year (MC's placeholder
        // for a film with no release date), so the year guard cannot reject it.
        else if (url.endsWith("/movie/the-odyssey")) undatedMoviePage("The Odyssey")
        else throw new RuntimeException(s"unexpected URL: $url")
    })

    c.urlFor("The Odyssey", year = Some(2026)) shouldBe
      Some("https://www.metacritic.com/movie/the-odyssey-2026")
    c.resolve("The Odyssey", year = Some(2026)).flatMap(_.metascore) shouldBe Some(89)
  }

  it should "fall back to the bare slug when no year-suffixed page exists" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.endsWith("/movie/the-dark-knight-2008")) throw new RuntimeException("HTTP 404")
        else if (url.endsWith("/movie/the-dark-knight")) moviePage("The Dark Knight", 2008, 84)
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.urlFor("The Dark Knight", year = Some(2008)) shouldBe
      Some("https://www.metacritic.com/movie/the-dark-knight")
  }

  "candidateSlugs with a year" should "put each year-suffixed variant ahead of its bare form" in {
    val c = new MetacriticClient(stub(Set.empty))
    c.candidateSlugs("The Odyssey", Some(2026)) shouldBe
      Seq("the-odyssey-2026", "the-odyssey", "odyssey-2026", "odyssey")
    c.candidateSlugs("The Odyssey") shouldBe Seq("the-odyssey", "odyssey")
  }

  // Regression, from prod (2026-07-19): Welles' "The Trial" (1962) was stored as
  // /movie/the-trial-el-juicio, a 2023 film — 61 years off. The slug probe DID
  // reject it on year, but rejection falls through to the search scrape, which
  // sorted candidates by year distance and then took the head regardless of how
  // far away it was. So the year guard pushed work onto an unguarded route.
  // Same shape put Zulawski's "Possession" (1981) on a 2008 page.
  "search fallback" should "reject an exact-title hit whose year is beyond tolerance, not merely rank it last" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else searchPage(Seq(("the-trial-el-juicio", "The Trial", 2023)))
    })
    c.urlFor("The Trial", year = Some(1962)) shouldBe None
  }

  it should "still accept an exact-title hit inside tolerance" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else searchPage(Seq(("the-trial", "The Trial", 1963)))
    })
    c.urlFor("The Trial", year = Some(1962)) shouldBe
      Some("https://www.metacritic.com/movie/the-trial")
  }

  it should "keep an undated hit — an unknown year is not grounds to reject" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else searchPage(Seq(("some-film", "Some Film", 0)))
    })
    c.urlFor("Some Film", year = Some(2026)) shouldBe
      Some("https://www.metacritic.com/movie/some-film")
  }

  it should "prefer the closest year among several compatible hits" in {
    val c = new MetacriticClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else searchPage(Seq(("far", "Twins", 2035), ("near", "Twins", 2027)))
    })
    c.urlFor("Twins", year = Some(2026)) shouldBe
      Some("https://www.metacritic.com/movie/near")
  }

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

  /** MC search-result cards in the real layout (see the note above). `year` of 0
   *  emits a card with no date, the undated-hit case. */
  private def searchPage(hits: Seq[(String, String, Int)]): String =
    hits.map { case (slug, title, year) =>
      val date = if (year == 0) "" else s"<span>July 22, $year</span>"
      s"""<a href="/movie/$slug/" class="c-search-item search-item__content">
         |  <p class="c-search-item__title">$title</p>$date
         |</a>""".stripMargin
    }.mkString("<html><body>", "\n", "</body></html>")

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

  // Regression: cinemas and MC disagree on the dash glyph ("Chainsaw Man – The
  // Movie" en-dash vs hyphen). Without folding, the exact-title bar fails and
  // the film is missed despite being identical.
  it should "fold dash variants so an en-dash title matches a hyphen query" in {
    val c = new MetacriticClient(stub(Set.empty))
    val hits = Seq(
      MetacriticClient.SearchHit("chainsaw-man-the-movie-reze-arc", "Chainsaw Man – The Movie: Reze Arc", Some(2025))
    )
    c.pickBestSearchHit(hits, "Chainsaw Man - The Movie: Reze Arc", Some(2025)).map(_.slug) shouldBe
      Some("chainsaw-man-the-movie-reze-arc")
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

  "parseReleaseYear" should "read the year from JSON-LD datePublished" in {
    MetacriticClient.parseReleaseYear(moviePage("North", 1994, 33)) shouldBe Some(1994)
  }

  it should "return None when the page has no datePublished" in {
    MetacriticClient.parseReleaseYear("<html><body>no json-ld here</body></html>") shouldBe None
  }

  "yearsCompatible" should "treat a missing year on either side as compatible" in {
    assert(MetacriticClient.yearsCompatible(None, Some(1994)))
    assert(MetacriticClient.yearsCompatible(Some(2026), None))
    assert(MetacriticClient.yearsCompatible(None, None))
  }

  it should "accept cross-region drift (up to ~15y) and reject a decade-plus collision gap" in {
    assert(MetacriticClient.yearsCompatible(Some(2025), Some(2025)))
    assert(MetacriticClient.yearsCompatible(Some(2025), Some(2026)))  // late/early boundary
    assert(MetacriticClient.yearsCompatible(Some(1975), Some(1979)))  // Picnic: AU vs US date
    assert(!MetacriticClient.yearsCompatible(Some(2026), Some(1996)))  // Michael: biopic vs 1996 comedy
    assert(!MetacriticClient.yearsCompatible(Some(2026), Some(1994)))  // The North: 2026 vs 1994
    assert(!MetacriticClient.yearsCompatible(Some(2018), Some(1977)))  // Suspiria: remake vs original
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
