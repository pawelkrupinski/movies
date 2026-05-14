package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.RottenTomatoesClient
import tools.HttpFetch

class RottenTomatoesClientSpec extends AnyFlatSpec with Matchers {

  "slugify" should "use underscores instead of hyphens" in {
    RottenTomatoesClient.slugify("The Dark Knight") shouldBe "the_dark_knight"
  }

  it should "drop apostrophes" in {
    RottenTomatoesClient.slugify("Schindler's List") shouldBe "schindlers_list"
  }

  it should "strip accents and Polish ł" in {
    RottenTomatoesClient.slugify("Wartość sentymentalna") shouldBe "wartosc_sentymentalna"
  }

  it should "collapse runs of separators and trim edges" in {
    RottenTomatoesClient.slugify("  Top   Gun!! ") shouldBe "top_gun"
  }

  private def stub(notFound: Set[String]) = new HttpFetch {
    def get(url: String): String =
      if (notFound.exists(url.contains)) throw new RuntimeException("HTTP 404")
      else "OK"
  }

  "canonicalUrl" should "return Some when 200" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.canonicalUrl("The Dark Knight") shouldBe
      Some("https://www.rottentomatoes.com/m/the_dark_knight")
  }

  it should "return None when 404" in {
    val c = new RottenTomatoesClient(stub(Set("/m/")))
    c.canonicalUrl("Foo Bar") shouldBe None
  }

  "urlFor" should "return None when canonical 404s (search URLs are never persisted)" in {
    val c = new RottenTomatoesClient(stub(Set("/m/")))
    c.urlFor("Foo Bar") shouldBe None
  }

  // Regression: RT often drops the leading "the_" (e.g. /m/sting not
  // /m/the_sting). Try the de-articled slug as a second probe.
  it should "try the slug with 'the_' stripped when the primary 404s" in {
    val c = new RottenTomatoesClient(stub(Set("/m/the_sting")))
    c.urlFor("The Sting") shouldBe Some("https://www.rottentomatoes.com/m/sting")
  }

  it should "try the slug with 'a_' stripped when the primary 404s" in {
    val c = new RottenTomatoesClient(stub(Set("/m/a_beautiful_mind")))
    c.urlFor("A Beautiful Mind") shouldBe
      Some("https://www.rottentomatoes.com/m/beautiful_mind")
  }

  it should "leave non-article-leading titles untouched (primary still wins)" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.urlFor("Viridiana") shouldBe Some("https://www.rottentomatoes.com/m/viridiana")
  }

  // Regression: same empty-slug class of bug as MC — slugify of a CJK-only
  // title returns "". `/m/` (RT's movie landing page) returns 200, so without
  // a guard `canonicalUrl` would silently store a bogus URL.
  "candidateSlugs" should "return empty when slugify produces an empty string (CJK / Cyrillic)" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.candidateSlugs("")               shouldBe empty
    c.candidateSlugs("大紅燈籠高高掛") shouldBe empty
  }

  "canonicalUrl" should "return None when the slug is empty (bogus /m/ probe protection)" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.canonicalUrl("大紅燈籠高高掛") shouldBe None
  }

  // ── Search-page scrape (last-resort fallback, mirrors MC) ─────────────────
  //
  // Fixture: test/resources/fixtures/rottentomatoes/search_top_gun.html — first
  // 6 <search-page-media-row> cards from a real /search?search=top%20gun
  // response. Each card carries the title/slug/year as attributes on the
  // custom element, with the link in a child <a data-qa="info-name" href="…/m/…">.

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  private val SearchTopGunFixture        = "/fixtures/rottentomatoes/search_top_gun.html"
  private val MovieTheDarkKnightFixture = "/fixtures/rottentomatoes/movie_the_dark_knight.html"

  "parseSearchResults" should "extract (slug, title, year) for every search-page-media-row in RT's HTML" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    val hits = c.parseSearchResults(loadFixture(SearchTopGunFixture))
    hits.map(_.slug)  shouldBe Seq(
      "top_gun_maverick", "top_gun", "10008578-top_gun",
      "tomcat_top_gun_2_resurrecting_the_f_14",
      "tomcat_top_gun_2_resurrecting_the_f_14_2022",
      "top_gun_over_moscow"
    )
    hits.map(_.title) shouldBe Seq(
      "Top Gun: Maverick", "Top Gun", "Top Gun",
      "Tomcat: Top Gun 2 Resurrecting the F-14",
      "Tomcat: Top Gun 2 Resurrecting the F-14",
      "Top Gun over Moscow"
    )
    hits.map(_.year)  shouldBe Seq(Some(2022), Some(1986), Some(1955), Some(2022), Some(2022), Some(1996))
  }

  "pickBestSearchHit" should "prefer an exact-title match and use year as the tie-breaker" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    val hits = c.parseSearchResults(loadFixture(SearchTopGunFixture))
    // Two exact "Top Gun" matches in the fixture (1986 and 1955). With
    // year=1986 the 1986 cut must win; with year=1955 the 1955 print wins.
    c.pickBestSearchHit(hits, "Top Gun", Some(1986)).map(_.slug) shouldBe Some("top_gun")
    c.pickBestSearchHit(hits, "Top Gun", Some(1955)).map(_.slug) shouldBe Some("10008578-top_gun")
    // No year hint → still picks an exact match (the first one MC scoring sees,
    // i.e. the more famous/popular one in the SERP order — that's what we get).
    c.pickBestSearchHit(hits, "Top Gun", None).map(_.slug) should (be(Some("top_gun")) or be(Some("10008578-top_gun")))
  }

  it should "reject partial-only matches (no wild guessing — better None than wrong)" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    val hits = c.parseSearchResults(loadFixture(SearchTopGunFixture))
    // Nothing exact-matches "Tomcat" alone; the picker must NOT return any of
    // the "Tomcat: Top Gun 2 …" rows (different film). Return None instead.
    c.pickBestSearchHit(hits, "Inconnu de la Grande Arche", Some(2025)) shouldBe None
  }

  it should "accept a modifier-suffix match (re-release / restored variants)" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    val hits = Seq(
      RottenTomatoesClient.SearchHit("top_gun_re_release", "Top Gun - Re-Release", Some(2024), None),
      RottenTomatoesClient.SearchHit("unrelated_film",      "Topsy Turvy",          Some(1999), None)
    )
    c.pickBestSearchHit(hits, "Top Gun", Some(2024)).map(_.slug) shouldBe Some("top_gun_re_release")
  }

  it should "return None for an empty hit list" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.pickBestSearchHit(Seq.empty, "anything", None) shouldBe None
  }

  // urlFor's chain falls through to the search-page scrape when every slug
  // probe 404s — same pattern MC uses.
  "urlFor" should "fall through to the search-page scrape when slug probes 404" in {
    val fixture = loadFixture(SearchTopGunFixture)
    val c = new RottenTomatoesClient(new HttpFetch {
      def get(url: String): String =
        if (url.contains("/m/")) throw new RuntimeException("HTTP 404")
        else if (url.contains("/search")) fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    // Primary slug "top_gun" 404s; the search scrape sees "Top Gun" as an
    // exact title match and (with year=1986) picks the 1986 cut.
    c.urlFor("Top Gun", None, Some(1986)) shouldBe Some("https://www.rottentomatoes.com/m/top_gun")
  }

  it should "use the fallback title when the primary 404s (CJK / wrong-language original)" in {
    // Primary "Foreign Title" 404s and has no leading article; fallback's slug
    // resolves on the first probe.
    val c = new RottenTomatoesClient(stub(Set("/m/foreign_title")))
    c.urlFor("Foreign Title", Some("Raise the Red Lantern")) shouldBe
      Some("https://www.rottentomatoes.com/m/raise_the_red_lantern")
  }

  it should "return None when neither primary nor fallback resolve and the search yields nothing" in {
    val c = new RottenTomatoesClient(new HttpFetch {
      def get(url: String): String =
        if (url.contains("/m/")) throw new RuntimeException("HTTP 404")
        else "<html><body></body></html>"
    })
    c.urlFor("foo", Some("bar")) shouldBe None
  }

  // ── Tomatometer score scrape ───────────────────────────────────────────────
  //
  // Fixture: test/resources/fixtures/rottentomatoes/movie_the_dark_knight.html —
  // a real RT movie page trimmed to the JSON-LD <script type="application/ld+json">
  // block carrying the schema.org `aggregateRating.ratingValue` we parse.

  "parseScore" should "extract the Tomatometer percentage from the JSON-LD aggregateRating block" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.parseScore(loadFixture(MovieTheDarkKnightFixture)) shouldBe Some(94)
  }

  it should "return None when the page has no aggregateRating ld+json" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.parseScore("<html><body>no rating here</body></html>") shouldBe None
  }

  it should "return None when ratingValue is non-numeric or out of [0, 100]" in {
    val c = new RottenTomatoesClient(stub(Set.empty))
    val bogus =
      """<script type="application/ld+json">{"@type":"Movie","aggregateRating":{"@type":"AggregateRating","ratingValue":"N/A"}}</script>"""
    c.parseScore(bogus) shouldBe None
    val outOfRange =
      """<script type="application/ld+json">{"@type":"Movie","aggregateRating":{"@type":"AggregateRating","ratingValue":"150"}}</script>"""
    c.parseScore(outOfRange) shouldBe None
  }

  "scoreFor" should "fetch the URL and return the parsed Tomatometer percentage" in {
    val fixture = loadFixture(MovieTheDarkKnightFixture)
    val c = new RottenTomatoesClient(new HttpFetch {
      def get(url: String): String =
        if (url == "https://www.rottentomatoes.com/m/the_dark_knight") fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.scoreFor("https://www.rottentomatoes.com/m/the_dark_knight") shouldBe Some(94)
  }

  it should "return None when the fetch fails (transient or 404)" in {
    val c = new RottenTomatoesClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("HTTP 503")
    })
    c.scoreFor("https://www.rottentomatoes.com/m/whatever") shouldBe None
  }

  it should "refuse to fetch a non-canonical URL (search URLs must never round-trip through scoreFor)" in {
    // scoreFor is meant for /m/<slug> pages only. If a caller hands it a
    // /search?... URL we should bail with None rather than try to parse it.
    val c = new RottenTomatoesClient(stub(Set.empty))
    c.scoreFor("https://www.rottentomatoes.com/search?search=foo") shouldBe None
  }
}
