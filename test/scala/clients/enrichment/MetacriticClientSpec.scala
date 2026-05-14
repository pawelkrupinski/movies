package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.MetacriticClient
import tools.HttpFetch

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
  private def stub(notFound: Set[String]) = new HttpFetch {
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

  it should "return None when neither primary nor fallback resolve" in {
    val c = new MetacriticClient(stub(Set("/movie/")))
    c.urlFor("foo", Some("bar")) shouldBe None
  }
}
