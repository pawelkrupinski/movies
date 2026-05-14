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
}
