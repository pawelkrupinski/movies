package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{WikidataClient, WikidataClient as WD}
import tools.HttpFetch

/**
 * Fixture-driven tests for WikidataClient.
 *
 * Fixtures captured from the Wikidata Action API:
 *   search:   GET /w/api.php?action=query&list=search&srsearch=haswbstatement:P5032=1118&…
 *   entities: GET /w/api.php?action=wbgetentities&ids=Q722281&props=claims&…
 * Film: "Popiół i diament" (1958) — Filmweb ID 1118, IMDb tt0052080, Wikidata Q722281.
 */
class WikidataClientSpec extends AnyFlatSpec with Matchers {

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  private def wikidataStub(responses: Map[String, String]): WikidataClient =
    new WikidataClient(new HttpFetch {
      def get(url: String): String = responses
        .collectFirst { case (key, body) if url.contains(key) => body }
        .getOrElse(throw new RuntimeException(s"no stub for $url"))
      // headers overload — test fakes ignore headers, routing by URL only
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("WikidataClient should not POST")
    })

  // ── filmwebEntityId ────────────────────────────────────────────────────────

  "filmwebEntityId" should "extract the numeric id from a canonical film URL" in {
    WD.filmwebEntityId("https://www.filmweb.pl/film/Popi%C3%B3%C5%82+i+diament-1958-1118") shouldBe Some("1118")
  }

  it should "extract from a serial URL" in {
    WD.filmwebEntityId("https://www.filmweb.pl/serial/Pucio-2026-10016294") shouldBe Some("10016294")
  }

  it should "return None for a search-redirect URL" in {
    WD.filmwebEntityId("https://www.filmweb.pl/search?query=Fargo") shouldBe None
  }

  it should "return None for a URL with no trailing numeric segment" in {
    WD.filmwebEntityId("https://www.filmweb.pl/film/SomeTitle") shouldBe None
  }

  // ── findImdbIdByFilmwebId ──────────────────────────────────────────────────

  "findImdbIdByFilmwebId" should "return the IMDb id when Wikidata has a P5032→P345 cross-reference" in {
    // Filmweb ID 1118 = "Popiół i diament" (1958) = Wikidata Q722281 = IMDb tt0052080
    val client = wikidataStub(Map(
      "haswbstatement"  -> loadFixture("/fixtures/wikidata/search_filmwebid_1118.json"),
      "wbgetentities"   -> loadFixture("/fixtures/wikidata/entities_Q722281.json"),
    ))
    client.findImdbIdByFilmwebId("1118") shouldBe Some("tt0052080")
  }

  it should "return None when no Wikidata item matches the Filmweb id" in {
    val client = wikidataStub(Map(
      "haswbstatement" -> loadFixture("/fixtures/wikidata/search_filmwebid_notfound.json"),
    ))
    client.findImdbIdByFilmwebId("99999999") shouldBe None
  }

  it should "return None when the matched Wikidata item has no P345 claim" in {
    val noP345 =
      """{"entities":{"Q99":{"claims":{"P5032":[{"mainsnak":{"datavalue":{"value":"99"}}}]}}}}"""
    val client = wikidataStub(Map(
      "haswbstatement" -> """{"query":{"search":[{"title":"Q99"}]}}""",
      "wbgetentities"  -> noP345,
    ))
    client.findImdbIdByFilmwebId("99") shouldBe None
  }

  it should "return None (not throw) when the HTTP call fails" in {
    val client = new WikidataClient(new HttpFetch {
      def get(url: String): String = throw new java.io.IOException("network error")
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String = ???
    })
    client.findImdbIdByFilmwebId("1118") shouldBe None
  }
}
