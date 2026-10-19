package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** RecordedCorpus builds its cache and chain BY NAME, so identity-slice can drop it into refs older than their required
 *  stopwatch (scripts/identity-slice-old-ref-test.sh compiles it there); here, that the by-name build is the real thing. */
class RecordedCorpusSpec extends AnyFlatSpec with Matchers {

  private object Answering extends HttpFetch {
    def get(url: String): String = s"answered $url"
    def post(url: String, body: String, contentType: String): String = get(url)
  }

  "a by-name construction" should "build a working chain, the stopwatch in its place and every other parameter defaulted" in {
    val chain = RecordedCorpus.constructed(classOf[FallbackHttpFetch], Seq("tree" -> Answering))
    chain.get("https://example.test/a") shouldBe "answered https://example.test/a"
  }

  it should "build an enrichment cache over the store it is handed" in {
    val cache = RecordedCorpus.constructed(classOf[EnrichmentCache], new InMemoryEnrichmentCacheStore())
    cache.preload() shouldBe 0
  }
}
