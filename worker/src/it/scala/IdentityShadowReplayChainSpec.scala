package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{CachedResponse, EnrichmentCache, HttpFetch, InMemoryEnrichmentCacheStore, MissingFixtures, RecordedCorpus}

/** A full corpus's replay chain ([[tools.RecordedCorpus.replayChain]]): what the recorded tree and the remembered verdicts
 *  answer is an answer; a request neither answers is a GAP however often it is asked — a failed read, which the next
 *  capture reads again — never the gap leaf's empty stand-in remembered as though the source had answered it. */
class IdentityShadowReplayChainSpec extends AnyFlatSpec with Matchers {

  private val api = "https://www.cineworld.co.uk/api/gatsby-source-boxofficeapi/movies?basic=false&castingLimit=10&ids="
  /** A tree holding nothing: every request falls through, as one the recording never made does. */
  private object EmptyTree extends HttpFetch {
    def get(url: String): String = throw new IllegalStateException(s"not recorded: $url")
    def post(url: String, body: String, contentType: String): String = throw new IllegalStateException(s"not recorded: $url")
  }
  private def cache(held: (String, CachedResponse)*) = {
    val c = new EnrichmentCache(new InMemoryEnrichmentCacheStore(held.toMap))
    c.preload(); c
  }

  "a corpus's replay chain" should "meet a gap every time it is asked, never remembering the leaf's stand-in as an answer" in {
    val leaf  = new RecordedCorpus.GapLeaf(new MissingFixtures)
    val chain = RecordedCorpus.replayChain(EmptyTree, cache(), leaf)
    // the chain-wide detail every Cineworld venue asks: one gap per asker, so none of them reads it as read-but-empty
    (1 to 3).foreach(_ => chain.get(s"${api}1000043020"))
    leaf.met shouldBe 3
  }

  it should "answer a remembered verdict without asking the leaf" in {
    val leaf  = new RecordedCorpus.GapLeaf(new MissingFixtures)
    val key   = tools.CachingEnrichmentFetch.keyOf("GET", s"${api}1000052295")
    val chain = RecordedCorpus.replayChain(EmptyTree, cache(key -> CachedResponse.Body("[]")), leaf)
    chain.get(s"${api}1000052295") shouldBe "[]"
    leaf.met shouldBe 0
  }

  "a live gap" should "stay a gap for the rest of the run when its read failed, asked once, and kept nowhere" in {
    val url   = s"https://api.themoviedb.org/3/movie/${java.util.UUID.randomUUID()}?api_key=${IdentityShadow.StubTmdbKey}"
    val reads = new java.util.concurrent.atomic.AtomicInteger
    val down  = new HttpFetch {
      def get(url: String): String = { reads.incrementAndGet(); throw new java.io.IOException("connection reset") }
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val leaf = new RecordedCorpus.GapLeaf(new MissingFixtures)
    val live = new IdentityShadow.LiveGapLeaf(settings.IdentityLiveGaps("key"), leaf, settings.IdentityLivePerHost(1), down)
    live.get(url) shouldBe "{}"
    live.get(url) shouldBe "{}"
    reads.get shouldBe 1
    leaf.met shouldBe 2
    java.nio.file.Files.exists(IdentityShadow.LiveGapLeaf.fileOf(s"GET $url")) shouldBe false
  }

  it should "keep every TMDB and IMDb answer it gave in place of the gap, as the capture's live answers" in {
    val tmdb  = s"https://api.themoviedb.org/3/search/movie?query=${java.util.UUID.randomUUID()}&api_key=${IdentityShadow.StubTmdbKey}"
    val imdb  = s"https://v3.sg.media-imdb.com/suggestion/x/${java.util.UUID.randomUUID()}.json"
    val venue = s"https://venue.example/film/${java.util.UUID.randomUUID()}"
    val up = new HttpFetch {
      def get(url: String): String = s"answer of ${url.replace("api_key=key", s"api_key=${IdentityShadow.StubTmdbKey}")}"
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val live = new IdentityShadow.LiveGapLeaf(settings.IdentityLiveGaps("key"), new RecordedCorpus.GapLeaf(new MissingFixtures),
      settings.IdentityLivePerHost(1), up)
    try {
      live.get(tmdb); live.get(imdb); live.get(venue)
      // keyed by the request as the replay asks it — the stub key, never the real one
      live.answered shouldBe Map(s"GET $tmdb" -> s"answer of $tmdb", s"GET $imdb" -> s"answer of $imdb")
    } finally Seq(tmdb, imdb).foreach(url => java.nio.file.Files.deleteIfExists(IdentityShadow.LiveGapLeaf.fileOf(s"GET $url")))
  }
}
