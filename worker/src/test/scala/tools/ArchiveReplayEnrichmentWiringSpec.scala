package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.InMemoryScrapeArchiveRepository

/**
 * That the convergence suite's wiring actually ROUTES enrichment through the
 * cache — the seam the cache exists for, as opposed to the cache's own behaviour
 * (which `CachingEnrichmentFetchSpec` covers).
 *
 * Worth its own spec because the failure mode is silent in both directions: a
 * wiring that quietly kept `OfflineHttpFetch` would leave every enrichment field
 * `None` and the convergence specs would still pass, having verified nothing new;
 * a wiring that put the cache UNDER the throttle would replay every hit through a
 * rate limiter and turn a warm run into a slow one.
 */
class ArchiveReplayEnrichmentWiringSpec extends AnyFlatSpec with Matchers {

  /** Stands in for the real network at the very bottom of the wiring's enrich-phase
   *  chain, so what a test counts is genuine wire attempts. */
  private class CountingLeaf extends HttpFetch {
    var calls: Int = 0
    override def get(url: String): String = { calls += 1; s"body for $url" }
    override def post(url: String, body: String, contentType: String): String = { calls += 1; "posted" }
  }

  private def wiringWith(cache: Option[EnrichmentCache], leaf: HttpFetch): ArchiveReplayWiring =
    new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, cache) {
      override protected def realHttpLeaf: HttpFetch = leaf
    }

  "the archive replay wiring" should "refuse enrichment HTTP when it has no cache" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(None, leaf)

    a [UnsupportedOperationException] should be thrownBy wiring.enrichmentFetch.get("https://api.themoviedb.org/3/x")
    leaf.calls shouldBe 0
  }

  it should "keep the SCRAPE side offline even when enrichment is cached" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    // The corpus comes from the archive; a scraper reaching for HTTP is a bug.
    a [UnsupportedOperationException] should be thrownBy wiring.httoFetch.get("https://cinema.test/listing")
    leaf.calls shouldBe 0
  }

  it should "answer a repeated enrichment call from the cache instead of the wire" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune") shouldBe
      "body for https://api.themoviedb.org/3/search?query=dune"
    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune")

    leaf.calls shouldBe 1
  }

  // The TMDB client is what every downstream rating source hangs off; if it were
  // left on the offline fetch nothing would enrich however good the cache was.
  it should "point the TMDB client at the cached fetch" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    wiring.cachedEnrichmentFetch should not be empty
    wiring.enrichmentFetch shouldBe wiring.cachedEnrichmentFetch.get
  }

  // CI runs with the fixture tree and NO Mongo cache — the cache URI named a tunnel
  // the job never started, so it was removed. The tree is then the whole determinism
  // mechanism, and it only works if it can GROW: a miss has to reach live and be
  // recorded, or the same miss recurs on every run for ever. Left as it was, "no
  // cache" meant "offline behind the fixtures" and every unrecorded URL simply
  // failed.
  it should "reach live and record it when fixtures are configured but no cache is" in {
    val leaf      = new CountingLeaf
    val directory = s"enrichment-wiring-${ProcessHandle.current().pid()}"
    val root      = java.nio.file.Paths.get(clients.tools.FakeHttpFetch.rootFor(directory))
    System.setProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES", directory)
    try {
      val wiring = wiringWith(None, leaf)

      wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune") shouldBe
        "body for https://api.themoviedb.org/3/search?query=dune"
      withClue("a fixture miss must reach the live leg: ") { leaf.calls shouldBe 1 }

      withClue("and be recorded, so the next run replays it: ") {
        java.nio.file.Files.exists(root) shouldBe true
        java.nio.file.Files.walk(root).filter(java.nio.file.Files.isRegularFile(_)).count() should be > 0L
      }
    } finally {
      System.clearProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES")
      if (java.nio.file.Files.exists(root))
        java.nio.file.Files.walk(root).sorted(java.util.Comparator.reverseOrder())
          .forEach(path => java.nio.file.Files.deleteIfExists(path))
    }
  }

  // The gate that decides whether TMDB gets a key at all. It asked about the CACHE,
  // so a leg running on the fixture tree alone handed `TmdbClient` `apiKey = None` —
  // and `search` is `authHeader.flatMap`, so every title came back `None` without the
  // fetch being touched. 892 films, 0 resolved, three specs green in 55 seconds.
  it should "give TMDB a real key when fixtures are its source and no cache is" in {
    val directory = s"enrichment-gate-${ProcessHandle.current().pid()}"
    val root      = java.nio.file.Paths.get(clients.tools.FakeHttpFetch.rootFor(directory))
    System.setProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES", directory)
    try {
      // Germany, so the language actually discriminates the two branches: the enabled
      // one passes the country's locale, the short-circuiting one takes the default.
      val wiring = new ArchiveReplayWiring(Country.Germany, new InMemoryScrapeArchiveRepository, None) {
        override protected def realHttpLeaf: HttpFetch = new CountingLeaf
      }

      wiring.enrichmentAvailable shouldBe true
      wiring.tmdbClient.language shouldBe Country.Germany.language
    } finally {
      System.clearProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES")
      if (java.nio.file.Files.exists(root))
        java.nio.file.Files.walk(root).sorted(java.util.Comparator.reverseOrder())
          .forEach(path => java.nio.file.Files.deleteIfExists(path))
    }
  }

  it should "know it has no enrichment source when it has neither cache nor fixtures" in {
    wiringWith(None, new CountingLeaf).enrichmentAvailable shouldBe false
  }

  it should "know it has an enrichment source when it has a cache" in {
    wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), new CountingLeaf)
      .enrichmentAvailable shouldBe true
  }

  // Three concurrent replays each build their own wiring; sharing the cache is what
  // stops them disagreeing about what the live service said.
  it should "share one cache's answers across separate wirings" in {
    val leaf  = new CountingLeaf
    val cache = new EnrichmentCache(new InMemoryEnrichmentCacheStore())

    wiringWith(Some(cache), leaf).enrichmentFetch.get("https://api.themoviedb.org/3/shared")
    wiringWith(Some(cache), leaf).enrichmentFetch.get("https://api.themoviedb.org/3/shared")

    leaf.calls shouldBe 1
  }
}
