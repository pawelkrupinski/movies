package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.concurrent.duration._

/**
 * The on-disk enrichment cache. What it has to get right is survival ACROSS a
 * process: the whole reason it exists is that a run's remembered 404s have to be
 * there for the next run, which is the one thing an in-memory cache cannot do and
 * the recorded fixture tree — successes only — does not do either.
 */
class FileEnrichmentCacheStoreSpec extends AnyFlatSpec with Matchers {

  private def withStore(ttl: FiniteDuration = FileEnrichmentCacheStore.Ttl)
                       (body: (Path, FileEnrichmentCacheStore) => Unit): Unit = {
    val root = Files.createTempDirectory("enrichment-cache-spec")
    try body(root, new FileEnrichmentCacheStore(root, ttl))
    finally Files.walk(root).sorted(java.util.Comparator.reverseOrder())
      .forEach(path => Files.deleteIfExists(path))
  }

  "the file enrichment cache" should "replay a body across a fresh store over the same directory" in {
    withStore() { (root, store) =>
      store.put("GET https://api.themoviedb.org/3/search", CachedResponse.Body("""{"results":[]}"""))

      new FileEnrichmentCacheStore(root).loadAll() shouldBe
        Map("GET https://api.themoviedb.org/3/search" -> CachedResponse.Body("""{"results":[]}"""))
    }
  }

  // THE headline. Half a country's films never resolve, and each still costs three
  // or four rating-slug guesses that 404. `RecordingHttpFetch` writes down only
  // successes, so without this every one of those is re-asked, live and paced, on
  // every run — which is the entire 22 minutes of a Poland leg.
  it should "remember a FAILURE, with its status, across a fresh store" in {
    withStore() { (root, store) =>
      store.put("GET https://www.rottentomatoes.com/m/kanal_1956",
        CachedResponse.Failed(Some(404), "GET", "HTTP 404"))

      new FileEnrichmentCacheStore(root).loadAll() shouldBe
        Map("GET https://www.rottentomatoes.com/m/kanal_1956" ->
          CachedResponse.Failed(Some(404), "GET", "HTTP 404"))
    }
  }

  it should "round-trip a body containing newlines, which every HTML page does" in {
    val page = "<html>\n  <body>\n    <h1>Kanał</h1>\n  </body>\n</html>"
    withStore() { (root, store) =>
      store.put("GET https://www.metacritic.com/movie/kanal", CachedResponse.Body(page))

      new FileEnrichmentCacheStore(root).loadAll()("GET https://www.metacritic.com/movie/kanal") shouldBe
        CachedResponse.Body(page)
    }
  }

  it should "keep a body and its raw bytes apart, and a status-less failure readable" in {
    withStore() { (root, store) =>
      store.put("BYTES https://example.test/x", CachedResponse.Bytes("YWJj"))
      store.put("GET https://example.test/dead", CachedResponse.Failed(None, "GET", "SocketTimeoutException"))

      val loaded = new FileEnrichmentCacheStore(root).loadAll()
      loaded("BYTES https://example.test/x")    shouldBe CachedResponse.Bytes("YWJj")
      loaded("GET https://example.test/dead")   shouldBe CachedResponse.Failed(None, "GET", "SocketTimeoutException")
    }
  }

  // Written with an aged stamp rather than by racing a zero TTL against the clock:
  // an entry put and read in the same millisecond is exactly on the boundary, so
  // that version of this test passed or failed on timer granularity.
  it should "not serve an entry older than its TTL" in {
    withStore() { (root, store) =>
      store.put("GET https://example.test/fresh", CachedResponse.Body("new"))
      val aged = root.resolve("00").resolve("00")
      Files.createDirectories(aged)
      Files.write(aged.resolve(s"stale${FileEnrichmentCacheStore.Extension}"),
        FileEnrichmentCacheStore.encode("GET https://example.test/stale",
          System.currentTimeMillis() - 8.days.toMillis, CachedResponse.Body("old")))

      new FileEnrichmentCacheStore(root).loadAll().keySet shouldBe Set("GET https://example.test/fresh")
    }
  }

  // A killed run leaves whatever it was mid-write. One unreadable entry should cost
  // one live fetch, not the entire preload it happens to sit in.
  it should "skip an undecodable entry rather than lose the whole preload" in {
    withStore() { (root, store) =>
      store.put("GET https://example.test/good", CachedResponse.Body("fine"))
      val corrupt = root.resolve("ab").resolve("cd")
      Files.createDirectories(corrupt)
      Files.write(corrupt.resolve(s"deadbeef${FileEnrichmentCacheStore.Extension}"), Array[Byte](1, 2, 3))

      new FileEnrichmentCacheStore(root).loadAll() shouldBe
        Map("GET https://example.test/good" -> CachedResponse.Body("fine"))
    }
  }

  it should "overwrite an existing entry rather than accumulate one file per write" in {
    withStore() { (root, store) =>
      store.put("GET https://example.test/a", CachedResponse.Body("first"))
      store.put("GET https://example.test/a", CachedResponse.Body("second"))

      val loaded = new FileEnrichmentCacheStore(root).loadAll()
      loaded shouldBe Map("GET https://example.test/a" -> CachedResponse.Body("second"))
      Files.walk(root).filter(p => p.getFileName.toString.endsWith(FileEnrichmentCacheStore.Extension))
        .count() shouldBe 1L
    }
  }

  it should "sit inside the fixture tree, so the artifact carries it" in {
    FileEnrichmentCacheStore.beside("enrichment-pl").toString should
      endWith ("test/resources/fixtures/enrichment-pl/.enrichment-cache")
  }
}
