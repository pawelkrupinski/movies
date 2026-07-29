package tools

import clients.tools.{FakeHttpFetch, RecordingHttpFetch}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/**
 * The fixture-first enrichment fetch: replay what is recorded, fall back to live
 * for what is not, and record that so the corpus converges on complete.
 */
class EnrichmentFixtureReplaySpec extends AnyFlatSpec with Matchers {

  private val directory = s"enrichment-spec-${ProcessHandle.current().pid()}"
  private val root: Path = Paths.get("test", "resources", "fixtures", directory)

  private class CountingLive(body: String) extends HttpFetch {
    var calls = 0
    override def get(url: String): String = { calls += 1; body }
    override def getBytes(url: String): Array[Byte] = { calls += 1; body.getBytes("UTF-8") }
    override def post(url: String, b: String, c: String): String = { calls += 1; body }
  }

  private def chain(live: HttpFetch): HttpFetch =
    new RecordingHttpFetch(directory,
      new FallbackHttpFetch(Seq(
        "fixtures" -> new FakeHttpFetch(directory, strict = true, foldYear = false),
        "live"     -> live)),
      foldYear = false)

  private val searchUrl = "https://api.themoviedb.org/3/search/movie?query=Cicha+noc&api_key=k"

  "the enrichment chain" should "fall back to live on a miss, record it, then replay without touching live" in {
    val live = new CountingLive("""{"results":[{"id":1,"title":"Cicha noc"}]}""")
    try {
      chain(live).get(searchUrl) should include ("Cicha noc")
      live.calls shouldBe 1

      // Second identical request: served from the file just recorded.
      val second = new CountingLive("""{"should":"not be used"}""")
      chain(second).get(searchUrl) should include ("Cicha noc")
      withClue("a recorded answer must not reach the live leg: ") { second.calls shouldBe 0 }
    } finally deleteTree(root)
  }

  // The year distinction the default fixture key would have destroyed.
  it should "keep year-scoped and yearless searches in separate fixtures" in {
    val yearScoped = "https://api.themoviedb.org/3/search/movie?query=Cicha+noc&year=2026&api_key=k"
    try {
      chain(new CountingLive("""{"total_results":0,"results":[]}""")).get(yearScoped)
      chain(new CountingLive("""{"total_results":16,"results":[{"id":9}]}""")).get(searchUrl)

      // Each replays its OWN body — folding the year would have collapsed them.
      val replay = new FakeHttpFetch(directory, strict = true, foldYear = false)
      replay.get(yearScoped) should include ("\"total_results\":0")
      replay.get(searchUrl)   should include ("\"total_results\":16")
    } finally deleteTree(root)
  }

  private def deleteTree(p: Path): Unit =
    if (Files.exists(p)) {
      Files.walk(p).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))
    }
}
