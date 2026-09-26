package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * The wire a hermetic convergence leg runs on: it refuses every request and names the
 * fixture the recorder would have written for it, so a gap in a recording is a precise
 * failure ("record this file") rather than a live fill that quietly makes the verdict
 * depend on whether Cinemeta was up.
 */
class HermeticHttpLeafSpec extends AnyFlatSpec with Matchers {

  "the hermetic leaf" should "refuse every verb and remember each request once" in {
    val missing = new MissingFixtures
    val leaf    = new HermeticHttpLeaf(missing)

    a [MissingFixtureException] should be thrownBy leaf.get("https://api.test/a?q=1")
    a [MissingFixtureException] should be thrownBy leaf.get("https://api.test/a?q=1", Map("Authorization" -> "x"))
    a [MissingFixtureException] should be thrownBy leaf.getBytes("https://api.test/b")
    a [MissingFixtureException] should be thrownBy leaf.post("https://api.test/graphql", """{"id":1}""", "application/json")

    missing.size shouldBe 3
  }

  // The key is only useful if it is the file the RECORDER writes — otherwise the report
  // sends whoever reads it looking for a name no recording will ever produce.
  it should "name the exact file a recording of the same request writes" in {
    val tree = s"hermetic-leaf-spec-${java.util.UUID.randomUUID()}"
    val root = Paths.get(clients.tools.FixtureRoot.RepositoryRelative.of(tree))
    try {
      val answering = new HttpFetch {
        override def get(url: String): String = "body"
        override def getBytes(url: String): Array[Byte] = "body".getBytes("UTF-8")
        override def post(url: String, body: String, contentType: String): String = "posted"
      }
      val recorder = new clients.tools.RecordingHttpFetch(tree, answering, foldYear = false)
      val get  = "https://api.themoviedb.org/3/search/movie?query=Dune&year=2021&api_key=k"
      val post = "https://caching.graphql.imdb.com/"
      recorder.get(get)
      recorder.post(post, """{"id":"tt1"}""", "application/json")

      val missing = new MissingFixtures
      val leaf    = new HermeticHttpLeaf(missing)
      an [Exception] should be thrownBy leaf.get(get)
      an [Exception] should be thrownBy leaf.post(post, """{"id":"tt1"}""", "application/json")

      missing.keys.map(_._1).foreach { key =>
        withClue(s"$key: ")(Files.isRegularFile(root.resolve(key)) shouldBe true)
      }
    } finally if (Files.exists(root))
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
  }

  "the missing-fixture report" should "count every gap, list them in a stable order, and never print a credential" in {
    val missing = new MissingFixtures
    val leaf    = new HermeticHttpLeaf(missing)
    Seq("https://www.omdbapi.com/?t=B&apikey=secret", "https://a.test/x", "https://www.omdbapi.com/?t=A&apikey=secret")
      .foreach(url => an [Exception] should be thrownBy leaf.get(url))

    val report = missing.report("enrichment-pl", limit = 2)
    report should include("needed 3 request(s)")
    report should include("… and 1 more")
    report should not include "secret"
    report should include("Record scrape fixtures")
    missing.keys.map(_._1) shouldBe missing.keys.map(_._1).sorted
  }
}
