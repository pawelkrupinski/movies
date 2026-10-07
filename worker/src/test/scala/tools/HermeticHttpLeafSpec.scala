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
    val root = Paths.get(settings.FixtureRoot.RepositoryRelative.of(tree))
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

  // What the next leg's fill row fetches: every gap whose whole request is a credential-free URL, with the
  // verb the remembered verdicts key it by — and nothing a process holding no secret could not ask, or a
  // public release asset should not name.
  "the refetch list" should "carry every credential-free GET with its verb, and nothing else" in {
    val missing = new MissingFixtures
    val leaf    = new HermeticHttpLeaf(missing)
    an [Exception] should be thrownBy leaf.get("https://www.flicks.us/movie/toy-story-5/")
    an [Exception] should be thrownBy leaf.getBytes("https://drafthouse.com/s/mother/v2/schedule/presentation/akira")
    an [Exception] should be thrownBy leaf.get("https://www.omdbapi.com/?t=A&apikey=secret")
    an [Exception] should be thrownBy leaf.get("https://api.test/bearer", Map("Authorization" -> "Bearer secret"))
    an [Exception] should be thrownBy leaf.post("https://caching.graphql.imdb.com/", """{"id":"tt1"}""", "application/json")

    val file = java.nio.file.Files.createTempDirectory("refetch").resolve("enrichment-us.refetch.tsv")
    missing.writeRefetches(file) shouldBe 2
    val lines = java.nio.file.Files.readAllLines(file).toArray(Array.empty[String]).toSeq
    lines.flatMap(MissingFixtures.Refetch.parse).map(_._2) shouldBe Seq(
      MissingFixtures.Refetch("BYTES", "https://drafthouse.com/s/mother/v2/schedule/presentation/akira"),
      MissingFixtures.Refetch("GET", "https://www.flicks.us/movie/toy-story-5/"))
    lines.mkString should not include "secret"
    lines.head shouldBe "# 2 fetchable gap(s)"
    withClue("a release refuses a zero-byte asset: ") {
      val none = file.resolveSibling("enrichment-de.refetch.tsv")
      new MissingFixtures().writeRefetches(none) shouldBe 0
      java.nio.file.Files.size(none) should be > 0L
    }
    missing.size shouldBe 5
    missing.report("enrichment-us") should include("by host: ")
  }

  it should "be written beside the tree, never inside it" in {
    MissingFixtures.refetchListBeside(Paths.get("test/resources/fixtures/enrichment-us")) shouldBe
      Paths.get("test/resources/fixtures/enrichment-us.refetch.tsv")
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
