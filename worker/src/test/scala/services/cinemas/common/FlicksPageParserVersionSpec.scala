package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What `FlicksClient` parses out of a day page is remembered against the page (`ChunkPageMemo`), and
 *  reused while the page stays the same and [[FlicksClient.PageParserVersion]] does too — so a change to
 *  the parse that does not bump the version would keep serving the old parse of every unchanged page.
 *  This pins the parse of every recorded day page to its version: change the parse, and it fails until
 *  the version is bumped and the new digest pinned. */
class FlicksPageParserVersionSpec extends AnyFlatSpec with Matchers {
  private val Pinned = Map(1 -> "b7037ba9eff854a83f3e3e084b0bef85bd63a88339d538cb79c1bba9204c0885")

  private val days = java.nio.file.Files.walk(java.nio.file.Paths.get("test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions"))
    .filter(_.toString.endsWith(".html")).toArray.map(_.toString).toSeq.sorted

  "FlicksClient.PageParserVersion" should "change whenever what a day page parses to changes" in {
    days should not be empty
    val noNetwork = new _root_.tools.HttpFetch {
      def get(url: String): String = fail(s"unexpected request $url")
      def post(url: String, body: String, contentType: String): String = fail(s"unexpected request $url")
    }
    val client = new FlicksClient(noNetwork, "pinned", models.OdeonNorwich, FlicksMarket.UnitedKingdom)
    val parses = days.map { path =>
      val date = path.split('/').last.stripSuffix(".html")
      s"$path\n${CinemaMovieJson.encode(client.parseChunkPage(date, clients.tools.FixtureFile.read(path)))}"
    }
    withClue(s"the parse of the recorded day pages moved — bump FlicksClient.PageParserVersion and pin its digest: ") {
      Pinned.get(FlicksClient.PageParserVersion) shouldBe Some(_root_.tools.Digest.sha256Hex(parses.mkString("\n")))
    }
  }
}
