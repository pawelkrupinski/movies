package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What `FlicksClient` parses out of a day page is remembered against the page (`ChunkPageMemo`), and
 *  reused while the page stays the same and [[FlicksClient.PageParserVersion]] does too — so a change to
 *  the parse that does not bump the version would keep serving the old parse of every unchanged page.
 *  This pins the parse of every recorded day page to its version: change the parse, and it fails until
 *  the version is bumped and the new digest pinned. */
class FlicksPageParserVersionSpec extends AnyFlatSpec with Matchers {
  private val Pinned = Map(
    1 -> "b7037ba9eff854a83f3e3e084b0bef85bd63a88339d538cb79c1bba9204c0885",
    2 -> "f758df7a8a279b0ed0dea347771ccf350250e9d9a9f5514ada34cfe2e5dbafd0") // every content_director name, not the card's first

  private val days = java.nio.file.Files.walk(java.nio.file.Paths.get("test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions"))
    .filter(_.toString.endsWith(".html")).toArray.map(_.toString).toSeq.sorted

  "FlicksClient.PageParserVersion" should "change whenever what a day page parses to changes" in {
    days should not be empty
    val noNetwork = new _root_.tools.HttpFetch {
      def get(url: String): String = fail(s"unexpected request $url")
      def post(url: String, body: String, contentType: String): String = fail(s"unexpected request $url")
    }
    val client = new FlicksClient(noNetwork, "pinned", models.OdeonNorwich, FlicksMarket.UnitedKingdom, today = _root_.tools.SpecClock.PinnedDay)
    val parses = days.map { path =>
      val date = path.split('/').last.stripSuffix(".html")
      s"$path\n${CinemaMovieJson.encode(client.parseChunkPage(date, clients.tools.FixtureFile.read(path)))}"
    }
    withClue(s"the parse of the recorded day pages moved — bump FlicksClient.PageParserVersion and pin its digest: ") {
      Pinned.get(FlicksClient.PageParserVersion) shouldBe Some(_root_.tools.Digest.sha256Hex(parses.mkString("\n")))
    }
  }

  // The recorded pages exercise only what they hold: a change to the encoder, the name helper or the models
  // they never reach kept serving remembered parses. The memo's name for the parse covers that code's bytes.
  "FlicksClient.PageParser" should "name the hand version and the code the parse runs through beyond FlicksClient" in {
    FlicksClient.ParseClasses should contain allOf (CinemaMovieJson.getClass, _root_.tools.PersonName.getClass, classOf[models.CinemaMovie])
    FlicksClient.PageParser shouldBe s"${FlicksClient.PageParserVersion}:${_root_.tools.Digest.classesHex(FlicksClient.ParseClasses)}"
    _root_.tools.Digest.classesHex(Seq(classOf[models.CinemaMovie])) should not be _root_.tools.Digest.classesHex(Seq(classOf[models.Movie]))
  }
}
