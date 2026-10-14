package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.FixtureRoot
import tools.HttpFetch

import java.nio.file.Files
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class RecordingHttpFetchSpec extends AnyFlatSpec with Matchers {

  // A header-bearing GET (Flicks' `is-ajax-call`) must reach the delegate WITH its headers: the
  // inherited default dropped them and recorded the page a header-less request gets instead.
  "RecordingHttpFetch" should "forward a GET's headers to the fetch it records, and record that answer" in {
    var seen = Option.empty[Map[String, String]]
    val live = new HttpFetch {
      def get(url: String): String = "plain page"
      override def get(url: String, headers: Map[String, String]): String = { seen = Some(headers); "ajax page" }
      override def getBytes(url: String): Array[Byte] = get(url).getBytes("UTF-8")
      def post(url: String, body: String, contentType: String): String = ""
    }
    val root      = Files.createTempDirectory("recording-spec")
    val recording = new RecordingHttpFetch("dir", live, root = FixtureRoot(root))

    recording.get("https://example.test/sessions/", Map("is-ajax-call" -> "yes")) shouldBe "ajax page"
    seen shouldBe Some(Map("is-ajax-call" -> "yes"))
    new FakeHttpFetch("dir", root = FixtureRoot(root)).get("https://example.test/sessions/") shouldBe "ajax page"
  }

  // A gap-fill leg's order passes share one live answer per request (`SharedLiveAnswers`), but each
  // pass's recorder writes that answer to the SAME fixture file, and each pass's replay reads that file
  // first. Written in place — create, truncate, write — a pass reading it mid-rewrite got an empty or
  // cut-off page: /m/eternity_2025 read back with no Tomatometer in one pass and 76 in the other two
  // (run 38498856157), an arrival-order "dependence" the pipeline does not have.
  it should "never let a reader see a fixture half-written while another recording rewrites it" in {
    val body = ("<html>" + "x" * (4 * 1024 * 1024) + "\"criticsScore\":{\"score\":\"76\"}</html>").getBytes("UTF-8")
    val live = new HttpFetch {
      def get(url: String): String = new String(body, "UTF-8")
      override def getBytes(url: String): Array[Byte] = body
      def post(url: String, body: String, contentType: String): String = ""
    }
    val root      = Files.createTempDirectory("recording-spec")
    val recording = new RecordingHttpFetch("dir", live, root = FixtureRoot(root))
    val url       = "https://www.rottentomatoes.com/m/eternity_2025"
    val _         = recording.getBytes(url)
    // Built once the tree exists, so it reads it by its real path, as a leg's replay does.
    val replay    = new FakeHttpFetch("dir", root = FixtureRoot(root))
    replay.getBytes(url).length shouldBe body.length

    val torn   = new AtomicInteger
    val reads  = new AtomicInteger
    val done   = new AtomicBoolean(false)
    // A read that throws (the file momentarily absent) is as torn as a short one.
    val reader = new Thread(() => while (!done.get()) {
      val _ = reads.incrementAndGet()
      if (!scala.util.Try(replay.getBytes(url).length).toOption.contains(body.length)) { val _ = torn.incrementAndGet() }
    })
    reader.start()
    try (1 to 30).foreach(_ => recording.getBytes(url))
    finally { done.set(true); reader.join() }

    reads.get() should be > 0
    torn.get() shouldBe 0
  }
}
