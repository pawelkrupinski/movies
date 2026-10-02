package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.FixtureRoot
import tools.HttpFetch

import java.nio.file.Files

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
}
