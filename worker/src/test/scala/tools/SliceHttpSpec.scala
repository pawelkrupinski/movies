package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.Answer

import java.nio.file.Files
import scala.jdk.CollectionConverters._

/** An identity slice's HTTP answers ([[SliceHttp]]): noted as the recording answered them, replayed from the notes alone
 *  — a recording's gap read as unknown, a remembered failure thrown again — and a request the notes lack failing loudly. */
class SliceHttpSpec extends AnyFlatSpec with Matchers {

  /** A recording: `/body` answers, `/gone` is a remembered 404, anything else is its gap leaf's. */
  private def recording() = {
    val leaf = new RecordedCorpus.GapLeaf(new MissingFixtures)
    val fetch = new HttpFetch {
      def get(url: String): String =
        if (url.endsWith("/body")) "answer" else if (url.endsWith("/gone")) throw new HttpStatusException(404, "GET", url, None) else leaf.get(url)
      def post(url: String, body: String, contentType: String): String = s"posted $body"
      override def getBytes(url: String): Array[Byte] = Array[Byte](1, 2, 3)
    }
    (fetch, leaf)
  }

  private def noted() = {
    val (fetch, leaf) = recording()
    val noting = new SliceHttp.Noting(fetch, () => leaf.metOnThread)
    noting.get("https://h/body")
    noting.post("https://h/graphql", "{q}", "application/json")
    noting.getBytes("https://h/poster.jpg")
    an[HttpStatusException] should be thrownBy noting.get("https://h/gone")
    noting.get("https://h/unrecorded")
    noting
  }

  "A slice's noted answers" should "replay the recording's bodies, bytes and POSTs from the notes alone" in {
    val replay = new SliceHttp.Replaying(noted().noted.asScala.toMap)
    replay.get("https://h/body") shouldBe "answer"
    replay.post("https://h/graphql", "{q}", "application/json") shouldBe "posted {q}"
    replay.getBytes("https://h/poster.jpg").toSeq shouldBe Seq[Byte](1, 2, 3)
  }

  it should "throw a remembered failure again, as its status" in {
    val replay = new SliceHttp.Replaying(noted().noted.asScala.toMap)
    the[HttpStatusException] thrownBy replay.get("https://h/gone") should have(Symbol("code")(404))
  }

  it should "read the recording's own gap as unknown, and name it" in {
    val replay = new SliceHttp.Replaying(noted().noted.asScala.toMap)
    replay.gaps.answered(replay.get("https://h/unrecorded")) shouldBe Answer.Unknown
    replay.gapsMet.asScala.map(_.url) shouldBe Set("https://h/unrecorded")
    replay.gaps.answered(replay.get("https://h/body")) shouldBe Answer.Known("answer")
  }

  "A request the slice does not hold" should "fail loudly, named, never answered empty" in {
    val replay = new SliceHttp.Replaying(noted().noted.asScala.toMap)
    a[SliceHttp.SliceLacks] should be thrownBy replay.get("https://h/never-asked")
    replay.gaps.answered(replay.get("https://h/never-asked-either")) shouldBe Answer.Unknown
    replay.lacking.asScala.map(_.url) shouldBe Set("https://h/never-asked", "https://h/never-asked-either")
  }

  it should "be read from the recording when one is given to widen from, and named as widened" in {
    val (fetch, leaf) = recording()
    val replay = new SliceHttp.Replaying(Map.empty, widen = Some(new SliceHttp.Noting(fetch, () => leaf.metOnThread)))
    replay.get("https://h/body") shouldBe "answer"
    replay.gaps.answered(replay.get("https://h/elsewhere")) shouldBe Answer.Unknown
    replay.lacking shouldBe empty
    replay.widened.keySet.map(_.url) shouldBe Set("https://h/body", "https://h/elsewhere")
  }

  "The notes" should "read back from disk as written" in {
    val notes = noted().noted.asScala.toMap
    val path  = Files.createTempDirectory("slice-http").resolve("http.jsonl.gz")
    val back  = SliceHttp.read { SliceHttp.write(path, notes); path }
    back.keySet shouldBe notes.keySet
    back.collect { case (r, SliceHttp.Outcome.Bytes(b)) => r.url -> b.toSeq } shouldBe Map("https://h/poster.jpg" -> Seq[Byte](1, 2, 3))
    back.values.count(_ == SliceHttp.Outcome.Gap) shouldBe 1
  }
}
