package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import integration.IdentityUnifiedDataset.{Judged, Row}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.GZIPInputStream

/** The unified training rows' labels — hand before weak, a weak label only where nothing argues against it — and the file. */
class IdentityUnifiedDatasetSpec extends AnyFlatSpec with Matchers {

  import IdentityUnifiedDataset.labelsOf

  private def judged(film: String, right: Int = 0, wrong: Int = 0, signals: Map[String, Double] = Map.empty) = Judged(film, signals, right, wrong)

  "A hand label" should "mark its film right or wrong, leave a conflict unlabelled, and every other contender of a right film wrong" in {
    labelsOf(Seq(judged("tmdb:1", right = 2), judged("tmdb:2"), judged("tmdb:3", right = 1, wrong = 1), judged("tmdb:4", wrong = 1)),
      taken = "tmdb:2", fromFixture = false) shouldBe
      (Seq("1" -> "hand", "0" -> "hand", "" -> "conflict", "0" -> "hand"), false)
  }

  "A weak label" should "take the model's film right and its other contenders wrong, on a corpus cluster no label judges" in {
    labelsOf(Seq(judged("tmdb:1"), judged("tmdb:2")), taken = "tmdb:1", fromFixture = false) shouldBe (Seq("1" -> "weak", "0" -> "weak"), false)
  }

  it should "be withheld where a poster or a family argues against the take, a label denies it, or the cluster is the fixture's" in {
    val unlabelled = Seq("" -> "", "" -> "")
    labelsOf(Seq(judged("tmdb:1", signals = Map("poster.otherMatches" -> 1.0)), judged("tmdb:2")), "tmdb:1", fromFixture = false) shouldBe (unlabelled, true)
    labelsOf(Seq(judged("tmdb:1", signals = Map("family.dissent" -> 1.0)), judged("tmdb:2")), "tmdb:1", fromFixture = false) shouldBe (unlabelled, true)
    labelsOf(Seq(judged("tmdb:1"), judged("tmdb:2")), taken = "tmdb:9", fromFixture = false) shouldBe (unlabelled, true)
    labelsOf(Seq(judged("tmdb:1"), judged("tmdb:2")), taken = "tmdb:1", fromFixture = true) shouldBe (unlabelled, false)
    labelsOf(Seq(judged("tmdb:1"), judged("tmdb:2")), taken = "", fromFixture = false) shouldBe (unlabelled, false)
  }

  "The training file" should "hold the header and the rows in country, cluster, film order" in {
    val dir = Files.createTempDirectory("unified")
    try {
      def row(country: String, film: String) = Row(country, "c1", "corpus", "Kino", 1, "Lalka\tx", film, "Lalka", "1", "1", "weak", 0, 0, Map.empty)
      val written = IdentityUnifiedDataset.write(dir.resolve("out"), Seq(row("pl", "tmdb:2"), row("de", "tmdb:9"), row("pl", "tmdb:1")))
      val lines = new String(new GZIPInputStream(Files.newInputStream(written)).readAllBytes(), StandardCharsets.UTF_8).linesIterator.toSeq
      lines.head shouldBe IdentityUnifiedDataset.Header.mkString("\t")
      lines.tail.map(_.split("\t").take(7).toSeq) shouldBe Seq(
        Seq("de", "c1", "corpus", "Kino", "1", "Lalka x", "tmdb:9"), Seq("pl", "c1", "corpus", "Kino", "1", "Lalka x", "tmdb:1"),
        Seq("pl", "c1", "corpus", "Kino", "1", "Lalka x", "tmdb:2"))
    } finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
  }
}
