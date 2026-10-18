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

  "The training file" should "hold the header and the rows in country, cluster, film order, and the takes file today's takes alone" in {
    val dir = Files.createTempDirectory("unified")
    try {
      def row(country: String, film: String, today: String = "1") = Row(country, "c1", "corpus", "Kino", 1, "Lalka\tx", film, "Lalka", today, "1", "weak", 0, 0, Map.empty)
      val written = IdentityUnifiedDataset.write(dir.resolve("out"), Seq(row("pl", "tmdb:2", today = "0"), row("de", "tmdb:9"), row("pl", "tmdb:1")))
      def lines(file: java.nio.file.Path) = new String(new GZIPInputStream(Files.newInputStream(file)).readAllBytes(), StandardCharsets.UTF_8).linesIterator.toSeq
      lines(written).head shouldBe IdentityUnifiedDataset.Header.mkString("\t")
      lines(written).tail.map(_.split("\t").take(7).toSeq) shouldBe Seq(
        Seq("de", "c1", "corpus", "Kino", "1", "Lalka x", "tmdb:9"), Seq("pl", "c1", "corpus", "Kino", "1", "Lalka x", "tmdb:1"),
        Seq("pl", "c1", "corpus", "Kino", "1", "Lalka x", "tmdb:2"))
      // the takes beside them: the header, then only the rows today's stack takes
      val takes = lines(dir.resolve("out/takes.tsv.gz"))
      takes.head shouldBe IdentityUnifiedDataset.Header.mkString("\t")
      takes.tail.map(_.split("\t")(6)) shouldBe Seq("tmdb:9", "tmdb:1")
    } finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
  }
}
