package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.UnmatchedClusters.{Label, Take}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The ratchet's machine-readable findings ([[UnmatchedClusters.findings]]): every take as the labels judge it, and every
 *  expected right take that is gone — moved to another film, or lost. */
class UnmatchedClustersFindingsSpec extends AnyFlatSpec with Matchers {

  private def take(venue: String, raw: String, tmdb: Int) =
    Take("de", venue, raw, Some(tmdb), None, "Filled", s"film $tmdb", explanation = Seq("filled by venues.current", "guards passed: bill.several"))
  private val labels = Seq(
    Label("de", "*", "Right", "tmdb:1", right = true, ""),
    Label("de", "*", "Wrong", "tmdb:2", right = false, ""),
    Label("de", "*", "Moved", "tmdb:9", right = true, ""))
  private val takes = Seq(take("A", "Right", 1), take("B", "Right", 1), take("A", "Wrong", 2), take("A", "Open", 3), take("A", "Moved", 4))
  private val expected = Set(("de", "A", "Right", "tmdb:1"), ("de", "A", "Moved", "tmdb:9"), ("de", "A", "Gone", "tmdb:5"))

  private lazy val found = UnmatchedClusters.findings(takes, labels, expected)

  "the ratchet's findings" should "judge each take by the labels, the expected right takes apart from the new ones" in {
    found.map(f => f.status -> (f.venue, f.rawTitle, f.film)) shouldBe Seq(
      // a take of a listing whose right film another database names is wrong too: the labels name tmdb:9 for "Moved"
      "WRONG"    -> ("A", "Moved", "tmdb:4"),
      "WRONG"    -> ("A", "Wrong", "tmdb:2"),
      "UNJUDGED" -> ("A", "Open", "tmdb:3"),
      "MOVED"    -> ("A", "Moved", "tmdb:4"),
      "LOST"     -> ("A", "Gone", ""),
      "NEW"      -> ("B", "Right", "tmdb:1"),
      "RIGHT"    -> ("A", "Right", "tmdb:1"))
  }

  it should "name the film an expected take moved from, and keep each take's explanation" in {
    found.find(_.status == "MOVED").map(_.was) shouldBe Some("tmdb:9")
    found.find(_.status == "LOST").map(_.was) shouldBe Some("tmdb:5")
    found.find(_.status == "UNJUDGED").map(_.explanation) shouldBe Some(Seq("filled by venues.current", "guards passed: bill.several"))
  }

  it should "write one TSV line per finding, a field's tabs and line breaks flattened" in {
    val path = Files.createTempDirectory("findings").resolve("ratchet-takes.tsv")
    UnmatchedClusters.writeFindings(path, Seq(found.find(_.rawTitle == "Wrong").get.copy(title = "a\tb\nc")))
    val lines = Files.readAllLines(path, StandardCharsets.UTF_8)
    lines.get(0) shouldBe UnmatchedClusters.FindingsHeader
    lines.get(1).split("\t", -1).toSeq shouldBe Seq("WRONG", "de", "A", "Wrong", "tmdb:2", "a b c", "Filled", "tmdb:2", "",
      "filled by venues.current ¶ guards passed: bill.several")
  }
}
