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

  "a label" should "call a film right at its venue however every venue's label calls it wrong" in {
    // PL Kino Iluzjon's "Lalka" [1968] {Wojciech Jerzy Has}: the archive's screening of Has' film, where the bare "Lalka"
    // every other venue bills is Kawalski's 2026 one
    val labels = Seq(
      Label("pl", "*", "Lalka", "tmdb:81315", right = false, "1968"),
      Label("pl", "*", "Lalka", "imdb:tt0064570", right = false, "1968"),
      Label("pl", "*", "Lalka", "tmdb:1321666", right = true, "2026"),
      Label("pl", "Kino Iluzjon", "Lalka", "tmdb:81315", right = true, "the archive bills Has' 1968 film"),
      Label("pl", "Kino Iluzjon", "Lalka", "imdb:tt0064570", right = true, "the archive bills Has' 1968 film"))
    def lalka(venue: String, tmdb: Int, imdb: String) = Take("pl", venue, "Lalka", Some(tmdb), Some(imdb), "OwnMatch", "Lalka")
    UnmatchedClusters.verdict(lalka("Kino Iluzjon", 81315, "tt0064570"), labels) shouldBe Some(true)
    UnmatchedClusters.verdict(lalka("Kino Kadr", 81315, "tt0064570"), labels) shouldBe Some(false)
    UnmatchedClusters.verdict(lalka("Kino Kadr", 1321666, "tt37082105"), labels) shouldBe Some(true)
  }

  // The shrink-only tolerance of the WRONG takes no safe fix removes (the judgements' known-wrong rows of scope `capture`,
  // [[IdentityJudgements.tolerated]]): a wrong take it lists stands, a wrong take it does not list fails, and a billing no
  // wrong take is any more fails as stale — they only shrink.

  private def agreed(venue: String, raw: String, tmdb: Int) = Take("de", venue, raw, Some(tmdb), None, "Agreed", s"film $tmdb")

  "the known-wrong takes" should "let a listed wrong take stand at every venue billing it, and fail an unlisted one" in {
    val known = Set(("de", "WHEN PIGS FLY", "tmdb:149933"))
    val audit = UnmatchedClusters.auditWrong(Seq(agreed("Neue Post Lichtspiele", "WHEN PIGS FLY", 149933),
      agreed("Neues Maxim Kino München", "WHEN PIGS FLY", 149933), agreed("Lumière", "40 Jahre  Lumière - Die Geburtstagsfeier", 403248)), known)
    audit.fresh.map(_.rawTitle) shouldBe Seq("40 Jahre  Lumière - Die Geburtstagsfeier")
    audit.stale shouldBe empty
  }

  it should "let a listed wrong take stand when its venue re-spaces the billing" in {
    val audit = UnmatchedClusters.auditWrong(Seq(agreed("Neue Post Lichtspiele", "WHEN  PIGS FLY ", 149933)), Set(("de", "WHEN PIGS FLY", "tmdb:149933")))
    audit.fresh shouldBe empty
    audit.stale shouldBe empty
  }

  it should "name a billing no wrong take is any more as stale, to strike" in {
    // a fix cures it (or takes another wrong film): the row goes, so they only shrink
    val known = Set(("de", "WHEN PIGS FLY", "tmdb:149933"), ("de", "Begegnungen der Seele 2", "imdb:tt37057962"))
    val audit = UnmatchedClusters.auditWrong(Seq(agreed("Neue Post Lichtspiele", "WHEN PIGS FLY", 149933)), known)
    audit.fresh shouldBe empty
    audit.stale shouldBe Seq(("de", "Begegnungen der Seele 2", "imdb:tt37057962"))
  }
}
