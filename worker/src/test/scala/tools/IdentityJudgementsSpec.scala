package tools

import org.bson.BsonArray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.UnmatchedClusters.{Label, Take}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** The one reader of the identity judgements in Scala ([[IdentityJudgements]]): it keys a billing as the Python reader
 *  does, reads each row's verdict as a label, and each gate's tolerance off the rows' state and scope. */
class IdentityJudgementsSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private val Header = IdentityJudgements.Columns.mkString("\t")
  private def file(lines: String*): Path = {
    val path = tempFile("identity-judgements", ".tsv")
    Files.writeString(path, lines.mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    path
  }
  private def take(venue: String, raw: String, tmdb: Int) = Take("pl", venue, raw, Some(tmdb), None, "OwnMatch", s"film $tmdb")

  "a billing" should "be keyed as the Python reader keys it, by the cases both read" in {
    val cases = Files.readAllLines(Path.of("test", "resources", "fixtures", "identity-judgements-normalise.jsonl"), StandardCharsets.UTF_8)
      .asScala.toSeq.filter(_.nonEmpty).map(line => BsonArray.parse(line).getValues.asScala.map(_.asString.getValue).toSeq)
    cases should not be empty
    cases.foreach { case Seq(raw, normalised) => withClue(raw)(IdentityJudgements.normalise(raw) shouldBe normalised) }
  }

  "the reader" should "read each right or wrong row as a label, an undecided one as none" in {
    val rows = IdentityJudgements.read(file("# a comment", Header,
      "pl\t*\tLalka\ttmdb:1\tright\topen\t\tLalka (2026)\t\t2026-10-14\trecall target",
      "pl\tKino Iluzjon\tLalka\ttmdb:2\twrong\topen\t\t\t\t2026-10-14\t",
      "de\t*\tEs\ttmdb:3\tundecided\topen\tserved\tEs (2017)\tno page says which\t2026-10-13\t"))
    IdentityJudgements.labels(rows) shouldBe Seq(Label("pl", "*", "Lalka", "tmdb:1", right = true, "recall target"),
      Label("pl", "Kino Iluzjon", "Lalka", "tmdb:2", right = false, ""))
  }

  it should "let a venue's row override every venue's" in {
    val labels = IdentityJudgements.labels(IdentityJudgements.read(file(Header,
      "pl\t*\tLalka\ttmdb:81315\twrong\topen\t\t\t\t\t", "pl\tKino Iluzjon\tLalka\ttmdb:81315\tright\topen\t\t\t\t\t")))
    UnmatchedClusters.verdict(take("Kino Iluzjon", "Lalka", 81315), labels) shouldBe Some(true)
    UnmatchedClusters.verdict(take("Helios", "Lalka", 81315), labels) shouldBe Some(false)
  }

  it should "tolerate a known-wrong row's billing at every venue in each gate its scope names, and an undecided suspect in the served one" in {
    val rows = IdentityJudgements.read(file(Header,
      "de\tCaligari\tES  (ULRICH SCHAMONI)\ttmdb:346364\twrong\tknown-wrong\tcapture,served\tEs (2017)\tno safe fix\t\t",
      "de\t*\tWHEN PIGS FLY\ttmdb:149933\twrong\topen\t\t\t\t\t",
      "de\t*\tHalloween [OV]\ttmdb:948\tundecided\topen\tserved\t\tno page says which\t2026-10-13\t"))
    IdentityJudgements.tolerated(rows, IdentityJudgements.Scope.Capture) shouldBe Map(("de", "ES (ULRICH SCHAMONI)", "tmdb:346364") -> "no safe fix")
    IdentityJudgements.tolerated(rows, IdentityJudgements.Scope.Served).keySet shouldBe
      Set(("de", "ES (ULRICH SCHAMONI)", "tmdb:346364"), ("de", "Halloween [OV]", "tmdb:948"))
    IdentityJudgements.tolerated(rows, IdentityJudgements.Scope.Rows) shouldBe empty
  }

  it should "read a ref's legacy labels.tsv, for a capture judged by its own ref's labels" in {
    val labels = IdentityJudgements.labels(file("country\tvenue\trawTitle\tfilm\tverdict\tnote", "pl\t*\tLalka\ttmdb:1\tright\tthe 2026 film"))
    labels shouldBe Seq(Label("pl", "*", "Lalka", "tmdb:1", right = true, "the 2026 film"))
  }

  it should "refuse a line short of a column, and a file of another header" in {
    an[IllegalArgumentException] should be thrownBy IdentityJudgements.read(file(Header, "pl\t*\tLalka\ttmdb:1\tright"))
    an[IllegalArgumentException] should be thrownBy IdentityJudgements.read(file("country\trawTitle\tfilm", "pl\tLalka\ttmdb:1"))
  }

  "the checked-in judgements" should "read, every row's verdict one of right, wrong and undecided" in {
    val rows = IdentityJudgements.read()
    rows should not be empty
    rows.map(_.verdict).toSet.subsetOf(Set("right", "wrong", "undecided")) shouldBe true
  }
}
