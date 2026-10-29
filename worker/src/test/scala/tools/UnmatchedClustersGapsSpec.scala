package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A capture's GAPS: the lookups a replay asks that the capture holds no answer to. The ratchet fails on any (a stale
 *  capture, re-captured); a MEASUREMENT reads them as unknown and names them, so a change that asks a new lookup
 *  (class 4's relay season searches, 2026-10-15: run 38694072183 lost every country's final takes to one UK RT record)
 *  is still measured, its blind spot listed beside the result. */
class UnmatchedClustersGapsSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private lazy val es = UnmatchedClusters.read(UnmatchedClusters.fixturePath(models.Country.all.find(_.code == "es").get))

  "A capture's gaps" should "be none for the capture replayed by the code it was captured for" in {
    UnmatchedClusters.gaps(es, UnmatchedClusters.replay(es)) shouldBe empty
  }

  it should "name every search the replay asked that the capture lacks, the takes still made" in {
    val lacking = es.copy(queries = Map.empty)
    val outcome = UnmatchedClusters.replay(lacking)
    val gaps    = UnmatchedClusters.gaps(lacking, outcome)
    gaps should not be empty
    gaps.map(_.kind).toSet should contain("tmdb")
    noException should be thrownBy UnmatchedClusters.takes(lacking, outcome)
  }

  "The ratchet" should "read a capture's gaps as unknown and list them only when it measures, failing the capture otherwise" in {
    val fixtures = tempDirectory("ratchet-fixtures")
    UnmatchedClusters.write(fixtures.resolve("es.json.gz"), es.copy(queries = Map.empty))
    def run(measure: Boolean) = {
      val out = tempDirectory("ratchet-out")
      val run = new scripts.UnmatchedClustersRatchet.Run(fixtures, IdentityJudgements.CheckedIn, out, measure)
      (run.reported, out)
    }
    val (measured, out) = run(measure = true)
    measured.isRight shouldBe true
    java.nio.file.Files.readString(out.resolve("capture-gaps.tsv")) should include("es\ttmdb\t")
    java.nio.file.Files.exists(out.resolve("ratchet-takes.tsv")) shouldBe true
    val (judged, notMeasured) = run(measure = false)
    judged.left.toOption.get should startWith("es: the fixture lacks answers this resolve asks — fill them: scripts/identity-capture.sh --fill es:")
    java.nio.file.Files.exists(notMeasured.resolve("ratchet-takes.tsv")) shouldBe false
  }

  it should "replay only the countries it is scoped to, and write only their takes" in {
    val out = tempDirectory("ratchet-scoped")
    val run = new scripts.UnmatchedClustersRatchet.Run(UnmatchedClusters.Directory, IdentityJudgements.CheckedIn, out, measure = true,
      countries = Set("es"))
    run.captures.map(_.country.code) shouldBe Seq("es")
    run.reported.isRight shouldBe true
    run.checks.flatMap(_.failure) shouldBe empty // another country's expected or known-wrong takes are not this run's
    val countries = java.nio.file.Files.readAllLines(out.resolve("ratchet-takes.tsv")).toArray.toSeq.drop(1).map(_.toString.split('\t')(1)).toSet
    countries -- Set("es") shouldBe empty
  }

  it should "write each country's gaps as a TSV row a measurement lists" in {
    val path = tempFile("capture-gaps", ".tsv")
    UnmatchedClusters.writeGaps(path, Seq(UnmatchedClusters.Gap("uk", "family", "(RottenTomatoes, record|lhistoire_de_richard_o)")))
    java.nio.file.Files.readString(path) shouldBe "country\tkind\tlookup\nuk\tfamily\t(RottenTomatoes, record|lhistoire_de_richard_o)\n"
  }
}
