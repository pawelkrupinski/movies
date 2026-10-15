package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A capture's GAPS: the lookups a replay asks that the capture holds no answer to. The ratchet fails on any (a stale
 *  capture, re-captured); a MEASUREMENT reads them as unknown and names them, so a change that asks a new lookup
 *  (class 4's relay season searches, 2026-10-15: run 38694072183 lost every country's final takes to one UK RT record)
 *  is still measured, its blind spot listed beside the result. */
class UnmatchedClustersGapsSpec extends AnyFlatSpec with Matchers {

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
    val fixtures = java.nio.file.Files.createTempDirectory("ratchet-fixtures")
    UnmatchedClusters.write(fixtures.resolve("es.json.gz"), es.copy(queries = Map.empty))
    def run(measure: Boolean) = {
      val out = java.nio.file.Files.createTempDirectory("ratchet-out")
      val run = new scripts.UnmatchedClustersRatchet.Run(fixtures, IdentityJudgements.CheckedIn, out, measure)
      (run.reported, out)
    }
    val (measured, out) = run(measure = true)
    measured.isRight shouldBe true
    java.nio.file.Files.readString(out.resolve("capture-gaps.tsv")) should include("es\ttmdb\t")
    java.nio.file.Files.exists(out.resolve("ratchet-takes.tsv")) shouldBe true
    val (judged, notMeasured) = run(measure = false)
    judged.left.toOption.get should startWith("es: the fixture lacks answers this resolve asks")
    java.nio.file.Files.exists(notMeasured.resolve("ratchet-takes.tsv")) shouldBe false
  }

  it should "write each country's gaps as a TSV row a measurement lists" in {
    val path = java.nio.file.Files.createTempFile("capture-gaps", ".tsv")
    UnmatchedClusters.writeGaps(path, Seq(UnmatchedClusters.Gap("uk", "family", "(RottenTomatoes, record|lhistoire_de_richard_o)")))
    java.nio.file.Files.readString(path) shouldBe "country\tkind\tlookup\nuk\tfamily\t(RottenTomatoes, record|lhistoire_de_richard_o)\n"
  }
}
