package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.DecorationScore

import java.nio.file.Files

/** The shipped candidate-decoration score is what `IdentityDecorationScoreFit` fits from the checked-in training rows
 *  — every learned weight has its re-learn tool, and the artefact is never edited by hand. */
class IdentityDecorationScoreFitSpec extends AnyFlatSpec with Matchers {

  "The shipped decoration score" should "be the fit of the checked-in training rows" in {
    Files.exists(IdentityDecorationScoreFit.Training) shouldBe true
    val shipped = DecorationScore.fromResource().getOrElse(fail(s"${DecorationScore.ResourcePath} is not on the classpath"))
    DecorationScore.fit(IdentityDecorationScoreFit.read(IdentityDecorationScoreFit.Training),
      IdentityDecorationScoreFit.versionOf(IdentityDecorationScoreFit.Training)) shouldBe shipped
  }

  "A training row" should "read back as it was written" in {
    val row = DecorationScore.Row("suffix:edukacja mlode horyzonty", DecorationScore.Features(2, 3, 1, prefix = false, 3, 0.0, 0.3333, 0.25),
      good = true, bad = false)
    val file = Files.createTempFile("training", ".tsv")
    try {
      Files.writeString(file, IdentityDecorationScoreFit.Header + "\n" + IdentityDecorationScoreFit.line(row) + "\n")
      IdentityDecorationScoreFit.read(file) shouldBe Seq(row)
    } finally Files.delete(file)
  }
}
