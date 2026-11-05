package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * THE FAMILY CHECK OVER EVERY DRIFT SLICE ([[FamilyConvergence]]): each committed drift slice's families walked over their
 * population lattice from the slice's own answers alone. A slice is cut over that lattice too
 * ([[scripts.IdentityDrift.cut]]), so a sub-population's question is the slice's answer, never a gap read as unknown:
 * the walk lacks nothing.
 */
class FamilyConvergenceSlicesSpec extends AnyFlatSpec with Matchers {
  private val root   = Path.of("test", "resources", "fixtures", "drift-slices")
  private val slices = Files.list(root).iterator.asScala.filter(Files.isDirectory(_)).flatMap(cc => Files.list(cc).iterator.asScala)
    .filter(d => Files.exists(d.resolve("slice.json.gz"))).toSeq.sortBy(_.toString)

  slices.foreach { dir =>
    "A drift slice" should s"answer every question its families' lattice asks: ${root.relativize(dir)}" in {
      val run = FamilyConvergence.check(FamilyConvergence.sliceCorpus(dir, "slices"), Map.empty, FamilyConvergence.Options())
      withClue("cut it again (scripts/identity-slice --emit-drift-slice): ")(run.lacking shouldBe empty)
    }
  }
}
