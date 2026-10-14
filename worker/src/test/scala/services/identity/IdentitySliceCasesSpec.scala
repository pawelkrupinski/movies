package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentitySlice

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * Real listings, pinned from recordings by `scripts/identity-slice --emit-case`: each directory of
 * `test/resources/fixtures/identity-slices/` is a hermetic slice of a recording — the listings' families, their corpus
 * context and every answer they read — and the film identity-judgements.tsv calls right for each listing (`expected.tsv`). The slice is
 * decided again, model and agreement, from its own answers alone, and each listing must take its right film — or, where
 * the case's expected.tsv says so by hand, no film (`none`) or any film but a wrong one (`not:<film>`).
 */
class IdentitySliceCasesSpec extends AnyFlatSpec with Matchers {

  private val root = Path.of("test", "resources", "fixtures", "identity-slices")
  private val cases: Seq[Path] =
    if (!Files.isDirectory(root)) Nil
    else Files.list(root).iterator().asScala.filter(dir => Files.exists(dir.resolve("expected.tsv"))).toSeq.sortBy(_.getFileName.toString)

  cases.foreach { dir =>
    "A pinned identity slice" should s"take identity-judgements.tsv's right film for each listing of ${dir.getFileName}" in {
      val takes = IdentitySlice.takesOf(dir)
      IdentitySlice.readExpected(dir).foreach { expected =>
        withClue(s"${expected.venue} | ${expected.rawTitle} (label: ${expected.note}): ") {
          val take = takes.get((expected.venue, expected.rawTitle))
          def check() = withClue(s"takes $take, expected ${expected.film}")(expected.holds(take) shouldBe true)
          if (expected.pending) pendingUntilFixed(check()) else check()
        }
      }
    }
  }
}
