package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentitySlice

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Real listings, pinned from recordings by `scripts/identity-slice --emit-case`: each directory of
 * `test/resources/fixtures/identity-slices/` is a hermetic slice of a recording — the listings' families, their corpus
 * context and every answer they read — and the film identity-judgements.tsv calls right for each listing (`expected.tsv`). The slice is
 * decided again, model and agreement, from its own answers alone, and each listing must take its right film — or, where
 * the case's expected.tsv says so by hand, no film (`none`) or any film but a wrong one (`not:<film>`).
 */
class IdentitySliceCasesSpec extends AnyFlatSpec with Matchers with tools.CountryScope {

  /** A slice is one country's, its directory's `<cc>-` prefix, which ends its test's name. */
  override protected def countriesOf(testName: String): Seq[String] = Seq(testName.split(' ').last.takeWhile(_ != '-'))

  private val root = Path.of("test", "resources", "fixtures", "identity-slices")
  private val cases: Seq[Path] =
    if (!Files.isDirectory(root)) Nil
    else Files.list(root).iterator().asScala.filter(dir => Files.exists(dir.resolve("expected.tsv"))).toSeq.sortBy(_.getFileName.toString)

  /** Every case's takes, the cases side by side: each decides its own slice from its own answers and shares nothing with
   *  another. One after another, the 68 cases were the worker's longest suite (146 s on CI, run 3419). A case that
   *  fails to decide fails its own test only. */
  private lazy val takesByCase: Map[Path, Try[Map[(String, String), String]]] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    try cases.zip(scripts.ParallelTasks.all(cases)(dir => Try(IdentitySlice.takesOf(dir)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  cases.foreach { dir =>
    "A pinned identity slice" should s"take identity-judgements.tsv's right film for each listing of ${dir.getFileName}" in {
      val takes = takesByCase(dir).get
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
