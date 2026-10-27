package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.AgreementStageComposition

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Every agreement stage the worker and its harnesses build is built through [[AgreementStageComposition]], and a
 *  harness's differs from the worker's by the composition's named overrides alone. The convergence harness, the live
 *  drift check and the capture each built their own, one lookups for both the venues' and TMDB's roles, and none of
 *  them read as production read: prod kept 26 takes a whole resolve made, for days (2026-10-27). */
class AgreementStageCompositionSpec extends AnyFlatSpec with Matchers {
  /** Where a stage may be constructed directly, and why. */
  private val Constructs = Map(
    "identity/src/main/scala/services/identity/agreement/AgreementStageComposition.scala" -> "the composition itself",
    // identity-slice lays this file into refs older than the composition (its TOOL_FILES), where it must compile
    "worker/src/test/scala/scripts/IdentitySlice.scala" -> "compiled into refs that predate the composition")

  private val Roots = Seq("identity/src/main", "worker/src/main", "worker/src/fixtures", "worker/src/test", "worker/src/it", "web/src/main")
  private def sources: Seq[Path] = Roots.map(Paths.get(_)).filter(Files.isDirectory(_)).flatMap(root =>
    Files.walk(root).iterator.asScala.filter(p => p.toString.endsWith(".scala") && !p.getFileName.toString.endsWith("Spec.scala")).toSeq)
  private val Constructor = """new\s+(services\.identity\.agreement\.)?AgreementStage\s*\(""".r

  "An agreement stage" should "be built through the one composition, but where this spec names why" in {
    val building = sources.filter(p => Constructor.findFirstIn(Files.readString(p)).isDefined).map(_.toString).toSet
    building shouldBe Constructs.keySet
  }

  "The composition" should "allow a harness no override but the named ones, each saying why no take moves by it" in {
    val declared = """(?m)^\s*case\s+(\w+)[^\n]*extends Override\(""".r
      .findAllMatchIn(Files.readString(Paths.get("identity/src/main/scala/services/identity/agreement/AgreementStageComposition.scala")))
      .map(_.group(1)).toSeq
    declared shouldBe Seq("Unbudgeted", "CorrectionsPerApply")
    Seq(AgreementStageComposition.Override.Unbudgeted, AgreementStageComposition.Override.CorrectionsPerApply(1)).foreach(_.reason should not be empty)
  }
}
