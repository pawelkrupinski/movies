package deploy

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.sys.process.Process

/** The cases of [[FluxImageAutomationSpec]] that run the shell CI runs (bash) to build the image tag. */
class FluxImageAutomationIntegrationSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  import FluxImageAutomationSpec.policyPattern

  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  /** The `run:` body of the step that builds the sortable tag, verbatim. */
  private def orderableTagScript: String =
    RepoFile.stepScript(mainYml, "Compute an orderable image tag")

  /** The tag CI would actually publish for `sha`, produced by running CI's own shell. */
  private def tagBuiltByCi(sha: String): String = {
    val script = tempFile("orderable-tag", ".sh")
    val output = tempFile("github-output", ".txt")
    Files.writeString(script, orderableTagScript.replace("${{ github.sha }}", sha))
    val rc = Process(Seq("bash", script.toString), None, "GITHUB_OUTPUT" -> output.toString).!
    rc shouldBe 0
    Files.readString(output).trim.stripPrefix("value=")
  }

  "the tag CI publishes" should "be one the ImagePolicy will actually deploy" in {
    val tag = tagBuiltByCi("b5af6c9f17c2a2b42bff9066db3507f296ebb257")
    withClue(s"CI builds `$tag`, policy selects `$policyPattern`: ") {
      tag should fullyMatch regex policyPattern
    }
  }

  it should "carry the short commit SHA, so a deployed tag still says which commit it is" in {
    tagBuiltByCi("b5af6c9f17c2a2b42bff9066db3507f296ebb257") should endWith("-b5af6c9")
  }
}
