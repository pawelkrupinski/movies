package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The registry every image goes to is named in the workflow itself, never through a variable that may be unset. */
class RegistryNameSpec extends AnyFlatSpec with Matchers {
  private lazy val workflows = RepoFile.listed(".github/workflows")(_.getName.endsWith(".yml"))
    .map(f => f.getName -> RepoFile.read(s".github/workflows/${f.getName}"))

  // Main #2030 (2026-10-31): the registry came from repository variables that existed only on one forge, with ghcr.io as
  // the fallback. When github.com turned primary it logged in to ghcr.io with registry.kinowo.net's password --
  // `denied` -- and the e2e corpus row never built the image. A registry must never hang on a variable being set: every
  // workflow that logs in to a registry names registry.kinowo.net (the one Flux scans) itself, with its own password.
  "every workflow that pushes an image" should "name registry.kinowo.net itself, never via a variable or a ghcr.io fallback" in {
    val pushers = workflows.filter { case (_, yml) =>
      val code = RepoFile.withoutComments(yml)
      code.contains("docker/login-action") || code.contains("docker login")
    }
    pushers.map(_._1).sorted shouldBe Seq("ci.yml", "main.yml")
    for ((name, yml) <- pushers) withClue(name) {
      val env = RepoFile.withoutComments(RepoFile.block(yml, "env")).linesIterator.map(_.trim).toSeq
      env should contain("IMAGE_REPO: registry.kinowo.net")
      env should contain("REGISTRY_HOST: registry.kinowo.net")
      env should contain("REGISTRY_USER: ci")
      val code = RepoFile.withoutComments(yml)
      code should not include "vars.IMAGE_REPO"
      code should not include "vars.REGISTRY_"
      code should not include "ghcr.io"
      code should not include "secrets.REGISTRY_PASSWORD ||"
    }
  }}
