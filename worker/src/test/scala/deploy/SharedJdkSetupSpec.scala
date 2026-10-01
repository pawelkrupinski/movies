package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every workflow and composite action must resolve a JDK through the shared
 * `.github/actions/setup-jdk`, never `actions/setup-java` directly.
 *
 * JDK 27 isn't preinstalled on hosted runners, so every job that needs it
 * resolves and downloads it over the network; a transient error there (an
 * `ECONNRESET` while `setup-java` was "Trying to resolve the latest version
 * from remote", as in run 36327812746) otherwise fails a job for a reason
 * that has nothing to do with the code under test. `setup-jdk` wraps
 * `actions/setup-java` with retries so that blip doesn't fail the build; a
 * direct `actions/setup-java` call bypasses the retry this repo settled on,
 * so there should be exactly one such call left -- inside `setup-jdk` itself.
 */
class SharedJdkSetupSpec extends AnyFlatSpec with Matchers {
  private val SetupJavaUse = """uses:\s*actions/setup-java@""".r
  private val sharedAction = ".github/actions/setup-jdk/action.yml"

  "every workflow" should "set up its JDK through the shared setup-jdk action" in {
    val workflows = RepoFile.workflows()
    workflows should not be empty

    val direct = for {
      file <- workflows
      line <- RepoFile.read(file.getPath).linesIterator.filterNot(_.trim.startsWith("#"))
      if SetupJavaUse.findFirstIn(line).isDefined
    } yield s"${file.getName}: ${line.trim}"

    withClue(s"use ./.github/actions/setup-jdk instead, so a transient network failure retries: ") {
      direct shouldBe empty
    }
  }

  "every composite action other than setup-jdk itself" should "set up its JDK through the shared setup-jdk action" in {
    val actions = RepoFile.compositeActions().filterNot(_ == sharedAction)
    actions should not be empty

    val direct = for {
      path <- actions
      line <- RepoFile.read(path).linesIterator.filterNot(_.trim.startsWith("#"))
      if SetupJavaUse.findFirstIn(line).isDefined
    } yield s"$path: ${line.trim}"

    withClue(s"use ./.github/actions/setup-jdk instead, so a transient network failure retries: ") {
      direct shouldBe empty
    }
  }

  "the shared setup-jdk action" should "exist and still be the one composite action calling actions/setup-java" in {
    RepoFile.compositeActions() should contain(sharedAction)
    val hits = SetupJavaUse.findAllIn(RepoFile.read(sharedAction)).length
    withClue("setup-jdk retries three times, so it should call actions/setup-java exactly three times: ") {
      hits shouldBe 3
    }
  }

  /**
   * setup-java's own sbt cache has no fallback key, so a build.sbt edit sent every runner of a
   * push to Maven Central at once for sbt and all its dependencies, and Central answered the
   * burst with 403s (run 36807612216). The cache lives in setup-jdk with a prefix restore-key
   * instead, and setup-java must not be handed `sbt` as well.
   */
  it should "cache sbt's dependencies with a fallback key, not through setup-java" in {
    val action = RepoFile.read(sharedAction)
    action should include("key: sbt-deps-${{ runner.os }}-")
    action should include("restore-keys: |\n          sbt-deps-${{ runner.os }}-")
    action should not include "cache: ${{ inputs.cache }}"
  }
}
