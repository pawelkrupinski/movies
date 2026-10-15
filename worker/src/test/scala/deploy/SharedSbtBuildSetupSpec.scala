package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The identity workflows' sbt jobs set up their build through ONE composite action
 *  (`.github/actions/setup-sbt-build`): the JDK, the sbt runner and the build cache were a trio
 *  copy-pasted into each job, the same 12 lines and the same `sbt-target-` key every time, and
 *  copies of a key drift apart. */
class SharedSbtBuildSetupSpec extends AnyFlatSpec with Matchers {
  private val Action = ".github/actions/setup-sbt-build/action.yml"
  private val Adopters = Seq(
    ".github/workflows/identity-capture.yml",
    ".github/workflows/identity-refit.yml",
    ".github/workflows/identity-decorations.yml"
  )

  "the identity workflows" should "set up their sbt build through the shared action, not the trio" in {
    Adopters.foreach { file =>
      val text = RepoFile.withoutComments(RepoFile.read(file))
      withClue(s"$file: ") {
        text should include("uses: ./.github/actions/setup-sbt-build")
        text should not include "uses: ./.github/actions/setup-jdk"
        text should not include "uses: sbt/setup-sbt@"
        text should not include "uses: ./.github/actions/sbt-target-cache"
      }
    }
  }

  "the shared action" should "install the JDK through setup-jdk and cache through sbt-target-cache" in {
    val action = RepoFile.read(Action)
    action should include("uses: ./.github/actions/setup-jdk")
    action should include("uses: sbt/setup-sbt@v1")
    action should include("uses: ./.github/actions/sbt-target-cache")
  }

  it should "key the default build cache as the shared sbt-target family, falling back to its newest entry" in {
    val action = RepoFile.read(Action)
    action should include("format('sbt-target-{0}-{1}', runner.os, hashFiles('build.sbt', 'project/build.properties'")
    action should include("format('sbt-target-{0}-', runner.os)")
  }

  /** Heap is each sbt call's own `-J-Xmx`: one job's calls differ, so a job-wide input would change some. */
  it should "leave the JVM heap to each sbt invocation" in {
    RepoFile.withoutComments(RepoFile.read(Action)) should not include "Xmx"
    RepoFile.read(".github/workflows/identity-refit.yml") should include("sbt -J-Xmx12g \"worker/Test/runMain scripts.IdentityRefit")
  }

  "the decorations relearn" should "restore no build cache, as before the trio was shared" in {
    RepoFile.read(".github/workflows/identity-decorations.yml") should include("target-cache: 'false'")
  }
}
