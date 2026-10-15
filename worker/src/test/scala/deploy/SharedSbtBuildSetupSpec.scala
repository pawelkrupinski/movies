package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The sbt jobs set up their build through ONE composite action (`.github/actions/setup-sbt-build`):
 *  the JDK, the sbt runner and the build cache were a trio copy-pasted into each job, the same 12
 *  lines and the same `sbt-target-` key every time, and copies of a key drift apart. The convergence
 *  legs' `convergence-setup` is the deliberate exception: it keys off git's index and skips the sbt-2
 *  disk cache, because every workspace glob walks a restored enrichment tree of up to 127k files. */
class SharedSbtBuildSetupSpec extends AnyFlatSpec with Matchers {
  private val Action = ".github/actions/setup-sbt-build/action.yml"
  private val Adopters = Seq(
    ".github/workflows/identity-capture.yml",
    ".github/workflows/identity-refit.yml",
    ".github/workflows/identity-decorations.yml",
    ".github/workflows/ci.yml",
    ".github/workflows/nightly-stress.yml",
    ".github/workflows/discover-cinemas.yml"
  )

  "the sbt workflows" should "set up their sbt build through the shared action, not the trio" in {
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
    // The refit's job is one script now; its JVM sets its own heap.
    RepoFile.read("scripts/identity-refit") should include("-Xmx${IDENTITY_REFIT_HEAP:-12g}")
  }

  /** Moving onto the action kept each job's cache entry: the shared family where the job compiled the it scope
   *  (the action's default key), and its own key where it had one. */
  "Main's sbt jobs" should "keep the build cache keys they had before the trio was shared" in {
    val ci = RepoFile.read(".github/workflows/ci.yml")
    val unit = RepoFile.jobs(ci)("test")
    unit should include("target-cache-key: sbt-target-${{ runner.os }}-${{ hashFiles('build.sbt', 'project/build.properties', 'project/plugins.sbt', '**/src/main/**/*.scala', '**/src/test/**/*.scala', 'web/src/main/resources/**') }}")
    unit should include("sbt-target-${{ runner.os }}-")
    val android = RepoFile.jobs(ci)("mobile-local-server")
    android should include("java-version: 21")
    android should include("target-cache-key: sbt-target-android-ls-${{ runner.os }}-")
    RepoFile.jobs(ci)("integration-test") should not include "target-cache-key"
    val e2e = RepoFile.jobs(ci)("e2e")
    e2e should include("target-cache-key: sbt-target-e2e-${{ runner.os }}-${{ steps.sbt-key.outputs.e2e }}")
    e2e.indexOf("id: sbt-key") should be < e2e.indexOf("uses: ./.github/actions/setup-sbt-build")
    val stress = RepoFile.read(".github/workflows/nightly-stress.yml")
    stress should include("target-cache-restore-only: 'true'")
    stress should not include "target-cache-key"
    RepoFile.read(".github/workflows/discover-cinemas.yml") should not include "target-cache-key"
  }

  "the decorations relearn" should "restore no build cache, as before the trio was shared" in {
    RepoFile.read(".github/workflows/identity-decorations.yml") should include("target-cache: 'false'")
  }

  "a caller's own target-cache key" should "fall back only to the prefixes it names, never the shared family" in {
    RepoFile.read(Action) should include(
      "restore-keys: ${{ inputs.target-cache-key == '' && format('sbt-target-{0}-', runner.os) || inputs.target-cache-restore-keys }}")
  }
}
