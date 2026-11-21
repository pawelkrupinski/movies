package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The old refs' dependency cache (.github/actions/old-ref-deps). The old-ref compile (the landing check's `oldrefs`
 * until 2026-11-21, Main's `old-ref-compile` row since) compiles the identity tools into pinned old refs, each with its
 * own build (sbt 1.13.0 where main is on 1.13.1), which main's dependency cache does not hold: every check downloaded
 * them from Maven Central, and its 429s failed the check (run 44807195909, 2026-11-20). One entry keyed by the floors
 * holds them; main's old-ref compile saves it, every run restores it and then runs offline, so a compile of an old ref
 * no longer depends on Maven Central answering.
 */
class OldRefDepsCacheSpec extends AnyFlatSpec with Matchers {
  private val Action    = ".github/actions/old-ref-deps/action.yml"
  private val Main = ".github/workflows/main.yml"

  private lazy val action = RepoFile.withoutComments(RepoFile.read(Action))

  "the old refs' dependency action" should "key the entry by the floors and restore it over the dependency cache's paths" in {
    action should include("scripts/ci/old-ref-deps-key.sh")
    // the same paths as the dependency cache: GitHub versions an entry by its path list, and the floors' builds look
    // where main's restored them
    action should include("uses: ./.github/actions/sbt-deps-key")
    action should include("path: ${{ steps.paths.outputs.paths }}")
    action should include("restore-keys: ${{ steps.key.outputs.prefix }}")
  }

  it should "restore through the runner-local mirror, never saving" in {
    action should include("uses: ./.github/actions/cache-restore")
    action should not include ("actions/cache/save@")
    action should not include ("uses: actions/cache@")
  }

  it should "run the job's JVMs offline on an exact hit, and only then" in {
    val offline = RepoFile.step(action, "Run the job's JVMs offline (every old-ref dependency restored)")
    offline should include("if: steps.restore.outputs.cache-hit == 'true'")
    offline should include("scripts/ci/jvm-offline-env.sh\" >> \"$GITHUB_ENV\"")
  }

  "the old-ref compile" should "run whenever GitHub holds no entry, the plan asking with the same key and paths" in {
    val yml  = RepoFile.read(Main)
    val plan = RepoFile.withoutComments(RepoFile.jobs(yml)("gates"))
    plan should include("scripts/ci/old-ref-deps-key.sh >> \"$GITHUB_OUTPUT\"")
    plan should include("uses: ./.github/actions/sbt-deps-key")
    plan should include("lookup-only: 'true'")
    val decide = RepoFile.step(plan, "Does the old-ref compile run?")
    decide should include("HIT: ${{ steps.old-ref-deps-lookup.outputs.cache-hit }}")
    decide should include("""elif [ "$HIT" != true ]; then echo "run=true"""")
    val job = RepoFile.withoutComments(RepoFile.jobs(yml)("old-ref-compile"))
    job should include("if: ${{ needs.gates.outputs.old-ref-compile == 'true' }}")
  }

  it should "restore the entry before the compile, and make every floor's sbt resolve" in {
    val job = RepoFile.withoutComments(RepoFile.jobs(RepoFile.read(Main))("old-ref-compile"))
    val at  = RepoFile.positionOf(job, "uses: ./.github/actions/old-ref-deps")
    RepoFile.positionOf(job, "uses: ./.github/actions/setup-sbt-build") should be < at
    val resolve = RepoFile.step(job, "Make every floor's build resolve")
    resolve should include("target/identity-tools/classpath")
    at should be < RepoFile.positionOf(job, "- name: Make every floor's build resolve")
    RepoFile.positionOf(job, "- name: Make every floor's build resolve") should be <
      RepoFile.positionOf(job, "bash scripts/identity-slice-old-ref-test.sh")
  }

  it should "save it once, from a green compile of every floor, on main only, its failed downloads forgotten" in {
    val job  = RepoFile.withoutComments(RepoFile.jobs(RepoFile.read(Main))("old-ref-compile"))
    val save = RepoFile.step(job, "Keep the old refs' dependencies")
    save should include("uses: actions/cache/save@")
    save should include("github.ref == 'refs/heads/main'")
    save should include("steps.old-ref-verdict.outcome == 'success'")
    save should include("steps.old-ref-deps.outputs.cache-hit != 'true'")
    save should include("path: ${{ steps.old-ref-deps.outputs.paths }}")
    save should include("key: ${{ steps.old-ref-deps.outputs.key }}")
    val forget = RepoFile.step(job, "Forget coursier's failed downloads before the old refs' dependencies are kept")
    forget should include("scripts/ci/coursier-forget-errors.sh")
    RepoFile.positionOf(job, "- name: End the old-ref compile's JVMs") should be <
      RepoFile.positionOf(job, "- name: Forget coursier's failed downloads")
    RepoFile.positionOf(job, "- name: Forget coursier's failed downloads") should be <
      RepoFile.positionOf(job, "- name: Keep the old refs' dependencies")
  }
}
