package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A push or PR build of the Android app keeps a STABLE versionCode, so its release
 * tasks come from the Gradle build cache.
 *
 * The workflow used to pass `KINOWO_VERSION_CODE: ${{ github.run_number }}` to the
 * release assemble on every run. The versionCode lands in the merged manifest, so
 * every task downstream of it — `minifyReleaseWithR8`, `lintVitalAnalyze*`, resource
 * linking and optimisation, packaging, for both `release` and `tuneRelease` — had a
 * new cache key each run and re-executed: run 36976986710 logged 94 executed / 15
 * from cache for that step (276 s). Locally, a second build after `clean` took 182 s
 * with a new code and 7 s with the same one (R8 and lint FROM-CACHE).
 *
 * Only a manual (`workflow_dispatch`) run, the one that may publish to Play, still
 * gets the run number. Store releases set their own epoch-second code locally
 * (infra/version-dashboard/src/mobile-release/release.ts), which this spec also
 * pins, together with build.gradle.kts still honouring the override.
 */
class AndroidBuildCacheVersionCodeSpec extends AnyFlatSpec with Matchers {

  private lazy val android = RepoFile.read(".github/workflows/android.yml")
  private lazy val assembleRelease = RepoFile.step(android, "Assemble release APK + AAB")
  private val versionCodeLine = """(?m)^\s*KINOWO_VERSION_CODE:\s*(.+)$""".r

  "the android workflow's release assemble" should "pass the run number only on a manual run" in {
    val values = versionCodeLine.findAllMatchIn(assembleRelease).map(_.group(1).trim).toList
    withClue("a per-run versionCode on push/PR makes R8, lint and packaging miss the build cache: ") {
      values should not be empty
      all(values) should include ("github.event_name == 'workflow_dispatch' && github.run_number")
      all(values) should not equal "${{ github.run_number }}"
    }
  }

  "the store release" should "still set its own versionCode, which the Gradle build honours" in {
    RepoFile.read("infra/version-dashboard/src/mobile-release/release.ts") should include (
      "KINOWO_VERSION_CODE: String(versionCode)"
    )
    RepoFile.read("android/app/build.gradle.kts") should include (
      """versionCode = System.getenv("KINOWO_VERSION_CODE")?.toIntOrNull() ?: mobileVersionCode"""
    )
  }
}
