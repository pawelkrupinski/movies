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
 * CI sets no versionCode at all, and publishes nothing to Play: a run number is far below the
 * epoch-second codes store releases set locally (infra/version-dashboard/src/mobile-release/release.ts,
 * pinned below together with build.gradle.kts still honouring the override), so a CI upload could
 * only ever be refused.
 */
class AndroidBuildCacheVersionCodeSpec extends AnyFlatSpec with Matchers {

  private lazy val android = RepoFile.read(".github/workflows/android.yml")
  private val versionCodeLine = """(?m)^\s*KINOWO_VERSION_CODE:\s*(.+)$""".r

  "the android workflow" should "give the release assemble no per-run versionCode" in {
    withClue("a per-run versionCode makes R8, lint and packaging miss the build cache: ") {
      versionCodeLine.findAllMatchIn(android).map(_.group(1).trim).toList shouldBe empty
    }
  }

  it should "publish nothing to Google Play, which would refuse a CI run number's versionCode" in {
    val commands = android.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    commands should not include "PLAY_SERVICE_ACCOUNT_JSON"
    commands should not include "publishReleaseBundle"
    commands should not include "tag-mobile-release.sh android"
  }

  "the store release" should "still set its own versionCode, which the Gradle build honours" in {
    RepoFile.read("infra/version-dashboard/src/mobile-release/release.ts") should include (
      "KINOWO_VERSION_CODE: String(versionCode)"
    )
    // The read itself, minus its receiver: spelled out whole, this line is a direct
    // process read as far as ProcessAccessLintSpec can tell, and fails it.
    RepoFile.read("android/app/build.gradle.kts") should include (
      """.getenv("KINOWO_VERSION_CODE")?.toIntOrNull() ?: mobileVersionCode"""
    )
  }
}
