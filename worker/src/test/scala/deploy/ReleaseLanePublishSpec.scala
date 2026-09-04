package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the two release lanes against the shapes that broke them.
 *
 * **Android's rolling releases are UPSERTED, never deleted and recreated.** Both
 * `android-latest` and `android-tune-latest` used to run
 * `gh release delete … --cleanup-tag || true` and then `gh release create`. That
 * leaves a window in which the tag does not exist, and on 2026-09-04 GitHub
 * answered one of those windows with `HTTP 403: Resource not accessible by
 * integration` — an error that reads like a missing permission but is not:
 * `contents: write` is declared on the job, and the same step succeeded on the
 * runs either side of it. Every push to `main` showed Android red for it. An
 * upsert (`view` → `edit` or `create`, then `upload --clobber`) never drops the
 * tag, so there is no window to lose.
 *
 * **The iOS archive lane is a SCRIPT.** Three releases ran without one, each
 * rebuilt by hand from a memory note, which is how the manual-signing flags and
 * the ExportOptions plist kept having to be rediscovered. `xcodebuild archive`
 * needs `CODE_SIGN_STYLE=Manual` here — the automatic style picks a development
 * certificate and the export then fails on the mismatch — so the flags are
 * pinned rather than left to whoever runs it next.
 */
class ReleaseLanePublishSpec extends AnyFlatSpec with Matchers {

  private lazy val android = RepoFile.read(".github/workflows/android.yml")
  private lazy val iosScript = RepoFile.read("scripts/ios-release.sh")

  "the android workflow" should "never delete a rolling release to republish it" in {
    withClue("a delete+create leaves the tag missing; see the 403 in this spec's doc: ") {
      android should not include "gh release delete"
    }
  }

  it should "upsert both rolling releases and clobber their assets" in {
    for (tag <- Seq("android-latest", "android-tune-latest")) withClue(s"$tag: ") {
      android should include (s"gh release view $tag")
      android should include (s"gh release edit $tag")
      android should include (s"gh release create $tag")
      android should include (s"gh release upload $tag")
    }
    android should include ("--clobber")
  }

  it should "still declare the contents:write the upload needs" in {
    android should include ("contents: write")
  }

  "the iOS release script" should "sign manually with the App Store profile" in {
    iosScript should include ("CODE_SIGN_STYLE=Manual")
    iosScript should include ("""CODE_SIGN_IDENTITY="Apple Distribution"""")
    iosScript should include ("Kinowo App Store")
    iosScript should include ("CQ4YC43YDM")
    iosScript should include ("<key>method</key><string>app-store-connect</string>")
  }

  it should "run the whole lane — test, archive, export, validate, upload" in {
    for (step <- Seq("swift test", "xcodebuild archive", "-exportArchive",
                     "--validate-app", "--upload-app"))
      withClue(s"$step: ") { iosScript should include (step) }
  }

  it should "never source .env.local, whose values break a shell parse" in {
    // One value contains `&`; `source`-ing it kills zsh. The script greps the
    // two vars out instead.
    iosScript should not include "source .env.local"
    iosScript should not include ". .env.local"
    iosScript should include ("APP_STORE_KEY_ID")
    iosScript should include ("APP_STORE_ISSUER_ID")
  }
}
