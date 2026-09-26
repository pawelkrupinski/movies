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
 * **Every GitHub-release write retries GitHub's transient answers.** The upsert did
 * not end the 403s: on 2026-09-23 an `edit` of the existing `android-latest` release
 * met the same `Resource not accessible by integration`, with no concurrent writer
 * (the run's duplicate `push` sibling was cancelled at t+3s) and the step green on
 * the runs either side. So every `gh release` write in the workflows and composite
 * actions goes through `scripts/ci/gh-release.sh`, which retries 403/409/5xx with
 * back-off; `scripts/ci/gh-release-test.sh` exercises it against a stub `gh`.
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

  private lazy val rollingPublish = RepoFile.read("scripts/ci/publish-rolling-release.sh")

  "the android workflow" should "never delete a rolling release to republish it" in {
    withClue("a delete+create leaves the tag missing; see the 403 in this spec's doc: ") {
      android should not include "gh release delete"
      rollingPublish should not include ("\"$release\" delete")
    }
  }

  it should "publish both rolling releases through the upserting, retrying script" in {
    for (tag <- Seq("android-latest", "android-tune-latest")) withClue(s"$tag: ") {
      android should include (s"scripts/ci/publish-rolling-release.sh\" $tag")
    }
    for (verb <- Seq("view", "create", "upload"))
      rollingPublish should include (s"\"$$release\" $verb")
    rollingPublish should include ("--clobber")
    // An existing release is never edited: `release edit` drew GitHub's intermittent 403 under
    // Contents: write, and nothing it wrote was needed (see publish-rolling-release.sh).
    rollingPublish should not include ("\"$release\" edit")
  }

  "every workflow and composite action" should "write GitHub releases only through the retrying gh-release.sh" in {
    val direct = """gh release (create|edit|upload|delete)""".r
    val offenders = for {
      dir  <- Seq(".github/workflows", ".github/actions")
      file <- Option(new java.io.File(dir).listFiles).toSeq.flatten.flatMap { f =>
                if (f.isDirectory) Option(f.listFiles).toSeq.flatten else Seq(f)
              }
      if file.getName.endsWith(".yml") || file.getName.endsWith(".yaml")
      line <- RepoFile.read(file.getPath).linesIterator
      if !line.trim.startsWith("#") && direct.findFirstIn(line).isDefined
    } yield s"${file.getPath}: ${line.trim}"
    withClue("a bare `gh release` write turns one of GitHub's transient 403s into a red run: ") {
      offenders shouldBe empty
    }
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
