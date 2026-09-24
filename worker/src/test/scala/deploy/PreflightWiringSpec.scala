package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The workflows that cannot succeed without a secret check it FIRST, in a seconds-long
 * `preflight` job that everything consuming the secret waits for.
 *
 * Several red runs in the four weeks to 2026-09-24 were configuration, not code: a signing
 * secret missing, an Android SDK licence unaccepted. They surfaced minutes in, reading like
 * build failures — an unsigned release APK the rolling-release step then refused, Gradle
 * declining to install a platform. `scripts/ci/preflight.sh` names the problem at t=0; this
 * pins the wiring that makes it run first:
 *
 *   - each workflow listed here has a `preflight` job, bounded to minutes, needing nothing;
 *   - every secret it asserts is mapped into its env (an unmapped name would always read
 *     empty and fail every run — or, checked for the wrong name, pass while the real one is
 *     missing);
 *   - every OTHER job that reads one of those secrets needs `preflight`, directly or through
 *     a job that does, so none can start its long work before the check has passed;
 *   - and none of those jobs overrides that wait with a status function (`!cancelled()`,
 *     `always()`) in its `if:` without also asking whether the preflight succeeded.
 *
 * A job calling a reusable workflow with `secrets: inherit` counts as reading what that workflow reads.
 */
class PreflightWiringSpec extends AnyFlatSpec with Matchers {

  /** workflow file → whether its preflight must also check the Android SDK licence. */
  private val Guarded = Map(
    ".github/workflows/android.yml" -> true,
    ".github/workflows/main.yml"    -> false,
    ".github/workflows/us-convergence.yml"         -> false,
    ".github/workflows/country-convergence.yml"    -> false,
    ".github/workflows/convergence-bisect.yml"     -> false,
    ".github/workflows/record-scrape-fixtures.yml" -> false
  )

  private def needs(job: String): Set[String] =
    job.linesIterator.map(_.trim).collectFirst { case s"needs: $n" => n }
      .map(_.stripPrefix("[").stripSuffix("]").split(",").map(_.trim).filter(_.nonEmpty).toSet)
      .getOrElse(Set.empty)

  private def secretsNamed(text: String): Set[String] =
    """secrets\.([A-Z][A-Z0-9_]*)""".r.findAllMatchIn(text).map(_.group(1)).toSet

  /** The job's own `if:` — at the indentation of its other keys, never a step's. */
  private def jobIf(job: String): Option[String] = {
    val keyIndent = job.linesIterator.drop(1).find(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length)
    job.linesIterator.collectFirst {
      case line if keyIndent.contains(line.takeWhile(_ == ' ').length) && line.trim.startsWith("if: ") =>
        line.trim.stripPrefix("if: ")
    }
  }

  /** A job calling a reusable workflow with `secrets: inherit` reads whatever that workflow reads. */
  private def secretsRead(job: String): Set[String] = {
    val called = job.linesIterator.map(_.trim).collectFirst { case s"uses: ./$path.yml" => s"$path.yml" }
    val inherited =
      if (job.linesIterator.exists(_.trim == "secrets: inherit")) called.map(p => secretsNamed(RepoFile.read(p))).getOrElse(Set.empty)
      else Set.empty
    secretsNamed(job) ++ inherited
  }

  /** The names on the preflight.sh command lines — every all-caps word after the script. */
  private def asserted(preflight: String): Set[String] = {
    val script = preflight.linesIterator.dropWhile(!_.contains("scripts/ci/preflight.sh")).mkString(" ")
    """\b([A-Z][A-Z0-9_]{2,})\b""".r.findAllMatchIn(script).map(_.group(1)).toSet
  }

  Guarded.foreach { case (file, androidLicence) =>
    lazy val all       = RepoFile.jobs(RepoFile.read(file))
    lazy val preflight = all.getOrElse("preflight", fail(s"$file has no `preflight` job"))

    s"$file" should "have a preflight job that starts at once and runs for minutes at most" in {
      needs(preflight) shouldBe empty
      val timeout = preflight.linesIterator.map(_.trim).collectFirst { case s"timeout-minutes: $n" => n.toInt }
      timeout.getOrElse(fail("preflight has no timeout-minutes")) should be <= 5
      preflight should include("scripts/ci/preflight.sh")
    }

    it should "map every secret its preflight asserts into the step's env" in {
      val names = asserted(preflight)
      names should not be empty
      names.foreach { name =>
        withClue(s"$name: ")(preflight should include(s"$name: $${{ secrets.$name }}"))
      }
    }

    if (androidLicence) it should "check the Android SDK licence the build's SDK set-up leaves" in {
      preflight should include("--android-sdk-licence")
      preflight should include("android-actions/setup-android")
    }

    it should "make every job reading an asserted secret wait for the preflight" in {
      val guardedSecrets = asserted(preflight)
      def waitsForPreflight(job: String, seen: Set[String] = Set.empty): Boolean =
        needs(all(job)).exists(n => n == "preflight" || (!seen(n) && all.contains(n) && waitsForPreflight(n, seen + job)))
      val unguarded = all.collect {
        case (name, body) if name != "preflight" && (secretsRead(body) & guardedSecrets).nonEmpty && !waitsForPreflight(name) => name
      }
      withClue("jobs reading a preflight-checked secret without waiting for it: ")(unguarded shouldBe empty)
    }

    // A status function in `if:` (`!cancelled()`, `always()`, `failure()`) replaces the implicit
    // `success()` over `needs`, so waiting alone no longer stops the job: a failed preflight
    // skips its dependants and the job starts anyway, without the secret. It has to ask.
    it should "not start a job reading an asserted secret once the preflight has failed" in {
      val guardedSecrets = asserted(preflight)
      val StatusFunction = """(always|cancelled|failure)\(\)""".r
      val ungated = all.collect {
        case (name, body) if name != "preflight" && (secretsRead(body) & guardedSecrets).nonEmpty &&
            jobIf(body).exists(cond => StatusFunction.findFirstIn(cond).isDefined && !cond.contains("needs.preflight.result == 'success'")) => name
      }
      withClue("jobs whose `if:` lets them run past a failed preflight: ")(ungated shouldBe empty)
    }
  }
}
