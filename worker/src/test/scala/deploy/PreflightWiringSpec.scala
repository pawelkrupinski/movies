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
 *     a job that does, so none can start its long work before the check has passed.
 *
 * The convergence workflows need one too; they are owned by a separate change and are not
 * listed yet.
 */
class PreflightWiringSpec extends AnyFlatSpec with Matchers {

  /** workflow file → whether its preflight must also check the Android SDK licence. */
  private val Guarded = Map(
    ".github/workflows/android.yml" -> true,
    ".github/workflows/main.yml"    -> false
  )

  private def jobs(yml: String): Map[String, String] = {
    val jobsBlock = RepoFile.block(yml, "jobs")
    val Header    = """^(\s+)([A-Za-z][\w-]*):\s*$""".r
    val topIndent = jobsBlock.linesIterator.drop(1)
      .collectFirst { case Header(indent, _) => indent.length }
      .getOrElse(fail("`jobs:` has no job under it"))
    jobsBlock.linesIterator
      .collect { case line @ Header(indent, name) if indent.length == topIndent => name }
      .map(name => name -> RepoFile.block(jobsBlock, name))
      .toMap
  }

  private def needs(job: String): Set[String] =
    job.linesIterator.map(_.trim).collectFirst { case s"needs: $n" => n }
      .map(_.stripPrefix("[").stripSuffix("]").split(",").map(_.trim).filter(_.nonEmpty).toSet)
      .getOrElse(Set.empty)

  private def secretsRead(job: String): Set[String] =
    """secrets\.([A-Z][A-Z0-9_]*)""".r.findAllMatchIn(job).map(_.group(1)).toSet

  /** The names on the preflight.sh command lines — every all-caps word after the script. */
  private def asserted(preflight: String): Set[String] = {
    val script = preflight.linesIterator.dropWhile(!_.contains("scripts/ci/preflight.sh")).mkString(" ")
    """\b([A-Z][A-Z0-9_]{2,})\b""".r.findAllMatchIn(script).map(_.group(1)).toSet
  }

  Guarded.foreach { case (file, androidLicence) =>
    lazy val all       = jobs(RepoFile.read(file))
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
  }
}
