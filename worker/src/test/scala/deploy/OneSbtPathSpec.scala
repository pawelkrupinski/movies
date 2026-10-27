package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/**
 * CI runs sbt ONE way: as `sbt`, resolved through scripts/ci/sbt-guard/sbt, which every sbt setup puts first on PATH.
 *
 * A compile can read a stale TASTy file in zinc's first cycle (`-Wsafe-init`, project/StaleTastyRetry.scala), on a build
 * restored from any older commit. The build's plugin recovers a batch run, but how a job called sbt could disarm it:
 * Identity live drift #2001 (run 3141) began its batch with a `set`, whose reload disarmed the plugin, and failed every
 * country. The guard recovers whatever the plugin did not, for every call of every job, so a call shaped differently
 * (a `set` first, `--client`) cannot step around the recovery again. So: each `sbt/setup-sbt` step is followed by the
 * guard's PATH step, each job that runs sbt sets it up, and nothing launches sbt but by that name.
 */
class OneSbtPathSpec extends AnyFlatSpec with Matchers {
  private val GuardStep = """run: echo "$GITHUB_WORKSPACE/scripts/ci/sbt-guard" >> "$GITHUB_PATH""""
  private val SetupSbt  = "uses: sbt/setup-sbt@"
  /** What sets sbt up in a job: the runner step itself, or a shared action holding it. */
  private val Setups = Seq(SetupSbt, "uses: ./.github/actions/setup-sbt-build", "uses: ./.github/actions/convergence-setup",
    "uses: ./.github/actions/run-page-test")
  /** A shell line calling sbt by name: `sbt` as a command word. */
  private val CallsSbt = """(^|[\s;&|(`])sbt\s+[-'"\w$]""".r
  /** sbt launched any other way: its native client, its launcher jar, or a path to a runner script. */
  private val OtherLaunch = """\bsbtn\b|sbt-launch|java\s.*-jar.*sbt|[/}]sbt["']?\s+\S""".r

  private def code(text: String): Seq[String] =
    RepoFile.withoutComments(text).linesIterator.map(_.trim).filter(_.nonEmpty).toSeq

  private def runsSbt(yml: String): Boolean =
    RepoFile.runSteps(yml).exists(step => step.script.linesIterator.exists(l => CallsSbt.findFirstIn(l).isDefined))

  /** The composite actions that call sbt in a step of their own, and those that set it up (convergence-fill: both). */
  private lazy val sbtActions: Seq[String] =
    RepoFile.compositeActions().filter(path => runsSbt(RepoFile.read(path))).map(path => path.stripSuffix("/action.yml"))
  private lazy val setups: Seq[String] = Setups ++ RepoFile.compositeActions()
    .filter(path => Setups.exists(RepoFile.withoutComments(RepoFile.read(path)).contains))
    .map(path => s"uses: ./${path.stripSuffix("/action.yml")}")

  "every sbt/setup-sbt step" should "be followed by the guard's PATH step, under the same condition" in {
    val found = for {
      path  <- RepoFile.ciFiles()
      lines  = code(RepoFile.read(path)).toVector
      at    <- lines.indices if lines(at).stripPrefix("- ").startsWith(SetupSbt)
    } yield {
      val rest  = lines.drop(at + 1)
      val next  = rest.indexWhere(_.startsWith("- "))
      next should be >= 0
      val setup = rest.take(next)
      val guard = rest.drop(next).take(5)
      val after = guard.drop(1).takeWhile(!_.startsWith("- "))
      withClue(s"$path: the step after setup-sbt: ") {
        (guard.head +: after).map(_.stripPrefix("- ")) should contain(GuardStep)
        after.filter(_.startsWith("if:")) shouldBe setup.filter(_.startsWith("if:"))
      }
      path
    }
    found.distinct.size should be >= 9
  }

  "every job that runs sbt" should "set sbt up, and so the guard" in {
    sbtActions should not be empty
    val sbtJobs = for {
      file      <- RepoFile.workflows().map(_.getPath)
      (job, yml) <- RepoFile.jobs(RepoFile.read(file))
      body       = RepoFile.withoutComments(yml)
      uses       = body.linesIterator.map(_.trim.stripPrefix("- ")).toSet
      if runsSbt(yml) || sbtActions.exists(action => uses(s"uses: ./$action"))
    } yield (s"$file: $job", body)
    sbtJobs.size should be >= 15
    sbtJobs.collect { case (job, body) if !setups.exists(body.contains) => job } shouldBe empty
  }

  "nothing in .github/ or scripts/ci/" should "launch sbt but by its name" in {
    def under(root: String): Seq[String] = {
      val stream = Files.walk(Paths.get(root))
      try stream.iterator.asScala.filter(Files.isRegularFile(_)).map(_.toString).toVector finally stream.close()
    }
    val scripts = (under(".github") ++ under("scripts/ci"))
      .filterNot(p => p.startsWith("scripts/ci/sbt-guard") || p.endsWith("-test.sh"))
      .filter(p => p.endsWith(".yml") || p.endsWith(".sh") || !p.contains('.'))
    scripts.size should be > 50
    val launches = for {
      path <- scripts
      line <- code(RepoFile.read(path))
      if OtherLaunch.findFirstIn(line).isDefined && !line.contains("sbt/setup-sbt@")
    } yield s"$path: $line"
    launches shouldBe empty
  }

  "the patterns" should "tell a call of sbt from a mention" in {
    Seq("sbt -batch compile", "( cd x && sbt 'set a := 1' run )", "if ! sbt --client \"$c\"; then")
      .forall(CallsSbt.findFirstIn(_).isDefined) shouldBe true
    Seq("cache: sbt", "path: ~/.cache/sbt", "sbt-target-linux-", "build.sbt", "scripts/sbt-warm x")
      .exists(CallsSbt.findFirstIn(_).isDefined) shouldBe false
    Seq("sbtn compile", "java -jar sbt-launch.jar", "\"$HOME/sbt/bin/sbt\" compile", "${SBT}/sbt x")
      .forall(OtherLaunch.findFirstIn(_).isDefined) shouldBe true
    Seq("sbt -batch compile", "uses: sbt/setup-sbt@v1", "build.sbt", "path: ~/.cache/sbt", "sbt-target-")
      .exists(OtherLaunch.findFirstIn(_).isDefined) shouldBe false
  }
}
