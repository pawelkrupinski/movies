package deploy

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, NoSuchFileException}
import java.nio.file.StandardOpenOption.{APPEND, CREATE}

import ShellCommands.Use

/** The cases of [[WorkflowScriptsRunnableSpec]] that run a real git: the index through `git ls-files`, and the
 *  `git sparse-checkout set` its cone-mode rule rests on. */
class WorkflowScriptsRunnableIntegrationSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  import WorkflowScriptsRunnableSpec.*

  /** Every repo path's mode in the git index. */
  private lazy val index: Map[String, String] = gitIndexModes()

  /** The git hooks: what scripts/install-hooks.sh links into .git/hooks (their self-tests and data files are not). */
  private val Hooks = Seq("scripts/hooks/pre-push", "scripts/hooks/pre-commit")

  private def ciScripts: Seq[Script] =
    RepoFile.workflows().map(_.getPath).sorted.flatMap { path =>
      val yml = RepoFile.read(path)
      RepoFile.jobs(yml).toSeq.sortBy(_._1).flatMap { (name, job) =>
        val defaultDir = WorkflowText.keys(job).get("defaults")
          .flatMap(_.linesIterator.map(_.trim).collectFirst { case s"working-directory: $dir" => dir.trim })
        steps(job).map(s => Script(s"$path ($name)", s.script, s.workingDirectory.orElse(defaultDir)))
      }
    } ++ RepoFile.compositeActions().flatMap { path =>
      val dir = path.stripSuffix("/action.yml")
      steps(RepoFile.read(path)).map(s => Script(path, s.script, s.workingDirectory, ownDirectory = Some(dir)))
    }

  /** A job's or action's shell: every `run:`, and the Android emulator runner's `script:` (shell it runs the same way),
   *  a one-line value YAML-quoted (`run: '"$GITHUB_WORKSPACE"/x.sh'`) read the way YAML reads it. */
  private def steps(yml: String): Seq[RepoFile.RunStep] =
    (RepoFile.runSteps(yml) ++ RepoFile.runSteps(yml, "script:")).map(s => s.copy(script = yamlScalar(s.script)))

  private def hookScripts: Seq[Script] =
    Hooks.map(path => Script(path, RepoFile.read(path), None, computedCdIsRoot = true, ownDirectory = Some("scripts/hooks")))

  private def localUses: Seq[(String, String)] = for {
    path <- RepoFile.ciFiles()
    line <- RepoFile.withoutComments(RepoFile.read(path)).linesIterator.toSeq
    target <- line.trim.stripPrefix("- ").trim match {
      case s"uses: ./$local" => Some(local.trim)
      case _                 => None
    }
  } yield path -> target

  "every script CI and the hooks run" should "be in the index, executable, with a shebang" in {
    val all = ciScripts ++ hookScripts
    // A sweep that finds nothing to check passes vacuously: the lexer or the step reader broke, not the CI got clean.
    all.flatMap(_.references).count(_._2.use == Use.Run) should be > 50
    withClue("a script CI or a hook runs that cannot run there (fix: `git update-index --chmod=+x <path>` and a `#!` line):\n") {
      all.flatMap(problems(index, _)).distinct shouldBe empty
    }
  }

  "every local action and reusable workflow a job uses" should "be in the index" in {
    localUses should not be empty
    val missing = localUses.collect {
      case (from, target) if !index.contains(target) && !index.contains(s"$target/action.yml") && !index.contains(s"$target/action.yaml") =>
        s"$from: uses ./$target, which is not in the index"
    }
    missing shouldBe empty
  }

  // Why every sparse checkout is cone mode: actions/checkout sets one up after `git init` and its fetch, before checking
  // anything out, in the .git the runner's template made -- ci-runner's had no info/ on 2026-10-23 (Archive recording
  // #823). The default template makes info/exclude, so the scratch repository drops it to stand in for that one.
  "a sparse checkout" should "set up in cone mode in a repository with no .git/info" in {
    val repo = new ScratchGitRepository(tempDirectory)
    val info = repo.root.resolve(".git/info")
    Files.deleteIfExists(info.resolve("exclude"))
    Files.deleteIfExists(info)
    withClue("non-cone, actions/checkout appends its patterns to the file, and so needs its directory: ") {
      a[NoSuchFileException] should be thrownBy
        Files.writeString(info.resolve("sparse-checkout"), "\nscripts/ci\n", CREATE, APPEND)
    }
    repo.git("sparse-checkout", "set", "scripts/ci", ".github/actions/report-timings")
    Files.readString(info.resolve("sparse-checkout")) should include("/scripts/ci/")
  }

  /** `git ls-files -s` of this checkout: path -> mode. Run with no inherited GIT_* variable, so that under a hook (which
   *  sets GIT_DIR / GIT_INDEX_FILE for ITS repository) it still reads the checkout the tests run in. */
  def gitIndexModes(): Map[String, String] = {
    val builder = new ProcessBuilder("git", "ls-files", "-s", "-z").redirectErrorStream(false)
    builder.environment().keySet().removeIf(_.startsWith("GIT_"))
    val process = builder.start()
    val out     = new String(process.getInputStream.readAllBytes(), UTF_8)
    if (process.waitFor() != 0) throw new AssertionError(s"git ls-files -s failed (exit ${process.exitValue()})")
    out.split('\u0000').toSeq.filter(_.nonEmpty).map { entry =>
      val (meta, path) = entry.span(_ != '\t')
      path.drop(1) -> meta.takeWhile(_ != ' ')
    }.toMap
  }
}
