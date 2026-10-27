package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import ShellCommands.{Reference, Use}

/**
 * EVERY SCRIPT CI CALLS EXISTS AND CAN RUN. scripts/ci/bounded-step.sh was committed without its executable bit
 * (268f10ac3): sourcing it needs none, but every `scripts/ci/bounded-step.sh --reap` step failed with "Permission
 * denied" and turned every convergence leg of main red for an hour (run 1994, fixed by 1c1cf9789).
 *
 * Over every command in `.github/workflows/`, `.github/actions/` and the git hooks in `scripts/hooks/` (what they run,
 * read by ShellCommands, each from its step's working directory):
 *
 *  - a repo file a command RUNS (`scripts/x.sh`, `./gradlew`) is in the git index as mode 100755, and its first line
 *    is a shebang naming an absolute interpreter (`#!/usr/bin/env bash`, `#!/bin/sh`) -- the bit on disk is not
 *    enough: CI checks out the index, not this laptop's tree;
 *  - a repo file a command hands to an interpreter (`bash x.sh`, `python3 x.py`, `source x.sh`) is in the index;
 *  - a local action or reusable workflow a job `uses: ./...` is in the index.
 */
class WorkflowScriptsRunnableSpec extends AnyFlatSpec with Matchers {

  import WorkflowScriptsRunnableSpec.*

  /** Every repo path's mode in the git index. */
  private lazy val index: Map[String, String] = gitIndexModes()

  /** The git hooks: what scripts/install-hooks.sh links into .git/hooks (their self-tests and data files are not). */
  private val Hooks = Seq("scripts/hooks/pre-push", "scripts/hooks/pre-commit")

  private def ciScripts: Seq[Script] =
    RepoFile.workflows().map(_.getPath).sorted.flatMap { path =>
      val yml = RepoFile.read(path)
      RepoFile.jobs(yml).toSeq.sortBy(_._1).flatMap { (name, job) =>
        val defaultDir = ForgejoParse.keys(job).get("defaults")
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

  "the check" should "go red on a script without its executable bit, without a shebang, or missing" in {
    val script = Script("ci.yml (job)", "scripts/ci/run.sh --reap\nbash scripts/ci/read.sh\nscripts/ci/plain.sh\nscripts/gone.sh", None)
    val modes  = Map("scripts/ci/run.sh" -> "100644", "scripts/ci/read.sh" -> "100644", "scripts/ci/plain.sh" -> "100755")
    val firstLine = Map("scripts/ci/run.sh" -> "#!/usr/bin/env bash", "scripts/ci/plain.sh" -> "set -eu")
    problems(modes, script, firstLine.get) shouldBe Seq(
      "ci.yml (job): line 1 runs scripts/ci/run.sh, mode 100644 in the index (not 100755)",
      "ci.yml (job): line 3 runs scripts/ci/plain.sh, whose first line is no shebang: set -eu",
      "ci.yml (job): line 4 runs scripts/gone.sh, which is not in the index",
    )
    problems(modes.updated("scripts/ci/run.sh", "100755").updated("scripts/gone.sh", "100755"), script, Map(
      "scripts/ci/run.sh" -> "#!/usr/bin/env bash", "scripts/ci/plain.sh" -> "#!/bin/sh", "scripts/gone.sh" -> "#!/bin/bash"
    ).get) shouldBe empty
  }

  "ShellCommands" should "find what a script runs and reads, from where" in {
    val script =
      """set -euo pipefail
        |X=$(scripts/ci/plan.sh "$1") # a comment naming scripts/not-run.sh
        |if [ -x scripts/ci/maybe.sh ]; then timeout 30 scripts/ci/bounded.sh --reap; fi
        |cd android && ./gradlew test 2>&1 | tee build/log.txt
        |(cd ../ios && scripts/sim.sh)
        |python3 -u scripts/run.py > out/report.txt
        |cat <<EOF
        |scripts/heredoc-is-data.sh
        |EOF
        |case "$mode" in
        |  scripts/pattern.sh|other) source ../scripts/lib.sh ;;
        |esac
        |for f in scripts/a.sh scripts/b.sh; do bash "$f"; done
        |cd "$somewhere"
        |scripts/unknown-dir.sh
        |""".stripMargin
    ShellCommands.references(script) shouldBe Seq(
      Reference("scripts/ci/plan.sh", Use.Run, 2),
      Reference("scripts/ci/bounded.sh", Use.Run, 3),
      Reference("android/gradlew", Use.Run, 4),
      Reference("ios/scripts/sim.sh", Use.Run, 5),
      Reference("android/scripts/run.py", Use.Read, 6),
      Reference("scripts/lib.sh", Use.Read, 11),
    )
  }

  it should "read the runner's workspace and an action's own directory as fixed places in the checkout" in {
    val script = Seq(
      """cd "$GITHUB_WORKSPACE" && scripts/a.sh""",
      """"${{ github.workspace }}/scripts/b.sh" --flag""",
      """"$GITHUB_WORKSPACE"/scripts/c.sh""",
      """"$GITHUB_ACTION_PATH"/drop.sh""",
      """cd android && ${{ github.workspace }}/scripts/d.sh""",
      """. "$(dirname "${BASH_SOURCE[0]}")/../lib.sh"""",
    ).mkString("\n")
    ShellCommands.references(script, ownDirectory = Some(".github/actions/x")).map(_.path) shouldBe
      Seq("scripts/a.sh", "scripts/b.sh", "scripts/c.sh", ".github/actions/x/drop.sh", "scripts/d.sh", ".github/actions/lib.sh")
    yamlScalar("""'"$GITHUB_WORKSPACE"/x.sh ''a'''""") shouldBe """"$GITHUB_WORKSPACE"/x.sh 'a'"""
  }

  it should "treat a computed cd as the checkout's root in a hook" in {
    ShellCommands.references("cd \"$root\" || exit 1\nscripts/x.sh", computedCdIsRoot = true) shouldBe
      Seq(Reference("scripts/x.sh", Use.Run, 2))
  }
}

object WorkflowScriptsRunnableSpec {

  /** Shell to check: where it comes from, its text, the directory it runs in, and its own directory (a composite
   *  action's, a script file's). */
  final case class Script(
      from: String,
      text: String,
      workingDirectory: Option[String],
      computedCdIsRoot: Boolean = false,
      ownDirectory: Option[String] = None,
  ) {
    /** The step's directory in the checkout: `${{ github.workspace }}` is its root; any other expression, unknown. */
    private def directory: Option[String] =
      Some(workingDirectory.getOrElse("").replaceAll("""^\$\{\{\s*github\.workspace\s*}}/?""", ""))
        .filterNot(_.contains("$")).flatMap(ShellCommands.normalise)

    def references: Seq[(Script, Reference)] =
      directory.toSeq.flatMap { dir =>
        ShellCommands.references(text, dir, computedCdIsRoot, ownDirectory).map(this -> _)
      }
  }

  /** A one-line YAML value as YAML reads it: single-quoted with `''` for a quote, double-quoted with backslash escapes;
   *  anything else as it is. */
  def yamlScalar(value: String): String =
    if (value.length >= 2 && value.startsWith("'") && value.endsWith("'")) value.drop(1).dropRight(1).replace("''", "'")
    else if (value.length >= 2 && value.startsWith("\"") && value.endsWith("\""))
      value.drop(1).dropRight(1).replace("\\\"", "\"").replace("\\\\", "\\")
    else value

  private val Shebang = """#!\s*/\S+(\s+\S+)*""".r

  private def firstLineOnDisk(path: String): Option[String] =
    scala.util.Try(Files.newBufferedReader(Paths.get(path), UTF_8)).toOption.map { reader =>
      try Option(reader.readLine()).getOrElse("")
      finally reader.close()
    }

  /** What is wrong with each repo file `script` uses, given the index's modes and each file's first line. */
  def problems(
      modes: Map[String, String],
      script: Script,
      firstLine: String => Option[String] = firstLineOnDisk,
  ): Seq[String] = script.references.flatMap { (_, ref) =>
    val verb = if (ref.use == Use.Run) "runs" else "reads"
    val at   = s"${script.from}: line ${ref.line} $verb ${ref.path}"
    modes.get(ref.path) match {
      case None                                   => Some(s"$at, which is not in the index")
      case Some(_) if ref.use == Use.Read         => None
      case Some(mode) if mode != "100755"         => Some(s"$at, mode $mode in the index (not 100755)")
      case Some(_) =>
        firstLine(ref.path) match {
          case Some(Shebang(_*)) => None
          case Some(other) if other.startsWith("#!") && other.endsWith("\r") => Some(s"$at, whose shebang ends in a CR")
          case other                             => Some(s"$at, whose first line is no shebang: ${other.getOrElse("")}")
        }
    }
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
