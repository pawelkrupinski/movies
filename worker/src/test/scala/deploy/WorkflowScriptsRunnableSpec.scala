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
 *  - a local action or reusable workflow a job `uses: ./...` is in the index;
 *  - a workflow step that `uses: ./...` comes after a checkout of this repository into the workspace whose
 *    sparse-checkout, if it has one, holds that action: the runner reads the action from the workspace, and 35cdcaac6
 *    put `./.github/actions/upstream-run` after a sparse checkout of scripts/ci and one script only ("no files
 *    found after reading paths: action.yml", Identity findings report #2049);
 *  - a sparse checkout is git's cone mode, listing directories of this repository: actions/checkout's non-cone mode
 *    appends its patterns to `.git/info/sparse-checkout` and makes no `.git/info/` for it, which only the runner's
 *    `git init` template provides. One runner's had none on the morning of 2026-10-23, and a
 *    non-cone checkout died "ENOENT: no such file or directory, open '.../.git/info/sparse-checkout'" (Archive
 *    recording #823's `report (timings)`). Cone mode runs `git sparse-checkout set`, which makes the directory itself. It takes
 *    directories only -- a pattern (a `*-paths.txt` glob) fails the checkout, a file is read as a directory -- and
 *    brings every file at the root and directly inside each listed directory's parents.
 *
 * The first two, which read the index through a real `git ls-files`, are in [[WorkflowScriptsRunnableIntegrationSpec]],
 * beside the real `git sparse-checkout set` the last one rests on.
 */
class WorkflowScriptsRunnableSpec extends AnyFlatSpec with Matchers {

  import WorkflowScriptsRunnableSpec.*

  /** Every job of every workflow, by where it is: `<workflow path> (<job>)`. */
  private lazy val workflowJobs: Seq[(String, String)] =
    RepoFile.workflows().map(_.getPath).sorted.flatMap { path =>
      RepoFile.jobs(RepoFile.read(path)).toSeq.sortBy(_._1).map((name, job) => s"$path ($name)" -> job)
    }

  "every workflow step that uses a local action" should "come after a checkout holding it" in {
    workflowJobs.flatMap((_, job) => jobSteps(job)).count(_.contains("uses: ./")) should be > 20
    workflowJobs.flatMap((from, job) => localActionsOutsideCheckout(from, job)) shouldBe empty
  }

  "every sparse checkout" should "be git's cone mode, of directories of this repository" in {
    val checked = workflowJobs ++ RepoFile.compositeActions().map(path => path -> RepoFile.read(path))
    checked.flatMap((_, job) => jobSteps(job)).count(sparseEntries(_).nonEmpty) should be > 10
    checked.flatMap((from, job) => sparseCheckoutProblems(from, job, new java.io.File(_).isDirectory)) shouldBe empty
  }

  "the sparse checkout check" should "go red on a non-cone checkout, and on an entry no directory of this repository" in {
    val job =
      """    runs-on: ubuntu-26.04
        |    steps:
        |        - uses: actions/checkout@v5
        |          with:
        |              sparse-checkout: |
        |                  scripts/ci
        |                  scripts/tool-conventions.sh
        |                  .github/*-paths.txt
        |                  /scripts
        |              sparse-checkout-cone-mode: false
        |        - uses: actions/checkout@v5
        |          with:
        |              sparse-checkout: scripts/ci # the job's scripts
        |              sparse-checkout-cone-mode: 'TRUE'
        |        - uses: actions/checkout@v5
        |          with:
        |              repository: other/repo
        |              path: other
        |              sparse-checkout: docs/guide.md
        |        - uses: actions/checkout@v5
        |          with:
        |              repository: other/repo
        |              sparse-checkout: docs
        |              sparse-checkout-cone-mode: ${{ inputs.cone }}
        |        - uses: actions/checkout@v5
        |""".stripMargin
    sparseCheckoutProblems("ci.yml (job)", job, Set("scripts", "scripts/ci")) shouldBe Seq(
      "ci.yml (job): a non-cone sparse checkout (scripts/ci, scripts/tool-conventions.sh, .github/*-paths.txt, /scripts), which " +
        "needs .git/info from the runner's git template: leave sparse-checkout-cone-mode out",
      "ci.yml (job): its sparse checkout lists scripts/tool-conventions.sh, no directory of this repository (cone mode takes directories)",
      "ci.yml (job): its sparse checkout lists .github/*-paths.txt, no directory of this repository (cone mode takes directories)",
      "ci.yml (job): its sparse checkout lists /scripts, no directory of this repository (cone mode takes directories)",
      "ci.yml (job): a non-cone sparse checkout (docs), which needs .git/info from the runner's git template: leave " +
        "sparse-checkout-cone-mode out",
    )
  }

  "what a checkout holds" should "be, sparse, git's cone: all under a listed directory, the files beside its parents" in {
    def job(checkout: String) = s"    runs-on: ubuntu-26.04\n    steps:\n$checkout        - run: scripts/ci/plan.sh\n"
    val sparse = job(
      """        - uses: actions/checkout@v5
        |          with:
        |              sparse-checkout: |
        |                  scripts/ci
        |                  .github/actions/upstream-run
        |""".stripMargin)
    // What `git sparse-checkout set scripts/ci .github/actions/upstream-run` checks out, and what it leaves out.
    Seq("scripts/ci/ci-row-scope.sh", "scripts/tool-conventions.sh", ".github/convergence-paths.txt", "build.sbt",
      ".github/actions/upstream-run/action.yml").filterNot(checkoutHolds(sparse, _)) shouldBe empty
    Seq("scripts/hooks/pre-push", ".github/workflows/ci.yml", ".github/actions/report-timings/action.yml")
      .filter(checkoutHolds(sparse, _)) shouldBe empty
    checkoutHolds(job("        - uses: actions/checkout@v5\n"), "scripts/hooks/pre-push") shouldBe true
    checkoutHolds(job(""), "build.sbt") shouldBe false
    checkoutHolds(job("        - uses: actions/checkout@v5\n          with:\n              repository: other/repo\n"),
      "build.sbt") shouldBe false
  }

  "the checkout check" should "go red on a local action with no checkout before it, or outside a sparse one" in {
    val job =
      """    runs-on: ubuntu-26.04
        |    steps:
        |        - uses: ./.github/actions/setup-jdk
        |        - uses: actions/checkout@v5
        |          with:
        |              sparse-checkout: |
        |                  scripts/ci
        |                  .github/actions/upstream-run/
        |              sparse-checkout-cone-mode: false
        |        - id: upstream
        |          uses: ./.github/actions/upstream-run
        |        - uses: ./.github/actions/setup-sbt-build # not in the sparse checkout
        |        - uses: actions/checkout@v5
        |          with:
        |              repository: other/repo
        |        - uses: ./.github/actions/report-timings
        |        - uses: actions/checkout@v5
        |        - uses: ./.github/actions/report-timings
        |""".stripMargin
    localActionsOutsideCheckout("ci.yml (job)", job) shouldBe Seq(
      "ci.yml (job): uses ./.github/actions/setup-jdk with no checkout of this repository before it",
      "ci.yml (job): uses ./.github/actions/setup-sbt-build, outside its sparse checkout (scripts/ci, .github/actions/upstream-run)",
      "ci.yml (job): uses ./.github/actions/report-timings, outside its sparse checkout (scripts/ci, .github/actions/upstream-run)",
    )
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

  /** Each step of a workflow job: its `- ` item's lines, whole-line comments dropped. */
  def jobSteps(job: String): Seq[String] = {
    def indentOf(line: String) = line.takeWhile(_ == ' ').length
    val lines   = RepoFile.withoutComments(job).linesIterator.toVector
    val stepsAt = lines.indexWhere(_.trim == "steps:")
    if (stepsAt < 0) Seq.empty
    else {
      val body = lines.drop(stepsAt + 1).takeWhile(l => l.trim.isEmpty || indentOf(l) > indentOf(lines(stepsAt)))
      body.find(_.trim.startsWith("- ")).map(indentOf).toSeq.flatMap { dash =>
        body
          .foldLeft(Vector.empty[Vector[String]]) { (steps, line) =>
            if (line.trim.startsWith("- ") && indentOf(line) == dash) steps :+ Vector(line)
            else steps.lastOption.fold(steps)(last => steps.init :+ (last :+ line))
          }
          .map(_.mkString("\n"))
      }
    }
  }

  /** A step's one-line `key:` value, its trailing comment dropped. */
  private def value(step: String, key: String): Option[String] =
    step.linesIterator.map(_.trim.stripPrefix("- ").trim).collectFirst {
      case line if line.startsWith(s"$key:") => line.stripPrefix(s"$key:").replaceAll("""\s+#.*$""", "").trim
    }

  /** A checkout step's `sparse-checkout:` entries as actions/checkout reads them (one a line, trimmed, blank lines
   *  dropped); None when it has none, a checkout of every file. */
  def sparseEntries(step: String): Option[Seq[String]] = {
    val lines = step.linesIterator.toVector
    lines.indexWhere(_.trim.startsWith("sparse-checkout:")) match {
      case -1 => None
      case at =>
        val key    = lines(at).takeWhile(_ == ' ').length
        val inline = lines(at).trim.stripPrefix("sparse-checkout:").replaceAll("""\s+#.*$""", "").trim
        val listed =
          if (inline.startsWith("|") || inline.startsWith(">"))
            lines.drop(at + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > key).map(_.trim)
          else Vector(yamlScalar(inline))
        Some(listed.filter(_.nonEmpty))
    }
  }

  /** What is wrong with each sparse checkout of `job` (a workflow job, or a composite action): non-cone mode, which
   *  actions/checkout reads from any `sparse-checkout-cone-mode` but `true` and whose patterns need the `.git/info/` only
   *  the runner's `git init` template makes (Archive recording #823); and, in a checkout of this repository, an entry
   *  that is no directory of it (`isDirectory`, from its root) -- a pattern, a file, an absolute or a gone path. */
  def sparseCheckoutProblems(from: String, job: String, isDirectory: String => Boolean): Seq[String] =
    jobSteps(job).flatMap { step =>
      (value(step, "uses"), sparseEntries(step)) match {
        case (Some(s"actions/checkout@$_"), Some(entries)) =>
          val nonCone = value(step, "sparse-checkout-cone-mode").map(yamlScalar).exists(!_.equalsIgnoreCase("true"))
          val notDirectories =
            if (value(step, "repository").nonEmpty) Seq.empty
            else entries.filterNot(entry => !entry.startsWith("/") && isDirectory(entry))
          Option.when(nonCone)(s"$from: a non-cone sparse checkout (${entries.mkString(", ")}), which needs .git/info " +
            "from the runner's git template: leave sparse-checkout-cone-mode out").toSeq ++
            notDirectories.map(entry =>
              s"$from: its sparse checkout lists $entry, no directory of this repository (cone mode takes directories)")
        case _ => Seq.empty
      }
    }

  /** Whether `step` checks this repository out into the workspace: actions/checkout with no `repository:` and no
   *  `path:`. */
  private def checksOutThisRepository(step: String): Boolean =
    value(step, "uses").exists(_.startsWith("actions/checkout@")) && value(step, "repository").isEmpty &&
      value(step, "path").isEmpty

  /** Whether `job`'s checkout of this repository (its last) holds the file `path`: a whole checkout holds every file; a
   *  sparse one, cone mode, everything under a listed directory and every file at the root or directly inside a listed
   *  directory's parents. */
  def checkoutHolds(job: String, path: String): Boolean =
    jobSteps(job).filter(checksOutThisRepository).lastOption.exists { checkout =>
      sparseEntries(checkout).forall { entries =>
        val directories = entries.map(_.stripSuffix("/"))
        val parent      = path.split('/').init.mkString("/")
        parent.isEmpty || directories.exists(dir => path.startsWith(s"$dir/") || dir.startsWith(s"$parent/"))
      }
    }

  /** What is wrong with each `uses: ./...` step of `job`: the runner reads a local action from the workspace, so a
   *  checkout of this repository into it (no `repository:`, no `path:`) comes first, and when that checkout is sparse
   *  it holds the action's directory. A later checkout replaces an earlier one. */
  def localActionsOutsideCheckout(from: String, job: String): Seq[String] =
    // None: no checkout yet; Some(None): the whole repository; Some(Some(paths)): a sparse one.
    jobSteps(job)
      .foldLeft((Option.empty[Option[Seq[String]]], Vector.empty[String])) { case ((checkout, problems), step) =>
        value(step, "uses") match {
          case _ if checksOutThisRepository(step) =>
            (Some(sparseEntries(step).map(_.map(_.stripPrefix("/").stripSuffix("/")))), problems)
          case Some(s"./$local") =>
            val target = local.stripSuffix("/")
            checkout match {
              case None => (checkout, problems :+ s"$from: uses ./$target with no checkout of this repository before it")
              case Some(Some(paths)) if !paths.exists(p => target == p || target.startsWith(s"$p/")) =>
                (checkout, problems :+ s"$from: uses ./$target, outside its sparse checkout (${paths.mkString(", ")})")
              case _ => (checkout, problems)
            }
          case _ => (checkout, problems)
        }
      }
      ._2
}
