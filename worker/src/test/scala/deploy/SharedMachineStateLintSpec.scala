package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.matching.Regex

/**
 * Tests touch only state of their own. Many agents and sessions run this repository's tests on ONE
 * machine at once, sharing one `.git` (config, refs, stash — every worktree commits through it),
 * one `/tmp`, one home directory.
 *
 * Four rules, over every test file in every language the repository tests in — shell (`*-test.sh`,
 * `*-spec.sh`), Python (`test_*.py`, `*_test.py`), Scala (`src/test`, `src/it`, `src/page`,
 * `src/fixtures`, testkit), TypeScript/JavaScript (`*.test.*`, `*.spec.*`, the dashboard's `test/`,
 * the Playwright suite), Swift and Kotlin tests — each offender named `file:line`:
 *
 *  1. NO DIRECT `git`. A test that builds a scratch repository goes through the one helper of its
 *     language, which drops the inherited GIT_* environment, stops ~/.gitconfig being read, supplies
 *     the identity, and (in shell) refuses this checkout: `scratch_repo` / `scratch_git` from
 *     scripts/scratch-git.sh, `ScratchGitRepository` in Scala, `TempRepo` (with setup.ts's
 *     `isolateGitEnvironment`) in the dashboard. On 2026-10-04 pre-push-test.sh ran under the
 *     pre-push hook, inherited its GIT_DIR, and its "scratch" `git init --bare` / `git config` /
 *     `git commit` wrote core.bare=true, user.name=Spec and five commits into the shared repository.
 *
 *  2. NO WRITE TO A FIXED TEMP PATH. `/tmp/<name>` (or `/var/tmp`) is shared by every agent: two runs
 *     write the same file, and one reads the other's (a co-agent's script replaced one between write and run on
 *     2026-09-05). Take a fresh directory per run — `mktemp -d`, `Files.createTempDirectory`,
 *     `mkdtempSync(join(tmpdir(), …))`.
 *
 *  3. NO USER DIRECTORY. A test never reads or writes the user's caches, documents or home (the iOS
 *     tests once kept their conditional-GET entries in the real ~/Library/Caches under the production
 *     file names, shared with every concurrent run and the developer's own app). It hands the code
 *     under test a directory of its own (`ConditionalPayloadCache.scratchDirectory()` on iOS).
 *
 *  4. NO RELEASED "FREE" PORT. Binding port 0 to learn a free port, closing it and binding that number
 *     later leaves a window in which any other process on the machine can take it. Bind port 0 and
 *     KEEP the socket — `TcpForwarder.start` holds its own; a spec that needs the port dead first
 *     severs the forwarder rather than releasing the port.
 *
 * Each allowlist names its file and why; an entry whose file no longer offends fails the build.
 */
class SharedMachineStateLintSpec extends AnyFlatSpec with Matchers {

  /** file → why it may run `git` itself. */
  private val DirectGitAllowed: Map[String, String] = Map(
    "worker/src/test/scala/deploy/ScratchGitRepository.scala" ->
      "the Scala helper: every command it starts runs with ScratchGitRepository.isolate's environment",
    "infra/version-dashboard/test/mobile/repo.ts" ->
      "the dashboard's TempRepo helper, under the environment setup.ts isolates for every test",
    "worker/src/test/scala/deploy/ScratchGitRepositorySpec.scala" ->
      "proves ScratchGitRepository.isolate against a decoy GIT_DIR — it must start git with an environment of its own making",
    "infra/version-dashboard/test/git-isolation.test.ts" ->
      "proves isolateGitEnvironment against a decoy GIT_DIR — it must run git with an environment of its own making",
    "worker/src/test/scala/services/enrichment/scraping/JsonLdScanSpec.scala" ->
      "a read-only `git ls-files` of this checkout's own fixtures: writes nothing, and reads the checkout it runs in",
  )

  /** file → why a fixed temp path written in it is not a shared file. */
  private val FixedTempAllowed: Map[String, String] = Map(
    "worker/src/test/scala/tools/AotClassListSpec.scala" ->
      "the `> /tmp/aot-classes.txt && mv` is the regeneration command a failure message prints for a person to run, not one the spec runs",
  )

  private val Roots = Seq("scripts", ".github", "data", "android", "ios", "infra", "tools", "page-tests-playwright",
    "common/src", "web/src", "worker/src", "testkit/src", "e2e/src")

  /** Never descended into: dependencies, build output, test resources (recorded response bodies by
   *  the ten thousand, no test code), and the GitOps checkout `fetch-gitops` links in. */
  private val SkippedDirectories = Set("node_modules", "target", "build", ".gradle", ".build", "Pods", "DerivedData",
    ".venv", "__pycache__", "resources", "kubernetes")

  private sealed trait Language
  private case object Shell extends Language
  private case object Python extends Language
  private case object Jvm extends Language
  private case object Script extends Language

  private def languageOf(path: String): Option[Language] = {
    val name = path.substring(path.lastIndexOf('/') + 1)
    val jvmTest = Seq("/src/test/", "/src/it/", "/src/page/", "/src/fixtures/", "/src/androidTest/", "testkit/src/", "ios/Tests/")
      .exists(path.contains)
    if (name.matches("""[\w.-]+[-_](test|spec)\.sh""")) Some(Shell)
    else if (name.matches("""test_[\w-]+\.py|[\w-]+_test\.py""")) Some(Python)
    else if (jvmTest && name.matches(""".+\.(scala|java|kt|swift)""")) Some(Jvm)
    else if (name.matches(""".+\.(test|spec)\.(ts|js|mjs)""") ||
      (path.startsWith("infra/version-dashboard/test/") || path.startsWith("page-tests-playwright/")) &&
        name.matches(""".+\.(ts|js|mjs)""")) Some(Script)
    else None
  }

  private lazy val testFiles: Seq[(String, Language)] = Roots.map(Paths.get(_)).filter(Files.isDirectory(_)).flatMap { root =>
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator.asScala.filter(Files.isRegularFile(_))
        .filterNot(path => path.iterator.asScala.exists(part => SkippedDirectories.contains(part.toString)))
        .map(_.toString).toList
    }
  }.sorted.filterNot(_ == ThisSpec).flatMap(path => languageOf(path).map(path -> _))

  /** This spec quotes the very shapes it forbids, in its examples. */
  private val ThisSpec = "worker/src/test/scala/deploy/SharedMachineStateLintSpec.scala"

  /** A line with its comment dropped: `#` for shell and Python, `//` and doc lines otherwise. */
  private def code(line: String, language: Language): String = language match {
    case Shell | Python =>
      val trimmed = line.trim
      if (trimmed.startsWith("#")) "" else line.replaceAll("""\s#\s.*$""", "")
    case Jvm | Script =>
      val trimmed = line.trim
      if (trimmed.startsWith("*") || trimmed.startsWith("/*") || trimmed.startsWith("//")) ""
      else line.replaceAll("""\s//\s.*$""", "")
  }

  /** `git` run as a command: at a line's start, after a separator, `$(`, a backtick or a
   *  launching keyword, with any `VAR=value` assignments before it. */
  private val ShellGit =
    """(?:^|[;&|({`!]|\$\(|\b(?:then|do|else|exec|command|env|xargs|time|if|while|until)\s)\s*(?:[A-Za-z_]\w*=\S*\s+)*git(?:\s|$)""".r
  private val DirectGit: Map[Language, Regex] = Map(
    Shell  -> ShellGit,
    Python -> """["']git["']\s*[,\]]|\b(?:system|run|check_output|check_call|Popen|call|getoutput)\(\s*f?["']git\s""".r,
    Jvm    -> """"git"\s*[,)\]]|Process\(\s*"git\s""".r,
    Script -> """\b(?:execFileSync|execFile|spawnSync|spawn|execSync|exec|execa)\s*\(\s*["'`]git\b""".r)

  private val UserDirectory =
    """\.cachesDirectory\b|\.documentDirectory\b|\.applicationSupportDirectory\b|\bdefaultDirectory\b|"user\.home"|NSHomeDirectory\(|homeDirectoryForCurrentUser|getCacheDir\(|getFilesDir\(""".r

  /** file → why it may name a user directory. */
  private val UserDirectoryAllowed: Map[String, String] = Map.empty

  private val ReleasedPort =
    """(?i)\bfree_?port\s*\(\s*\)|ServerSocket\(\s*0\s*\)[^\n]*\bclose\(|bind\(\([^)]*,\s*0\s*\)\)[^\n]*getsockname""".r

  /** file → why it may release a port it found free. */
  private val ReleasedPortAllowed: Map[String, String] = Map(
    "scripts/ci/close-mongo-tunnel-test.sh" ->
      "close-mongo-tunnel.sh kills listeners BY PORT and then proves the port dead: the spec's stages need the port free between them, which holding it would defeat",
  )

  // A `mktemp` template (`/tmp/kinowo.XXXXXX`) names a fresh path per run, not a fixed one.
  private val FixedTemp = """(?<![\w.$}-])/(?:var/)?tmp/(?![\w.-]*XXX)[\w.-]""".r
  /** What writes a file in any of the languages: a redirect, a file-making command, a writing API.
   *  A fixed path that is only a STRING — an argument a parser is fed, a stub's recorded argument, an
   *  assertion on a workflow's text — shares nothing, so a line must write as well as name one. */
  private val Writes =
    """(?<![=-])>|\b(?:tee|mkdir|touch|cp|mv|ln|rsync)\s|Files\.(?:write|writeString|createDirector|createFile|newOutputStream|newBufferedWriter|copy|move)|FileOutputStream|FileWriter|PrintWriter|writeFileSync|appendFileSync|mkdirSync|createWriteStream|write_text|write_bytes|\.mkdir\(|\bopen\([^)]*["'][wax]|createDirectory|createFile|\.write\(to""".r

  /** A fixed temp path on a line that also writes. */
  private val FixedTempWrite = s"(?=.*(?:${Writes.regex})).*${FixedTemp.regex}".r

  private def offending(rule: Language => Regex)(file: String, language: Language): Seq[String] =
    Files.readString(Paths.get(file)).linesIterator.zipWithIndex.collect {
      case (line, index) if rule(language).findFirstIn(code(line, language)).isDefined => s"$file:${index + 1}: ${line.trim}"
    }.toSeq

  private val directGit: (String, Language) => Seq[String] = offending(DirectGit)
  private val fixedTemp: (String, Language) => Seq[String] = offending(_ => FixedTempWrite)
  private val userDirectory: (String, Language) => Seq[String] = offending(_ => UserDirectory)
  private val releasedPort: (String, Language) => Seq[String] = offending(_ => ReleasedPort)

  private def offenders(rule: (String, Language) => Seq[String], allowed: Map[String, String]): Seq[String] =
    testFiles.filterNot { case (file, _) => allowed.contains(file) }.flatMap(rule.tupled)

  "the test-file sweep" should "reach every language it claims to" in {
    testFiles.map(_._2).distinct.toSet shouldBe Set(Shell, Python, Jvm, Script)
  }

  "tests" should "build scratch git repositories only through their language's isolating helper" in {
    val found = offenders(directGit, DirectGitAllowed)
    withClue(s"${found.size} direct `git` calls in tests. Use scratch_repo / scratch_git (scripts/scratch-git.sh), " +
      "ScratchGitRepository (Scala) or TempRepo (dashboard) — a raw git inherits a hook's GIT_DIR and writes into the " +
      "repository every worktree shares — or allowlist the file with a reason:\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  they should "not write to a fixed path under /tmp" in {
    val found = offenders(fixedTemp, FixedTempAllowed)
    withClue(s"${found.size} fixed temp paths in tests, shared by every agent on this machine. Make a directory per " +
      "run (mktemp -d, Files.createTempDirectory, mkdtempSync), or allowlist the file with a reason:\n" +
      found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  they should "not reach the user's caches, documents or home directory" in {
    val found = offenders(userDirectory, UserDirectoryAllowed)
    withClue(s"${found.size} user-directory reads/writes in tests, shared with every concurrent run and the developer's " +
      "own app. Hand the code under test a directory of the test's own, or allowlist the file with a reason:\n" +
      found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  they should "keep a port they bound, never release it to bind again later" in {
    val found = offenders(releasedPort, ReleasedPortAllowed)
    withClue(s"${found.size} free-port probes in tests: between the release and the later bind any process on this " +
      "machine can take the port. Bind port 0 and keep the socket, or allowlist the file with a reason:\n" +
      found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  "the allowlists" should "name only files that are tests and still offend" in {
    def stale(allowed: Map[String, String], rule: (String, Language) => Seq[String]) =
      allowed.keys.toSeq.sorted.filterNot(file => testFiles.find(_._1 == file).exists(rule.tupled(_).nonEmpty))
    withClue("allowlisted but no longer offending (or no longer a test file) — drop the entry: ") {
      (stale(DirectGitAllowed, directGit) ++ stale(FixedTempAllowed, fixedTemp) ++
        stale(UserDirectoryAllowed, userDirectory) ++ stale(ReleasedPortAllowed, releasedPort)) shouldBe empty
    }
  }

  "the temp-path rule" should "catch a write to a fixed temp path, and not a path that is only a string" in {
    val caught = Seq("echo x > /tmp/foo", "mkdir -p /tmp/x", "Files.writeString(Paths.get(\"/tmp/a\"), s)",
      "writeFileSync(\"/tmp/a.json\", s)", "open('/tmp/out.txt', 'w')", "cp a /var/tmp/b")
    val passed = Seq("POOL_APK=\"/tmp/app-debug.apk\"", "arguments(Seq(\"--export\", \"/tmp/lk/\"))",
      "echo x > \"$work/tmp/y\"", "Map(\"dir\" -> \"/tmp/x\")", "xs.map(x => \"/tmp/x\")", "echo y > \"$(mktemp)\"",
      "d=$(mktemp -d /tmp/kinowo-spec.XXXXXX) && echo y > \"$d/f\"")
    caught.filter(FixedTempWrite.findFirstIn(_).isEmpty) shouldBe empty
    passed.filter(FixedTempWrite.findFirstIn(_).isDefined) shouldBe empty
  }

  "the shell rule" should "catch git however a script runs it, and not its mentions" in {
    val caught = Seq("git init -q", "  git -C \"$r\" commit -qm x", "x=$(git rev-parse HEAD)", "a && git config user.name t",
      "GIT_AUTHOR_NAME=t git commit -q", "if git diff --quiet; then :; fi", "if ! git diff --quiet; then :; fi", "echo `git log -1`")
    val passed = Seq("scratch_git \"$r\" commit -qm x", "scratch_repo \"$r\" --bare", "# git init in a comment",
      "check \".gitignore keeps candidates out of git\" 1", "cat > \"$work/bin/git\" <<'STUB'", "echo legit stuff")
    caught.filter(line => ShellGit.findFirstIn(code(line, Shell)).isEmpty) shouldBe empty
    passed.filter(line => ShellGit.findFirstIn(code(line, Shell)).isDefined) shouldBe empty
  }
}
