package deploy

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.sys.process.{Process, ProcessBuilder}
import scala.util.Using

/**
 * A throwaway git repository for the specs that run the convergence CI scripts for real —
 * the bisect and the dispatch gate both decide from commit topology and changed paths, and
 * a spec that stubbed `git` would be asserting on the stub.
 *
 * Commits are made with fixed author and committer dates, so a repository built twice has
 * the same history, and nothing here reads the wall clock.
 *
 * Every command it runs — its own `git`, and the script under test through [[process]] — runs
 * with the git environment ISOLATED (see [[ScratchGitRepository.isolate]]): an sbt started
 * from a git hook hands its test JVMs the hook's GIT_DIR, which beats the directory a command
 * runs in, so a scratch commit would otherwise land in the repository every worktree shares.
 */
final class ScratchGitRepository private (val root: Path, private var tick: Int) {

  def this() = {
    this(Files.createTempDirectory("scratch-git"), 0)
    git("init", "-q", "-b", "main")
  }

  def git(args: String*): String =
    process(Seq("git") ++ args).!!.trim

  /** `command` run in this repository, with the git environment isolated and `environment` added. */
  def process(command: Seq[String], environment: (String, String)*): ProcessBuilder =
    ScratchGitRepository.isolated(command, root, environment*)

  /** Write `files` (path → content) and commit them as `subject`; the new commit's SHA. */
  def commit(subject: String, files: (String, String)*): String = {
    files.foreach { case (path, content) =>
      val file = root.resolve(path)
      Files.createDirectories(file.getParent)
      Files.writeString(file, content)
    }
    git("add", "-A")
    tick += 1
    val date = s"2026-01-01T00:${f"$tick%02d"}:00Z"
    process(Seq("git", "commit", "-q", "-m", subject), "GIT_AUTHOR_DATE" -> date, "GIT_COMMITTER_DATE" -> date).!!
    git("rev-parse", "HEAD")
  }

  /** An independent copy of this repository — same commits, same SHAs, same next commit
   *  date — made by copying files rather than replaying commits. For a spec that runs the
   *  same history many times and lets each run mutate its own (bisect state, new commits):
   *  building it once and copying is a fraction of the ~30 `git` spawns a rebuild costs. */
  def copy(): ScratchGitRepository = {
    val target = Files.createTempDirectory("scratch-git")
    Using.resource(Files.walk(root)) { paths =>
      paths.forEach { source =>
        val dest = target.resolve(root.relativize(source))
        if (Files.isDirectory(source)) Files.createDirectories(dest)
        else Files.copy(source, dest, StandardCopyOption.COPY_ATTRIBUTES)
      }
    }
    new ScratchGitRepository(target, tick)
  }

  /** An executable script in the repository's sibling scratch space (never committed). */
  def script(name: String, body: String): Path = {
    val file = Files.createTempDirectory("scratch-bin").resolve(name)
    Files.writeString(file, "#!/usr/bin/env bash\n" + body)
    file.toFile.setExecutable(true)
    file
  }
}

object ScratchGitRepository {

  /** The identity and signing policy a scratch commit takes, as command-line-scope config, so
   *  no repository needs a `git config` of its own and no developer's ~/.gitconfig decides. */
  private val Configuration = Seq(
    "user.name" -> "Spec", "user.email" -> "spec@example.test",
    "commit.gpgsign" -> "false", "tag.gpgsign" -> "false", "init.defaultBranch" -> "main")

  /** `environment` (a child process's, about to start) stripped of every inherited GIT_*
   *  variable, with the developer's global and the system config switched off and the scratch
   *  identity supplied — the JVM twin of scripts/scratch-git.sh. */
  private[deploy] def isolate(environment: java.util.Map[String, String]): Unit = {
    environment.keySet.removeIf(_.startsWith("GIT_"))
    environment.put("GIT_CONFIG_NOSYSTEM", "1")
    environment.put("GIT_CONFIG_GLOBAL", "/dev/null")
    environment.put("GIT_CONFIG_COUNT", Configuration.size.toString)
    Configuration.zipWithIndex.foreach { case ((key, value), index) =>
      environment.put(s"GIT_CONFIG_KEY_$index", key)
      environment.put(s"GIT_CONFIG_VALUE_$index", value)
    }
  }

  /** `command` run in `directory` with the git environment isolated and `environment` added. */
  def isolated(command: Seq[String], directory: Path, environment: (String, String)*): ProcessBuilder = {
    val builder = new java.lang.ProcessBuilder(command*).directory(directory.toFile)
    isolate(builder.environment())
    environment.foreach { case (key, value) => builder.environment().put(key, value) }
    Process(builder)
  }
}
