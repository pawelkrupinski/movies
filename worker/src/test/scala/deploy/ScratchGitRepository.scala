package deploy

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.sys.process.*
import scala.util.Using

/**
 * A throwaway git repository for the specs that run the convergence CI scripts for real —
 * the bisect and the dispatch gate both decide from commit topology and changed paths, and
 * a spec that stubbed `git` would be asserting on the stub.
 *
 * Commits are made with fixed author and committer dates, so a repository built twice has
 * the same history, and nothing here reads the wall clock.
 */
final class ScratchGitRepository private (val root: Path, private var tick: Int) {

  def this() = {
    this(Files.createTempDirectory("scratch-git"), 0)
    git("init", "-q", "-b", "main")
    git("config", "user.email", "spec@example.test")
    git("config", "user.name", "Spec")
    git("config", "commit.gpgsign", "false")
  }

  def git(args: String*): String =
    Process(Seq("git") ++ args, root.toFile).!!.trim

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
    Process(Seq("git", "commit", "-q", "-m", subject), root.toFile,
      "GIT_AUTHOR_DATE" -> date, "GIT_COMMITTER_DATE" -> date).!!
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
