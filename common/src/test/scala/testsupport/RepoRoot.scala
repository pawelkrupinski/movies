package testsupport

import java.io.File

/** The repository root — the directory holding `build.sbt`.
 *
 *  Specs run from an unspecified working directory (sbt forks the `Test` config
 *  with the submodule's `baseDirectory`, but runs `PageTest` unforked from the
 *  build root), so a spec that reads a checked-in file — a bundled catalog
 *  seed, a committed share card — cannot name it relative to the CWD. Walking
 *  up to `build.sbt` resolves the same path either way.
 */
object RepoRoot {

  lazy val dir: File = {
    var d = new File(".").getCanonicalFile
    while (d != null && !new File(d, "build.sbt").exists()) d = d.getParentFile
    Option(d).getOrElse(sys.error(s"repo root (build.sbt) not found from ${new File(".").getCanonicalPath}"))
  }

  /** A repo-root-relative path, e.g. `RepoRoot.file("web/src/main/assets/img")`. */
  def file(relativePath: String): File = new File(dir, relativePath)
}
