package testsupport

import java.nio.file.{FileSystems, Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The build's source directories, found by walking the repository rather than named module by module.
 *
 *  A module is a top-level directory with a `src/<config>/scala` tree. A lint that listed its modules by hand
 *  stopped covering the next one: six missed `identity/` when the resolver became a module of its own (2026-10-14).
 */
object ModuleSources {

  val root: Path = RepoRoot.dir.toPath

  /** The trees every lint built on this walks: each module's `src/<config>/scala`. A GLOB, never module names, and one
   *  literal: the test-impact selection (project/ReaderNames.scala, `globWalks`) reads it as these lints reading every
   *  file under the trees it matches, so a change in any module selects them (ReaderNamesSpec). */
  val SourceTrees: String = "*/src/*/scala"

  /** The directories `glob` matches under the root, one `/`-separated segment at a time. */
  private def expand(glob: String): Seq[Path] =
    glob.split('/').toSeq.foldLeft(Seq(root)) { (dirs, segment) =>
      val matcher = FileSystems.getDefault.getPathMatcher("glob:" + segment)
      dirs.flatMap(dir => Using.resource(Files.list(dir))(_.iterator.asScala.toList))
        .filter(path => Files.isDirectory(path) && matcher.matches(path.getFileName))
    }

  /** Every `<module>/src/<config>` directory of a module ([[SourceTrees]]), sorted; only the `configs` named (`main`,
   *  `test`, `it`, `page`, `fixtures`), or all of them when none are. */
  def configDirectories(configs: String*): Seq[Path] =
    expand(SourceTrees).map(_.getParent)
      .filter(dir => configs.isEmpty || configs.contains(dir.getFileName.toString))
      .sorted

  /** Every `.scala` file under those directories. */
  def scalaFiles(configs: String*): Seq[Path] =
    configDirectories(configs*).flatMap(dir => Files.walk(dir).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq)

  /** The modules whose `main` sources are test support (the shared fakes and harnesses), not production code. */
  val TestSupport: Set[String] = Set("testkit")

  /** Every production `.scala` file: the `main` sources of every module but the test-support ones. */
  def productionScalaFiles: Seq[Path] =
    scalaFiles("main").filterNot(file => TestSupport.contains(root.relativize(file).getName(0).toString))

  /** `path` relative to the repository root, `/`-separated. */
  def named(path: Path): String = root.relativize(path).toString.replace('\\', '/')
}
