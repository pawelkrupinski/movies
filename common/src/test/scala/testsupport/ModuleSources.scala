package testsupport

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The build's source directories, found by walking the repository rather than named module by module.
 *
 *  A module is a top-level directory with a `src/<config>/scala` tree. A lint that listed its modules by hand
 *  stopped covering the next one: six missed `identity/` when the resolver became a module of its own (2026-10-14).
 */
object ModuleSources {

  val root: Path = RepoRoot.dir.toPath

  /** The trees every lint built on this walks: each module's `src/<config>/scala`, by a GLOB, never module names. The
   *  test-impact selection reads the declaration (project/ReaderNames.scala, `declaredTrees`) as these lints reading
   *  every file under the trees it matches, so a change in any module selects them (ReaderNamesSpec). */
  val Trees: SourceTrees = SourceTrees("*/src/*/scala")

  /** Every `<module>/src/<config>/scala` directory of a module ([[Trees]]), sorted; only the `configs` named (`main`,
   *  `test`, `it`, `page`, `fixtures`), or all of them when none are. */
  def scalaDirectories(configs: String*): Seq[Path] =
    Trees.directories.filter(dir => configs.isEmpty || configs.contains(dir.getParent.getFileName.toString)).sorted

  /** Every `.scala` file under those directories. */
  def scalaFiles(configs: String*): Seq[Path] =
    scalaDirectories(configs*).flatMap(dir => Using.resource(Files.walk(dir))(_.iterator.asScala.filter(_.toString.endsWith(".scala")).toList))

  /** The modules whose `main` sources are test support (the shared fakes and harnesses), not production code. */
  val TestSupport: Set[String] = Set("testkit")

  /** Every production `.scala` file: the `main` sources of every module but the test-support ones. */
  def productionScalaFiles: Seq[Path] =
    scalaFiles("main").filterNot(file => TestSupport.contains(named(file).takeWhile(_ != '/')))

  /** `path` relative to the repository root, `/`-separated. */
  def named(path: Path): String = (if (path.isAbsolute) root.relativize(path) else path).toString.replace('\\', '/')
}
