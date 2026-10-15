package testsupport

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The build's source directories, found by walking the repository rather than named module by module.
 *
 *  A module is a top-level directory with a `src/<config>/scala` tree. A lint that listed its modules by hand
 *  stopped covering the next one: six missed `identity/` when the resolver became a module of its own (2026-10-14).
 */
object ModuleSources {

  val root: Path = RepoRoot.dir.toPath

  /** Every `<module>/src/<config>` directory of a module, sorted; only the `configs` named (`main`, `test`, `it`,
   *  `page`, `fixtures`), or all of them when none are. */
  def configDirectories(configs: String*): Seq[Path] =
    Files.list(root).iterator.asScala.toSeq
      .map(_.resolve("src"))
      .filter(Files.isDirectory(_))
      .flatMap(src => Files.list(src).iterator.asScala.toSeq)
      .filter(dir => Files.isDirectory(dir.resolve("scala")))
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
