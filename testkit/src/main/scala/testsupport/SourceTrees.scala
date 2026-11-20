package testsupport

import java.nio.file.{FileSystems, Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

/** A DECLARED read of the repository's source trees: the one way test code walks, lists or opens a path under a
 *  module's `src/` -- `SourceTrees("worker/src/main", "scrapers/src/main/scala/services/cinemas")`, or
 *  a glob, every module's `src/<config>/scala` (`*` any run of characters but `/`, a `**` segment any number of segments).
 *
 *  WHY ONE HELPER. No class dependency records that a lint walking `worker/src/main` reads `MovieService.scala`, so the
 *  test-impact selection (project/TestImpact.scala) must be told: it reads the string literals of every
 *  `SourceTrees(...)` call (project/ReaderNames.scala, `declaredTrees`) and selects, for a changed file under a declared
 *  tree, the declaring source and every test built on it. Spotting path literals anywhere instead missed every new
 *  shape a walk was built in (a module in a variable, `Path.of(".", "identity", "src")`, `.resolve("src")`), one review
 *  round each; SourceTreeWalksLintSpec now fails test code reaching a source tree any other way, and a declaration
 *  holding anything but literals. Paths are repository-relative where the JVM runs from the repository root (every
 *  forked test JVM: project/ForkedTests.scala), and absolute under it elsewhere (web's Test JVM, which Play forks in
 *  `web/`). */
final class SourceTrees private (val declared: Seq[String]) {

  /** These trees and `other`'s. */
  def ++(other: SourceTrees): SourceTrees = new SourceTrees((declared ++ other.declared).distinct)

  /** These trees but `other`'s. */
  def --(other: SourceTrees): SourceTrees = new SourceTrees(declared.filterNot(other.declared.contains))

  /** The existing directories (or files) the declared trees match, sorted. */
  def directories: Seq[Path] = declared.flatMap(SourceTrees.expand).distinct.sortBy(_.toString)

  /** The declared trees matching no existing path: a lint whose tree is gone scans nothing and passes. */
  def missing: Seq[String] = declared.filter(SourceTrees.expand(_).isEmpty)

  /** Every regular file under the declared trees whose name ends with `suffix`, sorted. */
  def files(suffix: String): Seq[Path] = directories.flatMap { root =>
    // `Files.walk` holds a directory handle open per level until the stream is closed.
    Using.resource(Files.walk(root))(_.iterator.asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(suffix)).toList)
  }.distinct.sortBy(_.toString)

  /** Every `.scala` file under the declared trees. */
  def scalaFiles: Seq[Path] = files(".scala")

  /** The one declared tree's path, a glob's never. */
  def path: Path = {
    require(declared.size == 1 && !declared.head.contains('*'), s"one tree, no glob: ${declared.mkString(", ")}")
    SourceTrees.base.resolve(declared.head)
  }

  /** `name` under the one declared tree: a file read by a name built at run time (`s"$rule.scala"`). */
  def file(name: String): Path = path.resolve(name)

  override def toString: String = declared.mkString("SourceTrees(", ", ", ")")
}

object SourceTrees {

  /** The declaration: string literals only (SourceTreeWalksLintSpec), each a tree under a module's `src/`. */
  def apply(trees: String*): SourceTrees = {
    require(trees.nonEmpty, "declare at least one tree")
    new SourceTrees(trees.map(_.stripSuffix("/")).distinct)
  }

  /** Where the trees resolve from: `""` (paths stay repository-relative) from the root, else the root itself, the
   *  nearest directory above holding `build.sbt`. */
  private[testsupport] lazy val base: Path = {
    val here = Paths.get("").toAbsolutePath
    val root = Iterator.iterate(here)(_.getParent).takeWhile(_ != null).find(dir => Files.exists(dir.resolve("build.sbt")))
      .getOrElse(sys.error(s"no build.sbt above $here"))
    if (root == here) Paths.get("") else root
  }

  /** The paths `tree` matches from the repository root, one `/`-separated segment at a time. */
  private def expand(tree: String): Seq[Path] =
    tree.split('/').toSeq.filter(_.nonEmpty).foldLeft(Seq(base)) { (paths, segment) =>
      if (segment == "**") paths.flatMap(path => Using.resource(Files.walk(path))(_.iterator.asScala.filter(Files.isDirectory(_)).toList))
      else if (segment.contains('*')) {
        val matcher = FileSystems.getDefault.getPathMatcher("glob:" + segment)
        paths.filter(Files.isDirectory(_)).flatMap(dir => Using.resource(Files.list(dir))(_.iterator.asScala.toList))
          .filter(path => Files.isDirectory(path) && matcher.matches(path.getFileName))
      } else paths.map(_.resolve(segment)).filter(Files.exists(_))
    }.filter(_.toString.nonEmpty)
}
