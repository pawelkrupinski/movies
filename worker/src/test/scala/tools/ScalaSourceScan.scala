package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Text-level helpers for the source lints (`NoWallClockInTestsSpec`,
 *  `NoPolandDefaultCountrySpec`, `NoSwallowedFailureSpec`): they read the repository's own `.scala` files from the
 *  build root (these specs run unforked, so the working directory is the repo root). */
object ScalaSourceScan {

  val MainRoots: Seq[String] = Seq("common/src/main", "web/src/main", "worker/src/main")

  def scalaFiles(roots: Seq[String]): Seq[Path] =
    roots.map(Paths.get(_)).filter(Files.isDirectory(_)).flatMap { root =>
      Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq
    }.sortBy(_.toString)

  def read(p: Path): String = new String(Files.readAllBytes(p), StandardCharsets.UTF_8)

  /** The line with any `//` comment dropped, or "" for a Scaladoc/block-comment line. */
  def code(line: String): String = {
    val trimmed = line.trim
    if (trimmed.startsWith("*") || trimmed.startsWith("/*")) ""
    else {
      val slashes = Iterator.iterate(line.indexOf("//"))(i => line.indexOf("//", i + 1))
        .takeWhile(_ >= 0)
        .find(i => line.substring(0, i).count(_ == '"') % 2 == 0)
      slashes.fold(line)(line.substring(0, _))
    }
  }

  /** `path`'s source with comments dropped line by line (line numbers preserved). */
  def codeOf(p: Path): String = read(p).linesIterator.map(code).mkString("\n")

  /** The index of the `)` matching the `(` at `open`, or the text's end. */
  def closingParen(text: String, open: Int): Int = {
    var depth = 0
    var i     = open
    while (i < text.length) {
      text(i) match {
        case '(' => depth += 1
        case ')' =>
          depth -= 1
          if (depth == 0) return i
        case _ =>
      }
      i += 1
    }
    text.length
  }

  /** The argument text of the call whose `(` is at `open`. */
  def argumentsAt(text: String, open: Int): String =
    text.substring(open + 1, math.max(open + 1, closingParen(text, open)))

  /** `text` split on its top-level commas — the parameters of one clause. */
  def topLevelParts(text: String): Seq[String] = {
    val parts = Vector.newBuilder[String]
    var depth = 0
    var from  = 0
    text.indices.foreach { i =>
      text(i) match {
        case '(' | '[' | '{' => depth += 1
        case ')' | ']' | '}' => depth -= 1
        case ',' if depth == 0 => parts += text.substring(from, i); from = i + 1
        case _ =>
      }
    }
    (parts += text.substring(from)).result().filter(_.trim.nonEmpty)
  }
}
