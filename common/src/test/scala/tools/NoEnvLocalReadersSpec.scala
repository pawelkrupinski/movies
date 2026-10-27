package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, NoSuchFileException, Path, SimpleFileVisitor}

/**
 * Nothing reads `.env.local` any more. The file was deleted on 2026-10-09 after a credential
 * theft, and every secret now lives in the Touch ID vault: the app reads only its process
 * (`Env.fromProcess`), and whatever launches something locally supplies the keys with
 * `secrets run movies KEY… -- <command>` (or `scripts/with-prod-mongo.sh`). A reader that
 * came back would quietly find nothing on one laptop and a stale, stolen value on another.
 *
 * Code only: a comment may still tell the history. A line counts as a comment when, trimmed,
 * it opens with a line-comment marker, a Scaladoc star, a block-comment opener or `#`, or
 * sits inside a Python docstring.
 */
class NoEnvLocalReadersSpec extends AnyFlatSpec with Matchers {

  private val Extensions = Seq(".scala", ".sbt", ".java", ".sh", ".py", ".ts", ".js", ".mjs", ".rb", ".yml", ".yaml", ".nix", ".tf")
  private val SkippedDirectories = Set(".git", ".claude", "target", "node_modules", "build", ".build", ".gradle", "fixtures", ".bsp", ".metals", ".idea")

  /** file → why it may name the file in code. */
  private val Allowed: Map[String, String] = Map(
    "common/src/test/scala/tools/NoEnvLocalReadersSpec.scala" -> "this lint",
    "worker/src/it/scala/tools/EnvIntegrationSpec.scala" ->
      "writes a .env.local to prove the process-bound Env never reads it",
    "worker/src/test/scala/deploy/ReleaseLanePublishSpec.scala" ->
      "asserts ios-release.sh does not mention it",
  )

  private def isComment(line: String): Boolean = {
    val trimmed = line.trim
    trimmed.startsWith("//") || trimmed.startsWith("*") || trimmed.startsWith("/*") || trimmed.startsWith("#")
  }

  /** A Python file's docstrings blanked, keeping their line breaks so line numbers hold. */
  private def withoutDocstrings(path: Path, text: String): String =
    if (!path.getFileName.toString.endsWith(".py")) text
    else "(?s)\"\"\".*?\"\"\"".r.replaceAllIn(text, found =>
      scala.util.matching.Regex.quoteReplacement(found.matched.filter(_ == '\n')))

  /** The skipped directories are pruned, never entered: a build running beside this spec (Zinc
   *  backing `classes` up into `classes.bak` and deleting it) changes `target` while it is walked,
   *  and `Files.walk` then fails on an entry gone between listing and reading it. For the same
   *  reason an entry that vanishes elsewhere is passed over rather than failing the walk. */
  private def sources(root: Path): Seq[Path] = {
    val found = Seq.newBuilder[Path]
    Files.walkFileTree(root, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attributes: BasicFileAttributes): FileVisitResult =
        if (dir != root && SkippedDirectories(dir.getFileName.toString)) FileVisitResult.SKIP_SUBTREE
        else FileVisitResult.CONTINUE
      override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
        if (attributes.isRegularFile && Extensions.exists(file.getFileName.toString.endsWith)) found += file
        FileVisitResult.CONTINUE
      }
      override def visitFileFailed(file: Path, failure: IOException): FileVisitResult = failure match {
        case _: NoSuchFileException => FileVisitResult.CONTINUE
        case other                  => throw other
      }
    })
    found.result()
  }

  "the repository" should "have no code that reads .env.local" in {
    val root = RepoRoot.dir.toPath
    val offenders = sources(root).flatMap { path =>
      val relative = root.relativize(path).toString
      if (Allowed.contains(relative)) Nil
      else withoutDocstrings(path, Files.readString(path)).linesIterator.zipWithIndex.collect {
        case (line, index) if line.contains(".env.local") && !isComment(line) => s"$relative:${index + 1}: ${line.trim}"
      }.toSeq
    }
    withClue("take the value from the process environment, launched under `secrets run movies KEY… -- <command>`: ") {
      offenders shouldBe empty
    }
  }

  "the allow-list" should "name only files that exist" in {
    Allowed.keys.filterNot(path => Files.exists(RepoRoot.dir.toPath.resolve(path))) shouldBe empty
  }
}
