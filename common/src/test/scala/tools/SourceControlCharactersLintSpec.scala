package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/**
 * A raw NUL byte in a Scala source makes git treat the whole file as binary — its diffs stop
 * showing ("Bin 0 -> 6346 bytes") and a reviewer sees nothing — and the shell's grep skips it. A
 * control character a string needs is written as its escape (`"\u0000"`) instead.
 */
class SourceControlCharactersLintSpec extends AnyFlatSpec with Matchers {

  "Scala sources" should "hold no raw NUL byte" in {
    val root = RepoRoot.dir.toPath
    val offenders = for {
      module <- Seq("common", "testkit", "worker", "web", "e2e")
      dir     = root.resolve(s"$module/src")
      if Files.isDirectory(dir)
      file   <- Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala")).toSeq
      if Files.readAllBytes(file).contains(0.toByte)
    } yield root.relativize(file).toString
    withClue("write the character as its escape (\"\\u0000\"): ") { offenders shouldBe empty }
  }
}
