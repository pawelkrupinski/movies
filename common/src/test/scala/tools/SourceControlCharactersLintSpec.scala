package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.ModuleSources

import java.nio.file.Files

/**
 * A raw NUL byte in a Scala source makes git treat the whole file as binary — its diffs stop
 * showing ("Bin 0 -> 6346 bytes") and a reviewer sees nothing — and the shell's grep skips it. A
 * control character a string needs is written as its escape (`"\u0000"`) instead.
 */
class SourceControlCharactersLintSpec extends AnyFlatSpec with Matchers {

  "Scala sources" should "hold no raw NUL byte" in {
    val offenders = ModuleSources.scalaFiles().filter(file => Files.readAllBytes(file).contains(0.toByte)).map(ModuleSources.named)
    withClue("write the character as its escape (\"\\u0000\"): ") { offenders shouldBe empty }
  }
}
