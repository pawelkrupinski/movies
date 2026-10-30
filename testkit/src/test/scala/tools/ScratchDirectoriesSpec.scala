package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Args, Reporter}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

/** A suite's temp paths go when it ends — passed or failed — and `TempDirectories.delete` removes a tree whatever its
 *  permissions. */
class ScratchDirectoriesSpec extends AnyFlatSpec with Matchers {

  private val quiet: Reporter = _ => ()

  /** A suite whose tests make paths in the constructor and in a test, the last test failing. */
  private final class Inner extends AnyFlatSpec with ScratchDirectories {
    val shared: Path        = tempDirectory("scratch-directories-shared")
    var made: Seq[Path]     = Seq(shared)
    "one" should "make a directory and a file" in {
      made ++= Seq(tempDirectory("scratch-directories-test"), tempFile("scratch-directories-test", ".txt"))
      Files.writeString(made.head.resolve("f"), "x")
      Files.exists(shared) shouldBe true
    }
    it should "fail" in fail("on purpose")
  }

  "a suite" should "delete every path it made when it ends, though a test failed" in {
    val inner = new Inner
    inner.run(None, Args(quiet)).succeeds() shouldBe false
    inner.made should have size 3
    inner.made.filter(Files.exists(_)) shouldBe empty
  }

  "TempDirectories.delete" should "remove a tree with an unreadable, unwritable directory in it" in {
    val root   = TempDirectories.create("scratch-directories-locked")
    val locked = Files.createDirectories(root.resolve("a/locked"))
    Files.writeString(locked.resolve("f"), "x")
    Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("---------"))
    TempDirectories.delete(root)
    Files.exists(root) shouldBe false
  }

  it should "leave alone a path it did not make" in {
    val made  = TempDirectories.create("scratch-directories-owner")
    val other = Files.createDirectories(made.resolve("not-made-by-temp-directories"))
    TempDirectories.delete(other)
    Files.exists(other) shouldBe true
    TempDirectories.delete(made)
    Files.exists(made) shouldBe false
  }

  "TempDirectories.within" should "delete its directory when the body throws" in {
    var seen: Option[Path] = None
    an[IllegalStateException] should be thrownBy TempDirectories.within("scratch-directories-within") { dir =>
      seen = Some(dir)
      throw new IllegalStateException("boom")
    }
    seen.exists(Files.exists(_)) shouldBe false
  }
}
