package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

/** The population walk's row cache ([[PopulationMonotonicitySpec.Cached]]): a slice kept under a digest is read back, the
 *  same rows; a slice whose files differ, or another digest, walks again; a failed walk is not kept. */
class PopulationWalkCacheSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {
  import PopulationMonotonicitySpec.{Cached, Row}

  private val rows = Seq(
    Row("Kino\tA", "Title \\ with\nbreaks\r", "film-none-film", "a@{x} -> b@{x, y}", "final", "lattice 2^3"),
    Row("", "", "", "", "model", "chain 0"))

  private def slice(root: Path, name: String, content: String): Path = {
    val dir = Files.createDirectories(root.resolve(name))
    Files.write(dir.resolve("slice.json.gz"), content.getBytes(StandardCharsets.UTF_8))
    dir
  }

  "The cached rows" should "decode to what was encoded, tabs, line breaks and backslashes too" in {
    Cached.decode(Cached.encode(rows)) shouldBe Some(rows)
    Cached.decode(Cached.encode(Nil)) shouldBe Some(Nil)
    Cached.decode(Seq("something else")) shouldBe None
  }

  "A kept slice" should "be read back, not walked again, until its files or the digest differ" in {
    val root   = tempDirectory("population-walk-cache")
    val dir    = slice(root, "s1", "one")
    val walks  = new AtomicInteger
    def walking(@annotation.unused d: Path) = { walks.incrementAndGet(); rows }
    val digest = root.resolve("cache").resolve("digest-a")
    Cached.walk(digest, dir, walking) shouldBe rows
    Cached.walk(digest, dir, walking) shouldBe rows
    walks.get shouldBe 1
    Files.write(dir.resolve("http.jsonl.gz"), "more".getBytes(StandardCharsets.UTF_8))
    Cached.walk(digest, dir, walking) shouldBe rows
    walks.get shouldBe 2
    Cached.walk(root.resolve("cache").resolve("digest-b"), dir, walking) shouldBe rows
    walks.get shouldBe 3
  }

  it should "key a file's bytes and its name" in {
    val root = tempDirectory("population-walk-key")
    val a    = Cached.key(slice(root, "a", "x"))
    val b    = Cached.key(slice(root, "b", "y"))
    val c    = slice(root, "c", "x")
    Cached.key(c) shouldBe a
    b should not be a
    Files.move(c.resolve("slice.json.gz"), c.resolve("other.json.gz"))
    Cached.key(c) should not be a
  }

  "A failed walk" should "not be kept" in {
    val root   = tempDirectory("population-walk-failed")
    val dir    = slice(root, "s", "z")
    val digest = root.resolve("d")
    an[IllegalStateException] should be thrownBy Cached.walk(digest, dir, _ => throw new IllegalStateException("boom"))
    Cached.walk(digest, dir, _ => rows) shouldBe rows
    Files.list(digest).count shouldBe 1
  }
}
