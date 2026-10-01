package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.{Files, Path}
import scala.sys.process.*

/**
 * An earlier recording's identity overlay, carried into a new recording's leg, run for real.
 *
 * A new recording had no overlay of its own, so every question only the model asks went live again
 * after each recording — Wikidata rate-limited a Poland leg for over 1,000 s. The carry fills only what
 * the new tree lacks, leaves remembered verdicts behind, and names the files it added so the leg's
 * publish carries them on.
 */
class OverlayCarrySpec extends AnyFlatSpec with Matchers {

  private val Carry = new File(".github/scripts/carry-overlay.sh").getAbsolutePath

  private def write(file: Path, content: String): Unit = {
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)
    ()
  }

  "the carry" should "add only answers the new tree lacks, keep the recording's own, and leave verdicts behind" in {
    val old  = Files.createTempDirectory("old-overlay")
    write(old.resolve("test/resources/fixtures/enrichment-pl/www.wikidata.org/w/api.php.0.json"), "old answer")
    write(old.resolve("test/resources/fixtures/enrichment-pl/www.filmweb.pl/film.0.html"), "old page")
    write(old.resolve("test/resources/fixtures/enrichment-pl/.enrichment-cache/wikidata.entry"), "429")
    val archive = old.resolveSibling(s"${old.getFileName}-identity-overlay-pl-1.tar.gz")
    Seq("tar", "-czf", archive.toString, "-C", old.toString, "test").!! : Unit

    val leg = Files.createTempDirectory("leg")
    write(leg.resolve("test/resources/fixtures/enrichment-pl/www.filmweb.pl/film.0.html"), "new recording's page")
    val added = leg.resolve("overlay-files.txt")

    val out    = new StringBuilder
    val status = Process(Seq("bash", Carry, archive.toString, added.toString), leg.toFile).!(ProcessLogger(l => out.append(l).append('\n')))

    withClue(out.toString)(status shouldBe 0)
    Files.readString(leg.resolve("test/resources/fixtures/enrichment-pl/www.wikidata.org/w/api.php.0.json")) shouldBe "old answer"
    Files.readString(leg.resolve("test/resources/fixtures/enrichment-pl/www.filmweb.pl/film.0.html")) shouldBe "new recording's page"
    Files.exists(leg.resolve("test/resources/fixtures/enrichment-pl/.enrichment-cache/wikidata.entry")) shouldBe false
    Files.readAllLines(added).toArray.toSeq shouldBe Seq("test/resources/fixtures/enrichment-pl/www.wikidata.org/w/api.php.0.json")
  }
}
