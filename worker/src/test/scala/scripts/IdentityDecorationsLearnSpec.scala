package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import services.identity.TitleDecorations

import java.nio.file.{Files, Path}

/** The decorations relearn: what it keeps of the artefact it relearns on top of, and what it writes. */
class IdentityDecorationsLearnSpec extends AnyFlatSpec with Matchers {

  private def withDirectory(test: Path => Unit): Unit = {
    val dir = Files.createTempDirectory("decorations-learn")
    try test(dir) finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
  }
  private def nothingRecorded(dir: Path) = RecordedCorpora(dir.resolve("corpora"), dir.resolve("fixtures"), dir.resolve("hard-clusters"))

  private val konesera = TitleDecorations.Learned("prefix", "kino konesera", films = 12, venues = 3, titles = 40, examples = Seq("Kino Konesera: Lalka"))

  "A relearn" should "keep the decorations its artefact holds when this recording shows none, and stamp its version" in withDirectory { dir =>
    val out = dir.resolve("out/identity-decorations.json")
    IdentityDecorationsLearn.write(TitleDecorations.Artefact("decorations-2026-10-01", "basis", Map.empty, Seq(konesera)), out)
    val relearned = IdentityDecorationsLearn.relearn(nothingRecorded(dir), "decorations-2026-10-15", out)
    relearned.version shouldBe "decorations-2026-10-15"
    relearned.decorations shouldBe Seq(konesera)
    relearned.inputs("listingTitles") shouldBe 0
    Json.parse(Files.readString(out)).as[TitleDecorations.Artefact] shouldBe relearned
  }

  it should "write a fresh artefact where none was" in withDirectory { dir =>
    val out = dir.resolve("fresh/identity-decorations.json")
    IdentityDecorationsLearn.relearn(nothingRecorded(dir), "v1", out).decorations shouldBe empty
    Json.parse(Files.readString(out)).as[TitleDecorations.Artefact].version shouldBe "v1"
  }
}
