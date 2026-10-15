package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.ModuleSources

import java.nio.file.{Files, Path}

/**
 * Keeps every Mongo codec inside the round-trip guard.
 *
 * `PersistedCodecsRoundTripSpec` (worker/src/it) and `UserCodecsRoundTripSpec` (web/src/it)
 * cover whatever a [[PersistedCodecs]] registry lists. That is only a guarantee if nothing
 * derives a codec OUTSIDE such a list, and if every registry is named by a round-trip spec
 * — the two ways a new persisted type could slip past it. This checks both, off the sources.
 */
class PersistedCodecsLintSpec extends AnyFlatSpec with Matchers {


  private def text(path: Path): String = Files.readString(path)

  "production code" should "derive Mongo macro codecs only through PersistedCodecs" in {
    val offenders = ModuleSources.productionScalaFiles
      .filterNot(_.getFileName.toString == "PersistedCodecs.scala")
      .filter(path => text(path).contains("Macros.createCodecProvider"))
    withClue("derive the codec from the registry's `OmittingNone`/`WritingNone` list, so the round-trip spec sees it: ") {
      offenders.map(ModuleSources.named) shouldBe empty
    }
  }

  "every PersistedCodecs registry" should "be round-tripped by an it spec" in {
    val declared = """object\s+(\w+)\s+extends\s+PersistedCodecs""".r
    val registries = ModuleSources.productionScalaFiles.flatMap(path => declared.findAllMatchIn(text(path)).map(_.group(1)))
    registries should not be empty
    val roundTripped = ModuleSources.scalaFiles("it").map(text).filter(_.contains("PersistedRoundTrip.registry[")).mkString("\n")
    withClue("add the registry to PersistedCodecsRoundTripSpec (or web's UserCodecsRoundTripSpec): ") {
      registries.filterNot(name => roundTripped.contains(s"$name.registry")) shouldBe empty
    }
  }
}
