package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Keeps every Mongo codec inside the round-trip guard.
 *
 * `PersistedCodecsRoundTripSpec` (worker/src/it) and `UserCodecsRoundTripSpec` (web/src/it)
 * cover whatever a [[PersistedCodecs]] registry lists. That is only a guarantee if nothing
 * derives a codec OUTSIDE such a list, and if every registry is named by a round-trip spec
 * — the two ways a new persisted type could slip past it. This checks both, off the sources.
 */
class PersistedCodecsLintSpec extends AnyFlatSpec with Matchers {

  private def scalaFiles(under: String): Seq[Path] = {
    val root = RepoRoot.dir.toPath
    Seq("common", "worker", "web").map(module => root.resolve(s"$module/src/$under")).filter(Files.isDirectory(_))
      .flatMap(dir => Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala")).toSeq)
  }

  private def text(path: Path): String = Files.readString(path)

  "production code" should "derive Mongo macro codecs only through PersistedCodecs" in {
    val offenders = scalaFiles("main/scala")
      .filterNot(_.getFileName.toString == "PersistedCodecs.scala")
      .filter(path => text(path).contains("Macros.createCodecProvider"))
    withClue("derive the codec from the registry's `OmittingNone`/`WritingNone` list, so the round-trip spec sees it: ") {
      offenders.map(RepoRoot.dir.toPath.relativize(_).toString) shouldBe empty
    }
  }

  "every PersistedCodecs registry" should "be round-tripped by an it spec" in {
    val declared = """object\s+(\w+)\s+extends\s+PersistedCodecs""".r
    val registries = scalaFiles("main/scala").flatMap(path => declared.findAllMatchIn(text(path)).map(_.group(1)))
    registries should not be empty
    val roundTripped = scalaFiles("it/scala").map(text).filter(_.contains("PersistedRoundTrip.registry[")).mkString("\n")
    withClue("add the registry to PersistedCodecsRoundTripSpec (or web's UserCodecsRoundTripSpec): ") {
      registries.filterNot(name => roundTripped.contains(s"$name.registry")) shouldBe empty
    }
  }
}
