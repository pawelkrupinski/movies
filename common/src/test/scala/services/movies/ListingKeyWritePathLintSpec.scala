package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Keeps every `movie_slots` / `screenings` write on the path that stamps the row's `listingKey`
 * (docs/design/identity-resolver.md, phase 4: the dual write beside today's keys).
 *
 * The stamp is made in exactly two places — `StoredSlotDto.of`, which derives it from the slot
 * row itself, and `ScreeningsSplit.screeningsOf` / `slotOps`, which hand each screenings row the
 * key of the slot it came from — and every writer reaches the collections through
 * `SlotsRepository` / `ScreeningsRepository`, whose Mongo implementations build their rows only
 * through those factories. A NEW write path breaks that in one of three ways, and each is a
 * rule here, read off the production sources:
 *
 *  1. building a storage row by its constructor instead of its factory (a row without a key);
 *  2. building a screenings payload somewhere other than the split, where the key is not known;
 *  3. writing either collection directly, beside the repositories.
 *
 * `ListingKeyDualWriteIntegrationSpec` proves the paths that exist today set it; this keeps the
 * next one from quietly not.
 */
class ListingKeyWritePathLintSpec extends AnyFlatSpec with Matchers {

  private val root = RepoRoot.dir.toPath

  private val mainSources: Seq[Path] =
    Seq("common", "worker", "web").map(module => root.resolve(s"$module/src/main/scala")).filter(Files.isDirectory(_))
      .flatMap(dir => Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala")).toSeq)

  private def named(path: Path): String = root.relativize(path).toString

  /** The source with comments removed, so a rule reads code, not the prose describing it. */
  private def code(path: Path): String =
    Files.readString(path).replaceAll("(?s)/\\*.*?\\*/", "").replaceAll("//[^\n]*", "")

  /** Every call `Name(` that is not the declaration, the factory or a type argument. */
  private def constructions(name: String): Seq[(String, Int)] = {
    val call = raw"(?<![\w.\[])$name\(".r
    mainSources.flatMap { path =>
      val src = code(path).replace(s"case class $name(", "")
      val n   = call.findAllMatchIn(src).size
      Option.when(n > 0)(named(path) -> n)
    }
  }

  "a side-collection storage row" should "be built only by its factory, which stamps the listing key" in {
    withClue("build a `movie_slots` row with `StoredSlotDto.of`, never its constructor: ") {
      constructions("StoredSlotDto") shouldBe Seq("common/src/main/scala/services/movies/SlotsRepository.scala" -> 1)
    }
    withClue("build a `screenings` row with `StoredScreeningsDto.of`, never its constructor: ") {
      constructions("StoredScreeningsDto") shouldBe Seq("common/src/main/scala/services/movies/ScreeningsRepository.scala" -> 1)
    }
  }

  "a screenings payload" should "be built only where the slot it belongs to is known" in {
    withClue("derive screenings rows through `ScreeningsSplit.screeningsOf` / `slotOps`, which key them by their slot: ") {
      constructions("ListedShowtimes").map(_._1).toSet shouldBe Set(
        "common/src/main/scala/services/movies/ScreeningsSplit.scala",       // the write path's rows, keyed by their slot
        "common/src/main/scala/services/movies/ScreeningsRepository.scala")  // a stored row read back, with the key it holds
    }
  }

  "the side collections" should "be written only through their repositories" in {
    val names  = """movie_slots"|"screenings"|SlotsRepository\.Collection|ScreeningsRepository\.Collection""".r
    val writes = """\b(insertOne|insertMany|replaceOne|updateOne|updateMany|findOneAndUpdate|findOneAndReplace|ReplaceOneModel|UpdateOneModel|UpdateManyModel|InsertOneModel)\b""".r
    val allowed = Set(
      "common/src/main/scala/services/movies/SlotsRepository.scala",
      "common/src/main/scala/services/movies/ScreeningsRepository.scala",
      // Names the collections only to key its OWN resume-token document, which is what it writes.
      "common/src/main/scala/services/movies/ChangeStreamResumeToken.scala")
    val offenders = mainSources.filter { path =>
      val src = code(path)
      names.findFirstIn(src).isDefined && writes.findFirstIn(src).isDefined
    }.map(named).filterNot(allowed)
    withClue("write `movie_slots` / `screenings` through SlotsRepository / ScreeningsRepository, which stamp `listingKey`: ") {
      offenders shouldBe empty
    }
  }
}
