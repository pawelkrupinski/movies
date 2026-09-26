package services.readmodel

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer
import tools.ReadModelDerivationCorpus

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

/**
 * What keeps [[ReadModelDerivation]] honest, in seconds and without booting the pipeline: the
 * checked-in derivation corpus's rows must still project under this code to the hashes recorded
 * beside them, and the derivation recorded there must be the one this code names.
 *
 * A change to what the projection derives from a row fails the first check: the same rows now
 * project differently. `FilmScheduleEndToEndSpec` then regenerates the corpus and names the new
 * derivation — cards-only or full, from which parts moved — and the second check fails until it
 * is appended to `ReadModelDerivation.History`, so no such change can ship without every worker
 * re-projecting its stored corpus once after the deploy. A change that only moves the ROWS (a
 * scraper, a fixture) passes both: the rows it adds were never recorded, and the rows that were
 * still project as recorded.
 */
class ReadModelDerivationVersionSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.default)
  private lazy val recorded =
    ReadModelDerivationCorpus.parseHashes(Files.readString(ReadModelDerivationCorpus.HashesPath, UTF_8))

  "the checked-in derivation corpus" should "project under this code exactly as it was recorded" in {
    val rows  = ReadModelDerivationCorpus.parseRows(Files.readString(ReadModelDerivationCorpus.RowsPath, UTF_8), normalizer)
    val moved = ReadModelDerivationCorpus.moved(recorded, rows, normalizer)
    withClue(
      s"${moved.size} checked-in row(s) project differently under this code than they were recorded to " +
        s"(${moved.take(5).map(m => s"${m.title}: ${m.parts.mkString(", ")}").mkString("; ")}), so what the projection " +
        "derives has changed. Regenerate the derivation corpus — it names the new derivation:\n" +
        "  sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'\n") {
      moved shouldBe empty
    }
  }

  "this code's derivation" should "be the one the checked-in corpus was recorded under" in {
    withClue(
      s"The derivation corpus was recorded under ${recorded.derivation}. Append to ReadModelDerivation.History:\n" +
        s"  ${ReadModelDerivationCorpus.historyEntry(recorded.derivation)}\n" +
        "so every worker re-projects its stored corpus once after the deploy (see ReadModelDerivationMarker).\n") {
      ReadModelDerivation.History.last shouldBe recorded.derivation
    }
  }
}
