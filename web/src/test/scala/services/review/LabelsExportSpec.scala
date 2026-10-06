package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.time.Instant

class LabelsExportSpec extends AnyFlatSpec with Matchers {

  private val at = Instant.parse("2026-10-06T08:00:00Z")
  private val opalenica = ReviewMember("Kino Opalenica", "FRANZ KAFKA", Some("https://b24/kafka"))
  private val kafka     = FilmFacts(FilmRef.tmdb(1157322), Some("Franz"), Some(2025), Seq("Agnieszka Holland"))
  private val dawnWall  = FilmFacts(FilmRef.tmdb(489471), Some("Dawn Wall"), Some(2017))

  private def answer(verdict: ReviewVerdict, shown: Option[FilmFacts] = Some(kafka), ref: Option[FilmRef] = None,
                     members: Seq[ReviewMember] = Seq(opalenica), warnings: Seq[String] = Nil) =
    ReviewAnswer(ReviewClusterId.of(members), "pl", ReviewPage.Queue, verdict, ref, shown, members.head.rawTitle, members, "dev", at, warnings)

  "an answer" should "become the rows its verdict means" in {
    LabelsExport.rowsOf(answer(ReviewVerdict.Right)).map(_.line) shouldBe
      Seq("pl\tKino Opalenica\tFRANZ KAFKA\ttmdb:1157322\tright\treview page: right: Franz (2025)")
    LabelsExport.rowsOf(answer(ReviewVerdict.Wrong)).map(r => r.verdict -> r.film) shouldBe Seq("wrong" -> "tmdb:1157322")
    Seq(ReviewVerdict.NoneOfThese -> "none of the candidates", ReviewVerdict.Event -> "not a film", ReviewVerdict.Bill -> "double bill")
      .foreach { case (verdict, note) =>
        LabelsExport.rowsOf(answer(verdict)).map(r => (r.verdict, r.film, r.note)) shouldBe
          Seq(("wrong", "tmdb:1157322", s"review page: $note: Franz (2025)"))
      }
  }

  it should "label a correction right for the new film and wrong for the one shown" in {
    LabelsExport.rowsOf(answer(ReviewVerdict.Film, shown = Some(dawnWall), ref = Some(FilmRef("filmweb", "10008278"))))
      .map(r => (r.film, r.verdict, r.note)) shouldBe Seq(
        ("filmweb:10008278", "right", "review page: hand label: filmweb:10008278"),
        ("tmdb:489471", "wrong", "review page: must not: Dawn Wall (2017)"))
    // "This film" on the film the card already showed is one right row, not right AND wrong
    LabelsExport.rowsOf(answer(ReviewVerdict.Film, ref = Some(kafka.ref))).map(_.verdict) shouldBe Seq("right")
  }

  it should "file a raw title billed by several venues under * and one per raw title" in {
    val members = Seq(opalenica, opalenica.copy(venue = "Kino Muza", page = Some("https://muza/kafka")),
      ReviewMember("Kino Muza", "Franz Kafka (napisy)", None))
    LabelsExport.rowsOf(answer(ReviewVerdict.Right, members = members)).map(r => r.venue -> r.rawTitle) shouldBe
      Seq("*" -> "FRANZ KAFKA", "Kino Muza" -> "Franz Kafka (napisy)")
  }

  it should "export nothing for an undo, or for a verdict on a card that showed no film" in {
    LabelsExport.rowsOf(answer(ReviewVerdict.Undo)) shouldBe empty
    LabelsExport.rowsOf(answer(ReviewVerdict.NoneOfThese, shown = None)) shouldBe empty
  }

  "merging into labels.tsv" should "add a new row, never duplicate one the file holds, and flip a contradicted one" in {
    val existing = Seq(
      LabelRow("pl", "*", "FRANZ KAFKA", "tmdb:1157322", "right", "recall target: Franz (2025)"),
      LabelRow("pl", "Kino Grajfka", "Klondike", "tmdb:913760", "right", "recall target: Klondike (2022)"))
    val (same, unchangedSummary) = LabelsExport.merge(existing, Seq(answer(ReviewVerdict.Right)))
    same shouldBe existing
    unchangedSummary.unchanged shouldBe 1
    unchangedSummary.added shouldBe 0

    val (flipped, flipSummary) = LabelsExport.merge(existing, Seq(answer(ReviewVerdict.Event)))
    flipped.head shouldBe LabelRow("pl", "*", "FRANZ KAFKA", "tmdb:1157322", "wrong",
      "review page: not a film: Franz (2025) (was: right: recall target: Franz (2025))")
    flipped.tail shouldBe existing.tail
    flipSummary.flipped shouldBe 1

    // re-exporting the same answers changes nothing more
    LabelsExport.merge(flipped, Seq(answer(ReviewVerdict.Event)))._1 shouldBe flipped

    val (added, addSummary) = LabelsExport.merge(existing, Seq(answer(ReviewVerdict.Wrong, shown = Some(dawnWall))))
    added shouldBe existing :+ LabelRow("pl", "Kino Opalenica", "FRANZ KAFKA", "tmdb:489471", "wrong", "review page: wrong: Dawn Wall (2017)")
    addSummary.added shouldBe 1
  }

  it should "carry the answers' contradiction warnings into the summary" in {
    val (_, summary) = LabelsExport.merge(Nil, Seq(answer(ReviewVerdict.Right, warnings = Seq("Kino X states 1999"))))
    summary.render should include("WARNING: pl FRANZ KAFKA: Kino X states 1999")
  }

  "the labels file" should "round-trip byte for byte through read and write" in {
    val path  = LabelsTsv.locate()
    val bytes = Files.readAllBytes(path)
    val copy  = Files.createTempFile("labels", ".tsv")
    try {
      LabelsTsv.write(copy, LabelsTsv.read(path))
      Files.readAllBytes(copy) shouldBe bytes
      LabelsExport.exportTo(copy, Seq(answer(ReviewVerdict.Wrong, shown = Some(dawnWall)))).added shouldBe 1
      LabelsTsv.read(copy).size shouldBe LabelsTsv.read(path).size + 1
    } finally Files.deleteIfExists(copy): Unit
  }
}
