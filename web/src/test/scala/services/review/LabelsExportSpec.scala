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

  it should "label a correction right for the new film and wrong for the one shown, when that is provably another film" in {
    LabelsExport.rowsOf(answer(ReviewVerdict.Film, shown = Some(dawnWall), ref = Some(FilmRef.tmdb(1461058))))
      .map(r => (r.film, r.verdict, r.note)) shouldBe Seq(
        ("tmdb:1461058", "right", "review page: hand label: tmdb:1461058"),
        ("tmdb:489471", "wrong", "review page: must not: Dawn Wall (2017)"))
    // another database's id the corpus links to a DIFFERENT film than the one shown
    val links = FilmIdentity.of(Seq(Set(FilmRef.tmdb(1461058), FilmRef("filmweb", "10008278"))))
    LabelsExport.rowsOf(answer(ReviewVerdict.Film, shown = Some(dawnWall), ref = Some(FilmRef("filmweb", "10008278"))), links)
      .map(r => (r.film, r.verdict)) shouldBe Seq("filmweb:10008278" -> "right", "tmdb:489471" -> "wrong")
    // another database's id nothing links: whether it is the same film cannot be told, so the shown film keeps its rows
    LabelsExport.rowsOf(answer(ReviewVerdict.Film, shown = Some(dawnWall), ref = Some(FilmRef("filmweb", "10008278"))))
      .map(r => (r.film, r.verdict)) shouldBe Seq("filmweb:10008278" -> "right")
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

  it should "never flip a right row for a correction naming the same film in another database" in {
    // the two flips a dry run of the 71 imported answers made: each answer names the labelled film by its TMDB id
    val existing = Seq(
      LabelRow("pl", "*", "FRANZ KAFKA", "filmweb:10008278", "right", "hand label: Franz Kafka (2025) Agnieszka Holland"),
      LabelRow("pl", "*", "LALKA / DOLLY", "rt:dolly", "right", "hand label: Dolly"))
    def correction(raw: String, labelled: FilmRef, chosen: FilmRef) = {
      val members = Seq(ReviewMember("Kino Opalenica", raw, Some("https://b24/" + raw)))
      ReviewAnswer(ReviewClusterId.of(members), "pl", ReviewPage.Matchable, ReviewVerdict.Film, Some(chosen), Some(FilmFacts(labelled)),
        raw, members, "dev", at)
    }
    val answers = Seq(correction("FRANZ KAFKA", FilmRef("filmweb", "10008278"), FilmRef.tmdb(1157322)),
      correction("LALKA / DOLLY", FilmRef("rt", "dolly"), FilmRef.tmdb(1309083)))
    val linked = FilmIdentity.of(Seq(Set(FilmRef.tmdb(1157322), FilmRef("filmweb", "10008278")), Set(FilmRef.tmdb(1309083), FilmRef("rt", "dolly"))))
    Seq(FilmIdentity.Unlinked, linked).foreach { identity =>
      val (rows, summary) = LabelsExport.merge(existing, answers, identity)
      summary.flipped shouldBe 0
      rows.take(2) shouldBe existing
      rows.drop(2).map(r => (r.rawTitle, r.film, r.verdict)) shouldBe
        Seq(("FRANZ KAFKA", "tmdb:1157322", "right"), ("LALKA / DOLLY", "tmdb:1309083", "right"))
    }
  }

  "two refs" should "be provably different films only by the same database's other id, or a link to another film" in {
    val linked = FilmIdentity.of(Seq(Set(FilmRef.tmdb(1), FilmRef("imdb", "tt1")), Set(FilmRef.tmdb(2), FilmRef("rt", "two"))))
    linked.provablyDifferent(FilmRef.tmdb(1), FilmRef.tmdb(2)) shouldBe true
    linked.provablyDifferent(FilmRef.tmdb(1), FilmRef.tmdb(1)) shouldBe false
    linked.provablyDifferent(FilmRef("imdb", "tt1"), FilmRef.tmdb(1)) shouldBe false
    linked.provablyDifferent(FilmRef("imdb", "tt1"), FilmRef.tmdb(2)) shouldBe true      // tt1 is tmdb 1
    linked.provablyDifferent(FilmRef("imdb", "tt1"), FilmRef("rt", "two")) shouldBe true  // tt1 is tmdb 1, rt two is tmdb 2
    linked.provablyDifferent(FilmRef("imdb", "tt9"), FilmRef.tmdb(2)) shouldBe false     // tt9: nothing known
    FilmIdentity.Unlinked.provablyDifferent(FilmRef("imdb", "tt1"), FilmRef.tmdb(2)) shouldBe false
  }

  it should "carry the answers' contradiction warnings into the summary" in {
    val (_, summary) = LabelsExport.merge(Nil, Seq(answer(ReviewVerdict.Right, warnings = Seq("Kino X states 1999"))))
    summary.render should include("WARNING: pl FRANZ KAFKA: Kino X states 1999")
  }

  "exporting" should "take only the answers given since the last export, and mark them used for the next" in {
    val dir   = Files.createTempDirectory("labels")
    val path  = dir.resolve("labels.tsv")
    val later = answer(ReviewVerdict.Wrong, shown = Some(dawnWall), members = Seq(opalenica.copy(venue = "Kino Muza")))
      .copy(at = at.plusSeconds(60))
    try {
      LabelsExport.exportTo(path, Seq(answer(ReviewVerdict.Right))).added shouldBe 1
      LabelsTsv.usedUntil(path) shouldBe Some(at)
      // the used answer, even contradicted by hand in the file since, is not applied again; the new one is
      LabelsTsv.write(path, LabelsTsv.read(path).map(_.copy(verdict = "wrong")))
      val second = LabelsExport.exportTo(path, Seq(answer(ReviewVerdict.Right), later))
      (second.added, second.flipped, second.alreadyUsed) shouldBe ((1, 0, 1))
      second.render should include ("1 answer already used")
      LabelsTsv.read(path).map(r => r.film -> r.verdict) shouldBe Seq("tmdb:1157322" -> "wrong", "tmdb:489471" -> "wrong")
      LabelsTsv.usedUntil(path) shouldBe Some(later.at)
    } finally { Files.deleteIfExists(LabelsTsv.usedPath(path)); Files.deleteIfExists(path); Files.deleteIfExists(dir): Unit }
  }

  it should "take back the rows of an exported answer withdrawn since, and restore a row it flipped" in {
    val dir   = Files.createTempDirectory("labels")
    val path  = dir.resolve("labels.tsv")
    val hand  = LabelRow("pl", "Kino Opalenica", "FRANZ KAFKA", "tmdb:1157322", "wrong", "by hand")
    val other = LabelRow("pl", "*", "Dune", "tmdb:438631", "right", "by hand")
    val right = answer(ReviewVerdict.Right)
    val undo  = answer(ReviewVerdict.Undo, shown = None).copy(at = at.plusSeconds(60))
    try {
      LabelsTsv.write(path, Seq(hand, other))
      LabelsExport.exportTo(path, Seq(right)).flipped shouldBe 1
      LabelsTsv.read(path).head.verdict shouldBe "right"
      val second = LabelsExport.exportTo(path, Seq(right, undo))
      second.withdrawn shouldBe 1
      second.render should include ("1 row of withdrawn answers taken back")
      LabelsTsv.read(path) shouldBe Seq(hand, other)   // the flipped row is the hand label again

      // a row the withdrawn answer added is removed; the file's other rows stay
      LabelsTsv.write(path, Seq(other)); Files.delete(LabelsTsv.usedPath(path))
      LabelsExport.exportTo(path, Seq(right)).added shouldBe 1
      LabelsExport.exportTo(path, Seq(right, undo)).withdrawn shouldBe 1
      LabelsTsv.read(path) shouldBe Seq(other)
      // and the next export has nothing more to take back
      LabelsExport.exportTo(path, Seq(right, undo)).withdrawn shouldBe 0
      LabelsTsv.read(path) shouldBe Seq(other)
    } finally { Files.deleteIfExists(LabelsTsv.usedPath(path)); Files.deleteIfExists(path); Files.deleteIfExists(dir): Unit }
  }

  it should "keep a withdrawn answer's row another standing answer also gives" in {
    val dir   = Files.createTempDirectory("labels")
    val path  = dir.resolve("labels.tsv")
    val muza  = ReviewMember("Kino Muza", "FRANZ KAFKA", Some("https://muza/kafka"))
    val both  = answer(ReviewVerdict.Right, members = Seq(opalenica, muza))           // FRANZ KAFKA under *
    val alone = answer(ReviewVerdict.Right, members = Seq(opalenica, muza.copy(page = Some("https://muza/kafka-2"))))
    val undo  = answer(ReviewVerdict.Undo, shown = None, members = Seq(opalenica, muza)).copy(at = at.plusSeconds(60))
    try {
      LabelsExport.exportTo(path, Seq(both, alone)).added shouldBe 1
      LabelsExport.exportTo(path, Seq(both, alone, undo)).withdrawn shouldBe 0
      LabelsTsv.read(path).map(r => r.venue -> r.verdict) shouldBe Seq("*" -> "right")
    } finally { Files.deleteIfExists(LabelsTsv.usedPath(path)); Files.deleteIfExists(path); Files.deleteIfExists(dir): Unit }
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
    } finally { Files.deleteIfExists(LabelsTsv.usedPath(copy)); Files.deleteIfExists(copy): Unit }
  }
}
