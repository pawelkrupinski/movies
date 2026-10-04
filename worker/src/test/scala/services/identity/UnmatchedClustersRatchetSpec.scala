package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.UnmatchedClusters
import tools.UnmatchedClusters.Take

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
 * The clusters the identity model leaves unmatched on the recorded full corpora ([[UnmatchedClusters]]' fixture),
 * resolved and put to the agreement stage again over their captured answers — the ratchet for matching more of them:
 *
 *  - ZERO WRONG: no take a label denies (`labels.tsv`: the recall targets, the must-not pairs, hand labels), none
 *    contradicting the right film a label names, and every take judged — a take no label covers fails, to be judged
 *    and added to `labels.tsv` before it counts;
 *  - ONLY MORE: every right take of `expected-matches.tsv` is still taken. A run that takes more right ones writes the
 *    new list to `target/identity-unmatched/expected-matches.tsv` — copy it over the checked-in one to ratchet.
 */
class UnmatchedClustersRatchetSpec extends AnyFlatSpec with Matchers {

  private val labels   = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
  private val expected = UnmatchedClusters.Directory.resolve("expected-matches.tsv")
  private val captures = models.Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read)
  private lazy val takes: Seq[Take] = captures.flatMap { capture =>
    val outcome = UnmatchedClusters.replay(capture)
    withClue(s"${capture.country.code}: the fixture lacks answers this resolve asks — re-capture it " +
      "(UnmatchedClustersCaptureIntegrationSpec): ") {
      outcome.model.unknownQueries shouldBe 0
      outcome.model.unknownFilms shouldBe 0
      outcome.stage.wanted shouldBe empty
      outcome.stage.wantedFinds shouldBe empty
    }
    UnmatchedClusters.takes(capture, outcome)
  }
  private def line(take: Take) = (take.country, take.venue, take.rawTitle, take.film)

  "the unmatched clusters' fixture" should "hold every country's capture" in {
    captures.map(_.country.code).toSet shouldBe Set("pl", "uk", "de", "es", "us")
  }

  "the resolver and the agreement over the unmatched clusters" should "take no film a label denies or contradicts" in {
    val wrong = takes.filter(take => UnmatchedClusters.verdict(take, labels).contains(false))
    withClue(wrong.map(UnmatchedClusters.takeLine).mkString("WRONG takes:\n", "\n", "\n")) { wrong shouldBe empty }
  }

  it should "take only films judged right — a new take is judged in labels.tsv before it counts" in {
    val unjudged = takes.filter(take => UnmatchedClusters.verdict(take, labels).isEmpty)
    withClue(unjudged.map(take => s"${UnmatchedClusters.takeLine(take)}\t${take.basis}\t${take.ids.mkString(",")}").mkString("UNJUDGED takes:\n", "\n", "\n")) {
      unjudged shouldBe empty
    }
  }

  it should "keep every right take of expected-matches.tsv, and write the list when more are right" in {
    val right = takes.filter(take => UnmatchedClusters.verdict(take, labels).contains(true))
    val had   = UnmatchedClusters.readTakeLines(expected)
    val now   = right.map(line).toSet
    val lost  = had -- now
    val counts = right.groupBy(_.country).view.mapValues(_.size).toSeq.sorted.map { case (cc, n) => s"$cc $n" }.mkString(", ")
    info(s"right takes: ${right.size} listings ($counts); expected ${had.size}")
    if (now != had) {
      val candidate = Path.of("target", "identity-unmatched", "expected-matches.tsv")
      Files.createDirectories(candidate.getParent)
      Files.writeString(candidate, right.map(UnmatchedClusters.takeLine).mkString("country\tvenue\trawTitle\tfilm\ttitle\n", "\n", "\n"), StandardCharsets.UTF_8)
      info(s"the right takes moved: wrote $candidate (+${(now -- had).size} −${lost.size})")
    }
    withClue(lost.toSeq.sorted.mkString("right takes LOST:\n", "\n", "\n")) { lost shouldBe empty }
  }
}
