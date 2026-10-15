package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{IdentityJudgements, UnmatchedClusters}
import tools.UnmatchedClusters.Take

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
 * The clusters the identity model leaves unmatched on the recorded full corpora ([[UnmatchedClusters]]' fixture),
 * resolved and put to the agreement stage again over their captured answers — the ratchet for matching more of them:
 *
 *  - ZERO WRONG: but those the identity judgements hold known-wrong in scope `capture` (no safe fix removes them; a
 *    cured row fails as stale, so they only shrink), no take a judgement denies (`identity-judgements.tsv`: the recall
 *    targets, the must-not pairs, hand labels), none contradicting the right film one names, and every take judged — a
 *    take no judgement covers fails, to be judged there before it counts;
 *  - ONLY MORE: every right take of `expected-matches.tsv` is still taken. A run that takes more right ones writes the
 *    new list to `target/identity-unmatched/expected-matches.tsv` — copy it over the checked-in one to ratchet.
 */
class UnmatchedClustersRatchetSpec extends AnyFlatSpec with Matchers {

  private val judged   = IdentityJudgements.read()
  private val labels   = IdentityJudgements.labels(judged)
  private val expected = UnmatchedClusters.Directory.resolve("expected-matches.tsv")
  private val captures = models.Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read)
  /** A measurement (the takes check) reads the capture's gaps as unknown and lists them; the ratchet fails on any. */
  private val gapsAsUnknown = UnmatchedClusters.gapsAsUnknown(sys.env)
  private lazy val takes: Seq[Take] = {
    val replayed = captures.map(capture => (capture, UnmatchedClusters.replay(capture)))
    val gaps     = replayed.flatMap { case (capture, outcome) => UnmatchedClusters.gaps(capture, outcome) }
    if (gapsAsUnknown) UnmatchedClusters.writeGaps(UnmatchedClusters.GapsPath, gaps)
    else replayed.foreach { case (capture, _) =>
      val its = gaps.filter(_.country == capture.country.code)
      withClue(s"${capture.country.code}: the fixture lacks answers this resolve asks — re-capture it " +
        s"(UnmatchedClustersCaptureIntegrationSpec): ${its.take(5).map(gap => s"${gap.kind} ${gap.lookup}").mkString("; ")}: ") {
        its shouldBe empty
      }
    }
    replayed.flatMap { case (capture, outcome) => UnmatchedClusters.takes(capture, outcome) }
  }
  private def line(take: Take) = (take.country, take.venue, take.rawTitle, take.film)

  /** The findings file, written before any assertion reads the takes: a red ratchet still leaves it to read. */
  private lazy val reported: Seq[Take] = {
    UnmatchedClusters.writeFindings(UnmatchedClusters.FindingsPath, UnmatchedClusters.findings(takes, labels, UnmatchedClusters.readTakeLines(expected)))
    takes
  }

  "the unmatched clusters' fixture" should "hold every country's capture" in {
    captures.map(_.country.code).toSet shouldBe Set("pl", "uk", "de", "es", "us")
  }

  it should "hold the venue detail of every host it marks as having detail" in {
    val missing = captures.map(capture => capture.country.code -> UnmatchedClusters.hostsMissingDetail(capture)).filter(_._2.nonEmpty)
    withClue("a capture marks listings as having venue detail and holds none of a host's — its recording's reads were refused; " +
      s"re-record the corpus (country -> host -> listings): ") { missing shouldBe empty }
  }

  private lazy val wrongAudit = UnmatchedClusters.auditWrong(reported.filter(take => UnmatchedClusters.verdict(take, labels).contains(false)),
    IdentityJudgements.tolerated(judged, IdentityJudgements.Scope.Capture).keySet)

  "the resolver and the agreement over the unmatched clusters" should "take no film a judgement denies or contradicts, but those known-wrong in scope capture" in {
    val wrong = wrongAudit.fresh
    withClue(wrong.map(UnmatchedClusters.takeLine).mkString("WRONG takes:\n", "\n", "\n")) { wrong shouldBe empty }
  }

  it should "still take every wrong film known-wrong in scope capture — a cured row is struck, so they only shrink" in {
    withClue(wrongAudit.stale.map(_.productIterator.mkString("\t")).mkString("known-wrong (capture) judgements no longer taken (STALE — " +
        "drop `capture` from the row's scope, or the row's state back to open):\n", "\n", "\n")) {
      wrongAudit.stale shouldBe empty
    }
  }

  it should "take only films judged right — a new take is judged in identity-judgements.tsv before it counts" in {
    val unjudged = reported.filter(take => UnmatchedClusters.verdict(take, labels).isEmpty)
    withClue(unjudged.map(take => s"${UnmatchedClusters.takeLine(take)}\t${take.basis}\t${take.ids.mkString(",")}").mkString("UNJUDGED takes:\n", "\n", "\n")) {
      unjudged shouldBe empty
    }
  }

  it should "keep every right take of expected-matches.tsv, and write the list when more are right" in {
    val right = reported.filter(take => UnmatchedClusters.verdict(take, labels).contains(true))
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
