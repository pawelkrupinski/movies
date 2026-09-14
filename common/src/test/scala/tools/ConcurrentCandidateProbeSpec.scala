package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConcurrentCandidateProbeSpec extends AnyFlatSpec with Matchers {

  "firstMatch" should "return None for an empty candidate list" in {
    ConcurrentCandidateProbe.firstMatch("t", Seq.empty[Int])(_ => Some("x")) shouldBe None
  }

  it should "return the first candidate (by priority order) that matches" in {
    ConcurrentCandidateProbe.firstMatch("t", Seq(1, 2, 3)) { c =>
      if (c == 2) Some(s"matched-$c") else None
    } shouldBe Some("matched-2")
  }

  it should "return None when no candidate matches" in {
    ConcurrentCandidateProbe.firstMatch("t", Seq(1, 2, 3))(_ => None) shouldBe None
  }

  // The whole point: N candidates run CONCURRENTLY, so N probes each blocking
  // for `delay` complete in about one `delay`, not N of them summed.
  it should "run every candidate's probe concurrently rather than one at a time" in {
    val delay = 150
    val candidates = 1 to 4
    val start = System.currentTimeMillis()
    ConcurrentCandidateProbe.firstMatch("t", candidates.toSeq) { c =>
      Thread.sleep(delay.toLong)
      None
    }
    val elapsed = System.currentTimeMillis() - start
    // Sequential would take ~4*150=600ms; concurrent should land near one
    // delay. Generous ceiling to absorb scheduling jitter in CI.
    elapsed should be < (delay * candidates.size / 2).toLong
  }

  // The correctness guarantee ConcurrentCandidateProbe exists for: a candidate
  // that answers FASTER must never win over an earlier-priority candidate that
  // is merely slower to answer — mirrors MetacriticClientSpec's "Odyssey"
  // regression, where the wrong-film bare slug would 200 just as readily as
  // the right year-suffixed one.
  it should "let priority order decide the winner, never response speed" in {
    val winner = ConcurrentCandidateProbe.firstMatch("t", Seq("slow-correct", "fast-wrong")) {
      case "slow-correct" => Thread.sleep(150); Some("correct")
      case "fast-wrong"   => Some("wrong")
      case other          => fail(s"unexpected candidate $other")
    }
    winner shouldBe Some("correct")
  }

  // A hard failure (a block/throttle/5xx, not an absence — see EnrichmentRead)
  // must propagate once its candidate's turn comes, exactly as a sequential
  // probe would abort there — even though a LOWER-priority candidate's future
  // may already have completed with what looks like a match.
  it should "propagate an earlier candidate's failure rather than let a later candidate's match win" in {
    val boom = new RuntimeException("HTTP 503")
    val thrown = intercept[RuntimeException] {
      ConcurrentCandidateProbe.firstMatch("t", Seq("fails", "would-match")) {
        case "fails"       => throw boom
        case "would-match" => Some("should never be seen")
        case other         => fail(s"unexpected candidate $other")
      }
    }
    thrown shouldBe boom
  }
}
