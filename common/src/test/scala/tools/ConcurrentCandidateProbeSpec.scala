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

  // The whole point: N candidates run CONCURRENTLY. Each probe arrives at an
  // N-party latch and waits for the others there, so every probe gets through
  // only if all N are in flight at once; run one at a time, the first would
  // stall on the latch until the timeout. Structural, so no wall-clock
  // ceiling for a loaded CI machine to blow (the old "< 300ms" check measured
  // 1084ms mid-testUnit on 2026-09-26 and failed while the code was fine).
  it should "run every candidate's probe concurrently rather than one at a time" in {
    val candidates = 1 to 4
    val allInFlight = new java.util.concurrent.CountDownLatch(candidates.size)
    val metAll = new java.util.concurrent.atomic.AtomicInteger(0)
    ConcurrentCandidateProbe.firstMatch("t", candidates.toSeq) { _ =>
      allInFlight.countDown()
      if (allInFlight.await(10, java.util.concurrent.TimeUnit.SECONDS)) metAll.incrementAndGet()
      None
    }
    metAll.get shouldBe candidates.size
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

  // `maxConcurrent` bounds how many probes are ever IN FLIGHT at once — a poster
  // fallback race holds a full image download + decode per candidate, so an
  // unbounded fan-out multiplies peak memory by the candidate count (web-pl's
  // OOM kills, 2026-09-17: ConcurrentCandidateProbe racing all 5 poster
  // fallbacks at once). Batching into rounds trades some latency for a hard
  // cap on concurrent memory.
  it should "cap concurrent probes at maxConcurrent, running the rest in later rounds" in {
    val delay = 150
    val candidates = 1 to 4
    val start = System.nanoTime() / 1000000
    ConcurrentCandidateProbe.firstMatch("t", candidates.toSeq, maxConcurrent = 2) { c =>
      Thread.sleep(delay.toLong)
      None
    }
    val elapsed = System.nanoTime() / 1000000 - start
    // 4 candidates at 2-at-a-time is 2 rounds of ~150ms — comfortably more than
    // one round (the unbounded case) and comfortably less than 4 sequential.
    elapsed should be >= delay.toLong
    elapsed should be < (delay * candidates.size).toLong
  }

  it should "still return the highest-priority match across round boundaries" in {
    val result = ConcurrentCandidateProbe.firstMatch("t", Seq(1, 2, 3, 4), maxConcurrent = 2) { c =>
      if (c == 3) Some(s"matched-$c") else None
    }
    result shouldBe Some("matched-3")
  }

  it should "treat an unspecified maxConcurrent exactly as before (one unbounded round)" in {
    ConcurrentCandidateProbe.firstMatch("t", Seq(1, 2, 3)) { c =>
      if (c == 2) Some(s"matched-$c") else None
    } shouldBe Some("matched-2")
  }
}
