package services.tasks

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration.*
import scala.util.Random

/**
 * The sample-then-re-check scaffolding both runtime-invariant audits run on. What it must get
 * right: a difference counts only when it is still there at the re-check (so a write in flight
 * never pages), the re-check travels in a delayed task (so whichever replica runs next confirms
 * it), and an id that cannot be judged is neither a pass nor a violation.
 */
class RecheckedAuditSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {

  private val now = Instant.parse("2026-09-24T12:00:00Z")

  private class Rig(sampleSize: Int = 10) {
    val queue    = new InMemoryTaskQueue
    val series   = new RecheckedAudit.Series("kinowo_worker_spec", "spec", Seq("pl"), new PrometheusRegistry())
    var broken   = Set.empty[String]
    var unknown  = Set.empty[String]
    val audit    = new RecheckedAudit("spec", TaskType.AuditReadModelContent, queue, series.forCountry("pl"),
      Clock.fixed(now, ZoneOffset.UTC), sampleSize, random = new Random(7))(id =>
      if (unknown(id)) None else Some(if (broken(id)) Seq(s"$id is wrong") else Nil))
    def counts: (Double, Double, Double) = series.counts("pl")
    /** Every queued task, claimed at `at`. */
    def claimable(at: Instant): Seq[Task] =
      Iterator.continually(queue.claim("spec", 1.minute, at)).takeWhile(_.isDefined).flatten.toSeq
  }

  "a sample" should "count what it judged, and queue what differed for a re-check fifteen minutes later" in new Rig {
    broken = Set("b", "d")
    audit.sample(Seq("a", "b", "c", "d")).toSet shouldBe Set("b", "d")
    counts shouldBe (4.0, 2.0, 0.0)                     // suspects are not violations yet
    claimable(now.plusSeconds(14 * 60)) shouldBe empty   // held back until the re-check is due
    val recheck = claimable(now.plusSeconds(15 * 60)).loneElement
    recheck.taskType shouldBe TaskType.AuditReadModelContent
    recheck.payload.values.flatMap(_.split("\n")).toSet shouldBe Set("b", "d")
  }

  it should "judge no more than its sample size" in new Rig(sampleSize = 3) {
    audit.sample((1 to 100).map(_.toString))
    counts._1 shouldBe 3.0
  }

  it should "neither pass nor fail an id it cannot judge, and queue nothing when all is well" in new Rig {
    unknown = Set("a")
    audit.sample(Seq("a", "b")) shouldBe empty
    counts shouldBe (1.0, 0.0, 0.0)
    claimable(now.plusSeconds(3600)) shouldBe empty
  }

  "a re-check" should "confirm only what still differs — a write that was in flight has landed by then" in new Rig {
    broken = Set("b", "d")
    audit.sample(Seq("a", "b", "c", "d"))
    broken = Set("d")                                    // b's projection landed meanwhile
    audit.handle(claimable(now.plusSeconds(15 * 60)).loneElement, () => fail("a re-check reads no ids"))
    counts shouldBe (4.0, 2.0, 1.0)
  }

  // The re-check's dedup key is one per audit, so a sample landing while an earlier re-check
  // still waits was DEDUPED — its suspects were never re-checked, and a violation found only
  // then was never confirmed. They join the waiting re-check instead.
  it should "re-check a later sample's suspects too when an earlier re-check is still waiting" in new Rig {
    broken = Set("b")
    audit.sample(Seq("a", "b"))
    broken = Set("b", "x")
    audit.sample(Seq("x", "y"))
    counts shouldBe (4.0, 2.0, 0.0)
    audit.handle(claimable(now.plusSeconds(15 * 60)).loneElement, () => fail("a re-check reads no ids"))
      .shouldBe(HandlerOutcome.Done)
    counts shouldBe (4.0, 2.0, 2.0)                     // b AND x confirmed
  }

  "the handler" should "sample what the id read returns, and sample nothing when that read did not complete" in new Rig {
    broken = Set("a")
    val task = Task("t1", TaskType.AuditReadModelContent, "spec-audit", Map.empty, attempts = 1)
    audit.handle(task, () => None) shouldBe HandlerOutcome.Skipped
    counts shouldBe (0.0, 0.0, 0.0)
    audit.handle(task, () => Some(Seq("a", "b"))) shouldBe HandlerOutcome.Done
    counts shouldBe (2.0, 1.0, 0.0)
  }
}
