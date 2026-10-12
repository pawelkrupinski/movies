package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.IntegrationRunClaim.{RunProcess, rivals}

import java.time.Instant

class IntegrationRunClaimSpec extends AnyFlatSpec with Matchers {

  private val self      = RunProcess(100, Some(Instant.parse("2026-10-12T12:00:00Z")))
  private val neighbour = RunProcess(200, Some(Instant.parse("2026-10-12T12:01:00Z")))
  private val forked    = RunProcess(300, Some(Instant.parse("2026-10-12T12:02:00Z")))

  "rivals" should "name a live run that is no part of this one" in {
    rivals(self, Seq(self, neighbour), live = _ => true, sameRun = _ => false) shouldBe Seq(neighbour)
  }

  it should "pass over this run's own forks, its own claim, and runs that have ended" in {
    rivals(self, Seq(self, forked), live = _ => true, sameRun = _ == forked.pid) shouldBe empty
    rivals(self, Seq(neighbour), live = _ => false, sameRun = _ => false) shouldBe empty
  }
}
