package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class ChunkScrapeStoreSpec extends AnyFlatSpec with Matchers {

  private val now   = Instant.parse("2026-06-25T00:00:00Z")
  private val stale = 15.minutes
  private def store = new InMemoryChunkScrapeStore()

  "startRun" should "start a run when none is active, then refuse a second while it's fresh (the mutex)" in {
    val s  = store
    val r1 = s.startRun("Kino", Seq("a", "b"), now, stale)
    r1 shouldBe defined
    s.startRun("Kino", Seq("a", "b"), now, stale) shouldBe None
    s.activeRun("Kino").map(_.runId) shouldBe r1
    s.activeRun("Kino").map(_.expectedKeys) shouldBe Some(Seq("a", "b"))
  }

  it should "supersede a STALE active run with a new one" in {
    val s     = store
    val r1    = s.startRun("Kino", Seq("a"), now, stale).get
    val later = now.plusSeconds(16 * 60)
    val r2    = s.startRun("Kino", Seq("a"), later, stale)
    r2 shouldBe defined
    r2 should not be Some(r1)
    s.activeRun("Kino").map(_.runId) shouldBe r2
  }

  "storeChunk" should "persist per active run, ignoring a stale runId" in {
    val s = store
    val r = s.startRun("Kino", Seq("a", "b"), now, stale).get
    s.storeChunk("Kino", r, "a", "VA", now)
    s.storeChunk("Kino", "ghost", "b", "VB", now) // not the active run → ignored
    s.storedKeys("Kino", r) shouldBe Set("a")
    s.loadChunks("Kino", r)  shouldBe Map("a" -> "VA")
    s.storedKeys("Kino", "ghost") shouldBe empty
  }

  "completeRun" should "drop the run and its chunks only for the matching runId" in {
    val s = store
    val r = s.startRun("Kino", Seq("a"), now, stale).get
    s.storeChunk("Kino", r, "a", "VA", now)
    s.completeRun("Kino", "not-this-run")
    s.activeRun("Kino") shouldBe defined
    s.completeRun("Kino", r)
    s.activeRun("Kino") shouldBe None
    s.activeRuns()      shouldBe empty
    s.loadChunks("Kino", r) shouldBe empty
  }

  "activeRuns" should "list every cinema's active run" in {
    val s = store
    s.startRun("A", Seq("x"), now, stale)
    s.startRun("B", Seq("y"), now, stale)
    s.activeRuns().map(_.cinema).toSet shouldBe Set("A", "B")
  }
}
