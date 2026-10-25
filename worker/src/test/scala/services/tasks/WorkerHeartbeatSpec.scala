package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkerHeartbeatSpec extends AnyFlatSpec with Matchers {

  "WorkerHeartbeat" should "seed its liveness pulse at construction time" in {
    val hb = new WorkerHeartbeat(new InMemoryTaskQueue, now = () => 1000L)
    hb.lastTickMillis shouldBe 1000L
  }

  it should "advance the liveness pulse on every beat" in {
    var clock = 1000L
    val hb = new WorkerHeartbeat(new InMemoryTaskQueue, now = () => clock)
    hb.lastTickMillis shouldBe 1000L
    clock = 61_000L
    hb.beat()
    hb.lastTickMillis shouldBe 61_000L
  }

  it should "stamp the pulse even if the queue read fails (the pulse proves the THREAD ran, not Mongo)" in {
    var clock = 1000L
    val hb = new WorkerHeartbeat(new FailingReadTaskQueue(new RuntimeException("Mongo down")), now = () => clock)
    clock = 61_000L
    noException should be thrownBy hb.beat()
    hb.lastTickMillis shouldBe 61_000L       // …the pulse still advanced
  }

  it should "log an unreadable queue as UNKNOWN, never as an empty one" in {
    val line = new WorkerHeartbeat(new FailingReadTaskQueue(new RuntimeException("Mongo down")), now = () => 1000L).statusLine()
    line should include ("UNKNOWN")
    line should include ("Mongo down")
    line should not include ("waiting=0")
  }

  // A beat that could not read the queue logged UNKNOWN and still completed worker-heartbeat's tick, so a queue
  // unreadable for hours never went stale.
  it should "not complete its tick when the queue depth could not be read" in {
    val failing = new FailingReadTaskQueue(new RuntimeException("Mongo down"))
    val ticks   = new tools.RecordingTickRecorder
    def beat(hb: WorkerHeartbeat): Unit =
      tools.ScheduledTick.recorded("worker-heartbeat", scala.concurrent.duration.Duration(60, "seconds"), play.api.Logger(getClass), ticks)(hb.beat())
    beat(new WorkerHeartbeat(failing, now = () => 1000L))
    ticks.completedRuns shouldBe empty
    beat(new WorkerHeartbeat(new InMemoryTaskQueue, now = () => 1000L))
    ticks.completedRuns should have size 1
  }
}
