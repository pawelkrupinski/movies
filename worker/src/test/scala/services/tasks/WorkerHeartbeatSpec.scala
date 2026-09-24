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
    val line = new WorkerHeartbeat(new FailingReadTaskQueue(new RuntimeException("Mongo down"))).statusLine()
    line should include ("UNKNOWN")
    line should include ("Mongo down")
    line should not include ("waiting=0")
  }
}
