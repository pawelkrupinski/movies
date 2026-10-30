package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.SpecTimeouts

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class SharedSpec extends AnyFlatSpec with Matchers {

  private final class Counted(linger: FiniteDuration, failMake: Boolean = false) {
    val made    = new AtomicInteger
    val stopped = new AtomicInteger
    val stop    = new CountDownLatch(1)
    val shared  = new Shared[Int](
      () => { if (failMake) throw new IllegalStateException("boot failed"); made.incrementAndGet() },
      _ => { stopped.incrementAndGet(); stop.countDown() },
      linger)
  }

  "a shared value" should "be made once for every holder at once" in {
    val c = new Counted(linger = 1.hour)
    c.shared.acquire() shouldBe 1
    c.shared.acquire() shouldBe 1
    c.made.get shouldBe 1
  }

  it should "stay made while any holder holds it" in {
    val c = new Counted(linger = 1.millis)
    c.shared.acquire(); c.shared.acquire()
    c.shared.release()
    c.stop.await(SpecTimeouts.quiet(200.millis).toMillis, TimeUnit.MILLISECONDS) shouldBe false
    c.stopped.get shouldBe 0
    c.shared.live shouldBe true
  }

  it should "be stopped once the last holder has released it and the linger has passed" in {
    val c = new Counted(linger = 1.millis)
    c.shared.acquire()
    c.shared.release()
    c.stop.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
    c.stopped.get shouldBe 1
    c.shared.live shouldBe false
  }

  it should "be reused, not made again, by a holder arriving within the linger" in {
    val c = new Counted(linger = 1.hour)
    c.shared.acquire()
    c.shared.release()
    c.shared.acquire() shouldBe 1
    c.made.get shouldBe 1
    c.stopped.get shouldBe 0
  }

  it should "be made again by a holder arriving after it was stopped" in {
    val c = new Counted(linger = 1.millis)
    c.shared.acquire()
    c.shared.release()
    c.stop.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
    c.shared.acquire() shouldBe 2
  }

  it should "hold nothing when making it failed" in {
    val c = new Counted(linger = 1.millis, failMake = true)
    an[IllegalStateException] should be thrownBy c.shared.acquire()
    c.shared.live shouldBe false
  }
}
