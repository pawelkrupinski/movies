package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Stopwatch

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.*

class BootStepsSpec extends AnyFlatSpec with Matchers {

  /** A clock the spec advances by hand. */
  private final class Ticks { @volatile var now = 0L; def advance(by: FiniteDuration): Unit = now += by.toNanos }

  "a boot" should "time each step and name the slowest first in its summary" in {
    val ticks = new Ticks
    val boot  = new BootSteps("us", new Stopwatch(() => ticks.now))
    boot.step("mongo")(ticks.advance(1.second))
    boot.step("movie cache hydrate")(ticks.advance(24.seconds))
    boot.step("task worker")(ticks.advance(500.millis))
    boot.summary shouldBe "[us] boot steps took 25.5s: movie cache hydrate 24.0s, mongo 1.0s, task worker 0.5s"
  }

  it should "run a background step without holding the boot up" in {
    val boot     = new BootSteps("us")
    val release  = new CountDownLatch(1)
    val finished = new CountDownLatch(1)
    val thread   = boot.inBackground("read-model projector") { release.await(); finished.countDown() }
    // The boot moved on while the step is still waiting.
    boot.step("movie cache hydrate")(())
    finished.getCount shouldBe 1L
    thread.isDaemon shouldBe true
    release.countDown()
    finished.await(5, TimeUnit.SECONDS) shouldBe true
  }

  it should "keep a failing background step from taking the boot down" in {
    val boot   = new BootSteps("us")
    val thread = boot.inBackground("read-model projector")(throw new IllegalStateException("seed failed"))
    thread.join(5000)
    thread.isAlive shouldBe false
    boot.step("task worker")(42) shouldBe 42
  }
}
