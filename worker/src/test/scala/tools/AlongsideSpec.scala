package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, TimeUnit}

/** Work a harness runs beside its critical path — the recorder's coverage read beside its corpus
 *  read, a convergence leg's identity sweep beside its boot — instead of after it, where each sat on
 *  the US recording's critical path (run 37111868620). */
class AlongsideSpec extends AnyFlatSpec with Matchers {

  "Alongside" should "run the second piece of work while the first is still running" in {
    val secondStarted = new CountDownLatch(1)
    val (first, second) = Alongside {
      // Serially, the second never starts while this one waits for it.
      if (secondStarted.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)) "corpus" else "the second waited for the first"
    } { secondStarted.countDown(); "coverage" }
    (first, second) shouldBe (("corpus", "coverage"))
  }

  it should "hand back the second's failure rather than a result without it" in {
    an[IllegalStateException] should be thrownBy Alongside("corpus")(throw new IllegalStateException("coverage read failed"))
  }

  "Alongside.start" should "run its work while the caller goes on, and hand its result back at the join" in {
    val callerWent = new CountDownLatch(1)
    val sweep      = Alongside.start("sweep")(callerWent.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS))
    callerWent.countDown()
    sweep.join() shouldBe true
  }

  it should "rethrow the work's failure at the join" in {
    val sweep = Alongside.start("sweep")(throw new IllegalStateException("sweep failed"))
    the[IllegalStateException] thrownBy sweep.join() should have message "sweep failed"
  }
}
