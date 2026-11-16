package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A change-stream observer's `onNext` must not throw: whatever its work throws is logged and goes no further. */
class ChangeStreamCallbackSpec extends AnyFlatSpec with Matchers {

  "ChangeStreamCallback.guarded" should "hand whatever the work throws, a fatal error included, to its logger and not throw" in {
    Seq[Throwable](new RuntimeException("apply failed"), new StackOverflowError(), new LinkageError("stale class")).foreach { thrown =>
      var logged = Option.empty[Throwable]
      noException should be thrownBy ChangeStreamCallback.guarded(t => logged = Some(t))(throw thrown)
      logged.map(_ eq thrown) shouldBe Some(true)
    }
  }

  it should "keep an interrupt's flag for whoever owns the thread" in {
    try {
      ChangeStreamCallback.guarded(_ => ())(throw new InterruptedException("shutting down"))
      Thread.currentThread().isInterrupted shouldBe true
    } finally { val _ = Thread.interrupted() }
  }

  it should "leave a delivery that worked alone" in {
    var logged = false
    var applied = false
    ChangeStreamCallback.guarded(_ => logged = true) { applied = true }
    (applied, logged) shouldBe ((true, false))
  }
}
