package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The two shapes that catch fatal errors too: what they catch, and that neither swallows a throwable it should not. */
class AnyThrowableSpec extends AnyFlatSpec with Matchers {

  private val fatal: Seq[Throwable] =
    Seq(new StackOverflowError(), new LinkageError("stale class"), new InterruptedException("stopping"), new RuntimeException("plain"))

  "rethrownAfter" should "show its clean-up anything the block throws, fatal or not, then rethrow that very throwable" in {
    fatal.foreach { thrown =>
      var seen = Option.empty[Throwable]
      val rethrown = the[Throwable] thrownBy AnyThrowable.rethrownAfter[Int](throw thrown)(t => seen = Some(t))
      rethrown should be theSameInstanceAs thrown
      seen.map(_ eq thrown) shouldBe Some(true)
    }
  }

  it should "answer the block's value, its clean-up untouched" in {
    var cleaned = false
    AnyThrowable.rethrownAfter(42)(_ => cleaned = true) shouldBe 42
    cleaned shouldBe false
  }

  "captured" should "answer whatever the block threw, fatal or not, or its value" in {
    try fatal.foreach(thrown => AnyThrowable.captured[Int](throw thrown).left.map(_ eq thrown) shouldBe Left(true))
    finally { Thread.interrupted(); () }   // the captured interrupt's flag, kept (below), is not the next test's
    AnyThrowable.captured("value") shouldBe Right("value")
  }

  // The worker's shutdown drain captured an interrupt and dropped it: the steps after it, and the thread's owner, no
  // longer knew they had been asked to stop. ChangeStreamCallback restored it by hand; every capture does now.
  it should "keep an interrupt's flag on the thread that captured it" in {
    try {
      AnyThrowable.captured[Int](throw new InterruptedException("stopping")).isLeft shouldBe true
      Thread.currentThread().isInterrupted shouldBe true
    } finally { Thread.interrupted(); () }
    AnyThrowable.captured[Int](throw new RuntimeException("plain")).isLeft shouldBe true
    Thread.currentThread().isInterrupted shouldBe false
  }
}
