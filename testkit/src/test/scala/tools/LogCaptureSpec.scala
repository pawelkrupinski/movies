package tools

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.{ILoggingEvent, LoggingEvent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/**
 * The reason this helper exists: reading what a capture collected must survive OTHER
 * threads appending to it at the same time.
 *
 * That is not hypothetical — ScalaTest runs suites in parallel and the loggers these
 * specs attach to are process-global, so a foreign thread appending mid-read is the
 * normal case. Against logback's `ListAppender`, whose `list` is a bare `ArrayList`,
 * the first test below throws `ConcurrentModificationException` within a few passes;
 * that is how `MovieCacheSpec` failed CI on 2026-09-02.
 *
 * The race is driven through the APPENDER rather than through a logger, deliberately.
 * Attaching an appender to a logger that another thread is hammering is unreliable in
 * logback itself — `COWArrayList.refreshCopy` can publish an array snapshot taken
 * before the `add`, leaving the new appender invisible to every subsequent event — so a
 * logger-level race would be testing logback's attach path, not this collector.
 */
class LogCaptureSpec extends AnyFlatSpec with Matchers {

  private val Logger = "kinowo.log-capture-spec"

  private def event(message: String): ILoggingEvent =
    new LoggingEvent(
      classOf[LogCaptureSpec].getName,
      LoggerFactory.getLogger(Logger).asInstanceOf[ch.qos.logback.classic.Logger],
      Level.INFO,
      message,
      null,
      null)

  "a collector" should "be readable while another thread is still appending to it" in {
    val collector = new LogCapture.CollectingAppender
    collector.start()
    collector.doAppend(event("seed"))

    // Bounded on both sides: the reader copies the whole queue per pass, so an
    // unbounded writer turns this into an out-of-memory test rather than a race one.
    val Reads   = 200
    val Appends = 20000
    val foreign = event("from another suite")
    val running = new AtomicBoolean(true)
    val failure = new AtomicReference[Throwable]()
    val noisy = new Thread(() => {
      var written = 0
      while (running.get() && written < Appends) { collector.doAppend(foreign); written += 1 }
    })
    noisy.setDaemon(true)
    noisy.start()
    try
      // A bare ArrayList throws ConcurrentModificationException within a handful of
      // passes; a weakly-consistent iterator simply returns a snapshot every time.
      (1 to Reads).foreach { _ =>
        try collector.events should not be empty
        catch { case t: Throwable => failure.compareAndSet(null, t) }
      }
    finally {
      running.set(false)
      noisy.join(5000)
      collector.stop()
    }

    Option(failure.get()).foreach(t => fail(s"reading the collector threw ${t.getClass.getName}", t))
  }

  "a capture" should "keep only this thread's events when asked, so a parallel suite cannot break an exact-match assertion" in {
    val logger  = LoggerFactory.getLogger(Logger)
    val emitted = new AtomicBoolean(false)

    val events = LogCapture.thisThread(Logger, Some(Level.DEBUG)) {
      logger.info("mine")
      val other = new Thread(() => { logger.info("theirs"); emitted.set(true) })
      other.start()
      other.join()
    }

    withClue("the foreign thread must actually have logged for this to prove anything: ")(
      emitted.get() shouldBe true)
    events.map(_.getFormattedMessage) shouldBe Seq("mine")
  }

  it should "leave the logger's level alone unless asked to change it" in {
    // The ROOT-appender specs assert that the CONFIGURED level lets an audit line
    // through; a capture that forced DEBUG would answer their question for them.
    val logger = LoggerFactory.getLogger(Logger).asInstanceOf[ch.qos.logback.classic.Logger]
    logger.setLevel(Level.WARN)
    try {
      LogCapture.capture(Logger)(logger.info("suppressed")) shouldBe empty
      logger.getLevel shouldBe Level.WARN
      LogCapture.capture(Logger, Some(Level.DEBUG))(logger.info("let through")) should have size 1
      withClue("the forced level must be restored afterwards: ")(logger.getLevel shouldBe Level.WARN)
    } finally logger.setLevel(null)
  }
}
