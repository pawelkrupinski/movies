package tools

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.{ILoggingEvent, LoggingEvent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

  // A PRIVATE logback context, not `LoggerFactory.getLogger`: while SLF4J is still initialising
  // (another suite in the same JVM triggering it concurrently) the factory hands out
  // `SubstituteLogger`s, and casting one to logback's Logger threw ClassCastException on CI
  // (2026-09-24). The collector under test never looks at the event's logger, so it need not
  // come from the global factory.
  private val context = new ch.qos.logback.classic.LoggerContext()

  private def event(message: String): ILoggingEvent =
    new LoggingEvent(
      classOf[LogCaptureSpec].getName,
      context.getLogger(Logger),
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
      noisy.join(SpecTimeouts.Io.toMillis)
      collector.stop()
    }

    Option(failure.get()).foreach(t => fail(s"reading the collector threw ${t.getClass.getName}", t))
  }

  "a capture" should "keep only this thread's events when asked, so a parallel suite cannot break an exact-match assertion" in {
    // Logback's own logger, not the factory's: while SLF4J is still initialising (a suite next door
    // starting it) the factory hands out a SubstituteLogger, which QUEUES "mine" for replay after
    // initialisation instead of reaching the capture's appender now (Main run 36812183340: List() vs "mine").
    val logger  = LogCapture.logbackLogger(Logger)
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

  // IdentityProjectionSpec on github.com (Main 41207214765): `List() was not equal to List("periodic")` -- a projection's
  // lines, logged on the test's own thread, missed by a capture attached while parallel suites logged to the same class
  // logger. Each capture attached its own appender, and logback's COWArrayList.refreshCopy, racing that attach on a
  // logging thread, can publish the appender list as it was before the add: every event until the next attach or detach
  // then skips the new appender. A logger's appender list is now changed once, by the capture hub's attach, settled
  // until the logging path sees it; captures register with the hub.
  it should "see every line of its own thread while other threads log to the same logger and capture it" in {
    val name   = "kinowo.log-capture-spec.attach-race"
    val logger = LogCapture.logbackLogger(name)
    logger.setAdditive(false) // the hammering stays off the console
    logger.setLevel(Level.INFO)
    val running = new AtomicBoolean(true)
    // logging AND capturing, as the suites next door do: each capture of theirs changed the appender list too
    val noisy = (1 to 3).map { n =>
      val t = new Thread(() => while (running.get()) LogCapture.capture(name)((1 to 20).foreach(_ => logger.info(s"theirs $n"))))
      t.setDaemon(true); t.start(); t
    }
    try {
      val missed = (1 to 2000).count(_ => !LogCapture.capture(name)(logger.info("mine")).exists(_.getFormattedMessage == "mine"))
      withClue("captures that missed their own thread's line: ")(missed shouldBe 0)
    } finally {
      running.set(false)
      noisy.foreach(_.join(SpecTimeouts.Io.toMillis))
      logger.setLevel(null)
      logger.setAdditive(true)
    }
  }

  it should "fix each line's thread and MDC as it is logged, with no other appender to do it" in {
    // An event reads both lazily, on whichever thread first asks. A console appender fixes them as it writes; a logger
    // without one (additivity off) left `thisThread` and a `within` block's fields to the reader's thread and MDC.
    val name   = "kinowo.log-capture-spec.deferred"
    val logger = LogCapture.logbackLogger(name)
    logger.setAdditive(false)
    logger.setLevel(Level.INFO)
    try {
      val events = LogCapture.capture(name) {
        org.slf4j.MDC.put("log_capture_spec", "set")
        try logger.info("mine") finally org.slf4j.MDC.remove("log_capture_spec")
        val other = new Thread(() => logger.info("theirs"), "log-capture-spec-other")
        other.start(); other.join()
      }
      events.map(e => e.getFormattedMessage -> e.getThreadName) shouldBe
        Seq("mine" -> Thread.currentThread().getName, "theirs" -> "log-capture-spec-other")
      events.head.getMDCPropertyMap.get("log_capture_spec") shouldBe "set"
    } finally { logger.setLevel(null); logger.setAdditive(true) }
  }

  it should "leave the logger's level alone unless asked to change it" in {
    // The ROOT-appender specs assert that the CONFIGURED level lets an audit line
    // through; a capture that forced DEBUG would answer their question for them.
    val logger = LogCapture.logbackLogger(Logger)
    logger.setLevel(Level.WARN)
    try {
      LogCapture.capture(Logger)(logger.info("suppressed")) shouldBe empty
      logger.getLevel shouldBe Level.WARN
      LogCapture.capture(Logger, Some(Level.DEBUG))(logger.info("let through")) should have size 1
      withClue("the forced level must be restored afterwards: ")(logger.getLevel shouldBe Level.WARN)
    } finally logger.setLevel(null)
  }

  // The race itself (a reset detaching the hub mid-capture) cannot be driven here without resetting the JVM-global
  // context under every other suite; what is proved is the exclusion that prevents it.
  it should "hold back a context reset until the captures in flight have finished" in {
    val entered   = new java.util.concurrent.CountDownLatch(1)
    val attempted = new java.util.concurrent.CountDownLatch(1)
    val resetter  = new Thread(() => { attempted.countDown(); LogCapture.exclusively(entered.countDown()) }, "log-capture-spec-resetter")
    LogCapture.capture(Logger) {
      resetter.start()
      attempted.await()
      withClue("an exclusive section must not start while a capture runs: ")(entered.await(SpecTimeouts.quiet(scala.concurrent.duration.DurationInt(150).millis).toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe false)
    }
    withClue("and must start once the capture is done: ")(entered.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true)
    resetter.join()
  }
}
