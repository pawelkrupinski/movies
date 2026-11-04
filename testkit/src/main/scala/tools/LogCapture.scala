package tools

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger as LogbackLogger}
import ch.qos.logback.core.AppenderBase
import org.slf4j.LoggerFactory

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.jdk.CollectionConverters.*

/**
 * Collects the log events a block of test code emits, safely under a parallel suite run.
 *
 * The obvious way to do this is logback's own `ListAppender`, and five specs did — but
 * its `list` is a bare `ArrayList` appended to WITHOUT synchronization, and every logger
 * a test attaches to is process-global. ScalaTest runs suites in parallel, so while one
 * spec captures `kinowo.removal-audit` (or, worse, ROOT), other suites' threads are
 * appending to the very list it is copying: `app.list.asScala.toSeq` walks an
 * `ArrayList` iterator another thread has just grown, and throws
 * `ConcurrentModificationException`. `MovieCacheSpec` failed exactly that way on
 * 2026-09-02, inside a test about degraded cinema scrapes that has nothing to do with
 * logging — which is what makes the failure expensive to read.
 *
 * A `ConcurrentLinkedQueue` fixes it at the root: its iterator is weakly consistent, so
 * a snapshot taken while other threads append is merely a snapshot, never an exception.
 *
 * THE ATTACH RACE, logback's own: attaching an appender to a logger another thread is
 * logging to can lose events. `COWArrayList.refreshCopy`, run by the logging thread, may
 * publish an array snapshot taken just before the `add`, and every event until the next
 * attach or detach then goes to the old array — so a capture came back empty although the
 * line was logged (roughly one attach in twenty against a thread logging in a tight loop;
 * IdentityProjectionSpec on github.com, Main 41207214765, a class logger other suites log to
 * and capture). So a capture never attaches anything: each logger gets ONE [[Hub]], attached
 * the first time it is captured and SETTLED there ([[settle]]: until the array the logging
 * path reads holds it, steadily), and captures register with the hub. A later attach or
 * detach by anyone else cannot drop it: a stale snapshot is at worst one taken after the
 * hub's own `add`.
 *
 * THE RESET RACE: a spec that resets or shuts down the JVM-global logback context (a Play `Server.stop`, a
 * `LoggerContext.reset()`) detaches every hub and appender and drops the events logged meanwhile, so a capture
 * running on another suite's thread came back empty although its line was logged
 * (`MongoUserRepositoriesUnreachableSpec`, "List() was empty", beside `RequestHeaderSizeSpec`). Captures hold the
 * read side of [[contextLock]] for their whole run; such a spec wraps itself in [[exclusively]] and so waits for
 * the captures in flight, and holds new ones back until it has put the configuration back.
 *
 * Lives in testkit so worker, web and e2e share one copy. `common`'s own tests cannot
 * reach it — testkit depends on `common`, and the reverse would be a project cycle — so
 * `RemovalAuditSpec` carries the same appender inline; keep the two in step.
 */
object LogCapture {

  /**
   * Runs `body` with a collecting appender on `loggerName`, returning everything that
   * landed on it.
   *
   * `level` is an OPTION, and the default of `None` means "leave the logger's level
   * alone" — the specs that assert an audit line reaches the ROOT appenders are
   * asserting the configured level lets it through, and forcing DEBUG for the capture
   * would answer their question for them. Pass `Some(Level.DEBUG)` when the point is to
   * see lines the configuration would otherwise suppress.
   *
   * Events from OTHER threads are included, which is the honest answer for a global
   * logger; a caller that must pin an exact list uses [[thisThread]] instead.
   */
  /** SLF4J answers a `getLogger` made while ANOTHER thread is mid-initialisation with a
   *  `SubstituteLogger` placeholder, and ScalaTest starts suites in parallel -- so the
   *  first spec to log after the JVM comes up can be handed the placeholder instead of
   *  logback's logger, and a bare cast threw `ClassCastException` under
   *  `common/testOnly services.movies.*` on 2026-09-06. Once initialisation completes
   *  `getLogger` returns the real logger, so this waits for it; when it is already
   *  done (every call but the first), the first probe already holds. */
  def logbackLogger(name: String): LogbackLogger = {
    Eventually.poll(pollMs = 10)(LoggerFactory.getLogger(name).isInstanceOf[LogbackLogger])
    LoggerFactory.getLogger(name).asInstanceOf[LogbackLogger]
  }

  private val contextLock = new java.util.concurrent.locks.ReentrantReadWriteLock()

  /** Runs `body`, which resets or stops the global logging context, with no capture running and none starting. */
  def exclusively[A](body: => A): A = {
    val write = contextLock.writeLock()
    write.lock()
    try body finally write.unlock()
  }

  def capture(loggerName: String, level: Option[Level] = None)(body: => Unit): Seq[ILoggingEvent] = {
    val read = contextLock.readLock()
    read.lock()
    try captureLocked(loggerName, level)(body) finally read.unlock()
  }

  private def captureLocked(loggerName: String, level: Option[Level])(body: => Unit): Seq[ILoggingEvent] = {
    val logger    = logbackLogger(loggerName)
    val hub       = hubOf(logger)
    val collector = new CollectingAppender
    collector.setContext(logger.getLoggerContext)
    collector.start()
    val previous = logger.getLevel
    level.foreach(logger.setLevel)
    hub.collectors.add(collector)
    try {
      body
      collector.events
    } finally {
      hub.collectors.remove(collector)
      collector.stop()
      level.foreach(_ => logger.setLevel(previous))
    }
  }

  /** The one appender a captured logger carries for good, handing each event to the captures running on it. An event
   *  reads its thread's name and MDC LAZILY, on whatever thread first asks: they are fixed here, on the logging thread
   *  (as a console appender does), or [[thisThread]] and a `within` block's fields would read the reader's own. */
  private final class Hub extends AppenderBase[ILoggingEvent] {
    val collectors: java.util.Set[CollectingAppender] = ConcurrentHashMap.newKeySet[CollectingAppender]()
    override def append(event: ILoggingEvent): Unit =
      if (!collectors.isEmpty) { event.prepareForDeferredProcessing(); collectors.forEach(_.collect(event)) }
  }

  private val hubs = new ConcurrentHashMap[LogbackLogger, Hub]()

  private def hubOf(logger: LogbackLogger): Hub = {
    // a hub its context detached (a reset) is replaced: a capture never waits on one no logging path can reach
    val hub = hubs.compute(logger, (l, old) =>
      if (old != null && old.isStarted && l.isAttached(old)) old
      else {
        val h = new Hub
        h.setName("log-capture-hub")
        h.setContext(l.getLoggerContext)
        h.start()
        l.addAppender(h)
        if (old != null) settled.remove(old)
        h
      })
    settle(logger, hub)
    hub
  }

  /** Until the appender array `logger`'s logging path reads holds `hub`, on three looks in a row a few ms apart ([[Eventually]]'s polls): a
   *  `refreshCopy` racing the hub's `add` publishes its old array AFTER the add, so one look is not enough. A look that
   *  misses it marks the array stale, as an attach would, so the next read takes it afresh. Once settled, one look per
   *  capture. Read through logback's private fields; were they ever renamed, every capture fails loudly here. */
  private def settle(logger: LogbackLogger, hub: Hub): Unit = {
    def visible = appenderArray(logger).contains(hub)
    if (!(settled.contains(hub) && visible)) {
      var steady = 0
      val held = Eventually.poll(pollMs = 2) {
        if (visible) steady += 1 else { steady = 0; staleFlag(logger).set(false) }
        steady >= 3
      }
      if (!held) throw new IllegalStateException(s"LogCapture: the capture hub on ${logger.getName} never held on its logging path")
      settled.add(hub)
      ()
    }
  }
  private val settled = ConcurrentHashMap.newKeySet[Hub]()

  private def appenderList(logger: LogbackLogger): AnyRef = {
    val aai = classOf[LogbackLogger].getDeclaredField("aai")
    aai.setAccessible(true)
    val impl = aai.get(logger)
    val list = classOf[ch.qos.logback.core.spi.AppenderAttachableImpl[?]].getDeclaredField("appenderList")
    list.setAccessible(true)
    list.get(impl)
  }
  /** The array `appendLoopOnAppenders` reads: refreshed first when stale, as it is. */
  private def appenderArray(logger: LogbackLogger): Seq[AnyRef] =
    appenderList(logger).asInstanceOf[ch.qos.logback.core.util.COWArrayList[AnyRef]].asTypedArray().toSeq
  private def staleFlag(logger: LogbackLogger): java.util.concurrent.atomic.AtomicBoolean = {
    val fresh = classOf[ch.qos.logback.core.util.COWArrayList[?]].getDeclaredField("fresh")
    fresh.setAccessible(true)
    fresh.get(appenderList(logger)).asInstanceOf[java.util.concurrent.atomic.AtomicBoolean]
  }

  /**
   * The subset of [[capture]] this thread emitted.
   *
   * For the specs that pin an EXACT event list: `kinowo.removal-audit` is a
   * fixed-name, process-global logger and `MovieCacheSpec` and
   * friends all perform real deletes in parallel, so one foreign line fails an
   * exact-match assertion. Every call under test is synchronous on the test thread,
   * which makes this filter both precise and complete.
   */
  def thisThread(loggerName: String, level: Option[Level] = None)(body: => Unit): Seq[ILoggingEvent] = {
    val thread = Thread.currentThread().getName
    capture(loggerName, level)(body).filter(_.getThreadName == thread)
  }

  /** Appends into a queue whose iterator cannot throw, unlike `ListAppender`'s `ArrayList`. */
  final class CollectingAppender extends AppenderBase[ILoggingEvent] {
    private val queue = new ConcurrentLinkedQueue[ILoggingEvent]()
    override def append(event: ILoggingEvent): Unit = { queue.add(event); () }
    private[tools] def collect(event: ILoggingEvent): Unit = if (isStarted) { queue.add(event); () }
    def events: Seq[ILoggingEvent] = queue.iterator().asScala.toSeq
  }
}
