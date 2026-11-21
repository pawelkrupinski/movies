package services.movies

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The production reopen driver's own scheduler thread. Every side-collection watch builds a driver of its own, and
 *  `close()` only stopped further reopens: a watch that had ever scheduled one left its daemon thread parked for the
 *  life of the process. */
class ChangeStreamReopenThreadSpec extends AnyFlatSpec with Matchers {

  private def reopenThreads(name: String): Seq[Thread] =
    Thread.getAllStackTraces.keySet.asScala.toSeq.filter(thread => thread.getName == s"$name-reopen" && thread.isAlive)

  "ChangeStreamReopen.onDaemonScheduler" should "end its scheduler thread once the driver is closed" in {
    val name   = s"reopen-thread-probe-${java.util.UUID.randomUUID()}"
    val reopen = ChangeStreamReopen.onDaemonScheduler(name, () => ())
    reopen.failed()                              // schedules a reopen, which starts the scheduler's thread
    reopenThreads(name) should not be empty
    reopen.close()
    _root_.tools.Eventually.eventually(reopenThreads(name) shouldBe empty, timeoutMs = _root_.tools.SpecTimeouts.Io.toMillis)
  }

  it should "start no thread for a driver that never scheduled a reopen" in {
    val name = s"reopen-thread-probe-${java.util.UUID.randomUUID()}"
    ChangeStreamReopen.onDaemonScheduler(name, () => ()).close()
    reopenThreads(name) shouldBe empty
  }
}
