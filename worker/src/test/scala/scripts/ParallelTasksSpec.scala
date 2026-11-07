package scripts

import tools.SpecPool
import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimits}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar._

import java.util.concurrent.ExecutionException
import scala.concurrent.ExecutionContext

class ParallelTasksSpec extends AnyFlatSpec with Matchers with TimeLimits {

  // interrupts the waiting test thread at the limit: without it failAfter only judges a body that returns
  private given Signaler = ThreadSignaler

  private def onPool[A](body: ExecutionContext ?=> A): A = {
    SpecPool.usingContext(2) { ec =>
      body(using ec)
    }
  }

  "All tasks" should "return every result in the items' order" in {
    onPool(ParallelTasks.all(Seq(3, 1, 2))(_ * 10)) shouldBe Seq(30, 10, 20)
  }

  // 2026-10-19: a stale fixture class on the classpath threw NoClassDefFoundError inside a refit task; a plain
  // Future never completes on a fatal error, so the unit run sat idle behind Await.result(..., Duration.Inf) for 50 min.
  they should "fail, not hang, when a task throws a fatal error" in {
    val thrown = failAfter(30.seconds) {
      the[ExecutionException] thrownBy onPool(ParallelTasks.all(Seq(1, 2))(i => if (i == 2) throw new NoClassDefFoundError("gone") else i))
    }
    thrown.getCause shouldBe a[NoClassDefFoundError]
  }

  they should "rethrow a task's ordinary exception as itself" in {
    the[IllegalStateException] thrownBy onPool(ParallelTasks.all(Seq(1))(_ => throw new IllegalStateException("bad"))) should
      have message "bad"
  }
}
