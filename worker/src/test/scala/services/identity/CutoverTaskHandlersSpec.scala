package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{HandlerOutcome, Task, TaskHandler, TaskType}

/** A cut-over country completes the old identity path's queued tasks unrun, and runs every other. */
class CutoverTaskHandlersSpec extends AnyFlatSpec with Matchers {

  private final class Recording(val taskType: TaskType) extends TaskHandler {
    var ran = 0
    def handle(task: Task): HandlerOutcome = { ran += 1; HandlerOutcome.Done }
  }

  private def task(t: TaskType) = Task("id", t, "key", Map.empty, 0)

  "The old path's task types" should "be completed without running their handlers" in {
    val handlers = CutoverTaskHandlers.OldPathTypes.toSeq.map(new Recording(_))
    val cut      = CutoverTaskHandlers.of(handlers)
    cut.map(_.taskType) shouldBe handlers.map(_.taskType)
    cut.foreach(h => h.handle(task(h.taskType)) shouldBe HandlerOutcome.Skipped)
    handlers.map(_.ran).sum shouldBe 0
  }

  "Every other task type" should "keep its own handler" in {
    val kept = Seq(TaskType.ScrapeCinema, TaskType.ResolveImdbId, TaskType.ImdbRating, TaskType.SettleNow).map(new Recording(_))
    CutoverTaskHandlers.of(kept) shouldBe kept
  }
}
