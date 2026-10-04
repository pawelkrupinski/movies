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
    cut.map(_.taskType).toSet shouldBe CutoverTaskHandlers.OldPathTypes
    cut.foreach(h => h.handle(task(h.taskType)) shouldBe HandlerOutcome.Skipped)
    handlers.map(_.ran).sum shouldBe 0
  }

  "Every other task type" should "keep its own handler" in {
    // ReadVenuePage is the cut-over model's own: it reads a new listing's venue page before the model takes it in.
    val kept = Seq(TaskType.ScrapeCinema, TaskType.EnrichDetails, TaskType.ReadVenuePage, TaskType.ResolveImdbId, TaskType.ImdbRating,
      TaskType.SettleNow).map(new Recording(_))
    CutoverTaskHandlers.of(kept).filterNot(h => CutoverTaskHandlers.OldPathTypes(h.taskType)) shouldBe kept
  }

  // The old path's handlers were DELETED with it, and a queued task with no handler is handed straight
  // back to the queue: PL's one leftover ResolveTmdb was claimed and released 22,570 times overnight
  // (2026-10-04), starving the detail and rating tasks queued behind it.
  "A queued task of an old path type" should "be completed even when no handler for its type is wired any more" in {
    val cut = CutoverTaskHandlers.of(Seq(new Recording(TaskType.EnrichDetails)))
    CutoverTaskHandlers.OldPathTypes.foreach { t =>
      withClue(s"$t: ")(cut.find(_.taskType == t).map(_.handle(task(t))) shouldBe Some(HandlerOutcome.Skipped))
    }
  }
}
