package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class ResolveTmdbHandlerSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  private def handlerRecording(into: ListBuffer[(String, Option[Int])]) =
    new ResolveTmdbHandler((title, year) => { into += ((title, year)); () })

  "ResolveTmdbHandler" should "force a re-resolve with the task's title + year and return Done" in {
    val calls = ListBuffer.empty[(String, Option[Int])]
    val h     = handlerRecording(calls)
    val task  = Task("id", TaskType.ResolveTmdb,
      EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2024)),
      EnrichTaskKeys.moviePayload("Dune", Some(2024)), attempts = 1)

    h.handle(task) shouldBe Done
    calls.toList shouldBe List(("Dune", Some(2024)))
  }

  it should "pass through a yearless film" in {
    val calls = ListBuffer.empty[(String, Option[Int])]
    val h     = handlerRecording(calls)
    val task  = Task("id", TaskType.ResolveTmdb,
      EnrichTaskKeys.resolveTmdbDedup("Untitled", None),
      EnrichTaskKeys.moviePayload("Untitled", None), attempts = 1)

    h.handle(task) shouldBe Done
    calls.toList shouldBe List(("Untitled", None))
  }

  it should "drop (Done, no re-resolve) a task with no title payload" in {
    val calls = ListBuffer.empty[(String, Option[Int])]
    val h     = handlerRecording(calls)
    val task  = Task("id", TaskType.ResolveTmdb, "resolve-tmdb|", Map.empty, attempts = 1)

    h.handle(task) shouldBe Done
    calls shouldBe empty
  }
}
