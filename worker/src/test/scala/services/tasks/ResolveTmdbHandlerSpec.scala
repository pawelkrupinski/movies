package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class ResolveTmdbHandlerSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  private type ResolveCall = (String, Option[Int], Option[String], Option[String], Boolean)

  /** A handler whose resolve records its args and returns `concluded` (true =
   *  definitive → Done; false = transient failure → Reschedule). */
  private def handlerReturning(concluded: Boolean, into: ListBuffer[ResolveCall]) =
    new ResolveTmdbHandler((title, year, orig, dir, force) => {
      into += ((title, year, orig, dir, force)); concluded
    })

  "ResolveTmdbHandler" should "resolve with the task's title/year/hints/force and return Done when concluded" in {
    val calls = ListBuffer.empty[ResolveCall]
    val h     = handlerReturning(concluded = true, calls)
    val task  = Task("id", TaskType.ResolveTmdb,
      EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2024)),
      EnrichTaskKeys.resolveTmdbPayload("Dune", Some(2024),
        director = Some("Denis Villeneuve"), originalTitle = Some("Dune: Part Two"), force = true),
      attempts = 1)

    h.handle(task) shouldBe Done
    calls.toList shouldBe List(("Dune", Some(2024), Some("Dune: Part Two"), Some("Denis Villeneuve"), true))
  }

  it should "Reschedule (transient failure) when the resolve does not conclude" in {
    val calls = ListBuffer.empty[ResolveCall]
    val h     = handlerReturning(concluded = false, calls)
    val task  = Task("id", TaskType.ResolveTmdb,
      EnrichTaskKeys.resolveTmdbDedup("Untitled", None),
      EnrichTaskKeys.resolveTmdbPayload("Untitled", None), attempts = 2)

    h.handle(task) shouldBe a[Reschedule]
    calls.toList shouldBe List(("Untitled", None, None, None, false))
  }

  it should "drop (Done, no resolve) a task with no title payload" in {
    val calls = ListBuffer.empty[ResolveCall]
    val h     = handlerReturning(concluded = true, calls)
    val task  = Task("id", TaskType.ResolveTmdb, "resolve-tmdb|", Map.empty, attempts = 1)

    h.handle(task) shouldBe Done
    calls shouldBe empty
  }
}
