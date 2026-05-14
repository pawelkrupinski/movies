package services.events

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class EventBusSpec extends AnyFlatSpec with Matchers {

  "EventBus.publish" should "invoke a subscriber whose PartialFunction matches the event" in {
    val bus  = new EventBus
    val seen = mutable.ListBuffer.empty[MovieAdded]
    bus.subscribe { case e: MovieAdded => seen.append(e) }

    bus.publish(MovieAdded("Drzewo Magii", Some(2024)))

    seen.toList shouldBe List(MovieAdded("Drzewo Magii", Some(2024)))
  }

  it should "deliver to every subscriber when multiple are registered" in {
    val bus    = new EventBus
    val counts = (0 until 3).map(_ => new AtomicInteger(0))
    counts.foreach { c =>
      bus.subscribe { case _: MovieAdded => c.incrementAndGet(); () }
    }

    bus.publish(MovieAdded("X", None))

    counts.map(_.get) shouldBe Seq(1, 1, 1)
  }

  // Key contract for the PartialFunction-based API: a subscriber only needs
  // to pattern-match on the cases it cares about. The bus uses applyOrElse,
  // so events that don't match a subscriber's PF are silently skipped — no
  // explicit `case _ => ()` fallback required.
  it should "silently skip events the subscriber's PartialFunction doesn't match (applyOrElse)" in {
    val bus  = new EventBus
    val seen = mutable.ListBuffer.empty[MovieAdded]
    // Subscriber only cares about events whose title starts with "Keep:".
    bus.subscribe { case e @ MovieAdded(t, _, _) if t.startsWith("Keep:") => seen.append(e) }

    bus.publish(MovieAdded("Skip me", None))
    bus.publish(MovieAdded("Keep: this one", Some(2025)))
    bus.publish(MovieAdded("Skip me too", None))

    seen.toList shouldBe List(MovieAdded("Keep: this one", Some(2025)))
  }

  it should "isolate handler exceptions so one bad subscriber can't break the bus" in {
    val bus  = new EventBus
    val seen = mutable.ListBuffer.empty[String]
    bus.subscribe { case MovieAdded(t, _, _) => throw new RuntimeException(s"boom on $t") }
    bus.subscribe { case MovieAdded(t, _, _) => seen.append(t) }

    bus.publish(MovieAdded("First", None))
    bus.publish(MovieAdded("Second", None))

    // Both events reached the second subscriber even though the first one
    // throws on every event.
    seen.toList shouldBe List("First", "Second")
  }

  it should "support PartialFunctions composed with orElse on a single subscription" in {
    val bus  = new EventBus
    val seen = mutable.ListBuffer.empty[String]
    val handleWithYear: PartialFunction[DomainEvent, Unit] = {
      case MovieAdded(t, Some(y), _) => seen.append(s"with-year:$t/$y")
    }
    val handleNoYear: PartialFunction[DomainEvent, Unit] = {
      case MovieAdded(t, None, _) => seen.append(s"no-year:$t")
    }
    bus.subscribe(handleWithYear orElse handleNoYear)

    bus.publish(MovieAdded("A", Some(2024)))
    bus.publish(MovieAdded("B", None))

    seen.toList should contain theSameElementsInOrderAs Seq("with-year:A/2024", "no-year:B")
  }
}
