package services.events

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class EventBusSpec extends AnyFlatSpec with Matchers {

  "EventBus.publish" should "invoke a subscriber whose PartialFunction matches the event" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[MovieDetailsComplete]
    bus.subscribe { case e: MovieDetailsComplete => seen.append(e) }

    bus.publish(MovieDetailsComplete("Drzewo Magii", Some(2024)))

    seen.toList shouldBe List(MovieDetailsComplete("Drzewo Magii", Some(2024)))
  }

  it should "deliver to every subscriber when multiple are registered" in {
    val bus    = new InProcessEventBus
    val counts = (0 until 3).map(_ => new AtomicInteger(0))
    counts.foreach { c =>
      bus.subscribe { case _: MovieDetailsComplete => c.incrementAndGet(); () }
    }

    bus.publish(MovieDetailsComplete("X", None))

    counts.map(_.get) shouldBe Seq(1, 1, 1)
  }

  // Key contract for the PartialFunction-based API: a subscriber only needs
  // to pattern-match on the cases it cares about. The bus uses applyOrElse,
  // so events that don't match a subscriber's PF are silently skipped — no
  // explicit `case _ => ()` fallback required.
  it should "silently skip events the subscriber's PartialFunction doesn't match (applyOrElse)" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[MovieDetailsComplete]
    // Subscriber only cares about events whose title starts with "Keep:".
    bus.subscribe { case e @ MovieDetailsComplete(t, _, _, _) if t.startsWith("Keep:") => seen.append(e) }

    bus.publish(MovieDetailsComplete("Skip me", None))
    bus.publish(MovieDetailsComplete("Keep: this one", Some(2025)))
    bus.publish(MovieDetailsComplete("Skip me too", None))

    seen.toList shouldBe List(MovieDetailsComplete("Keep: this one", Some(2025)))
  }

  it should "isolate handler exceptions so one bad subscriber can't break the bus" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[String]
    bus.subscribe { case MovieDetailsComplete(t, _, _, _) => throw new RuntimeException(s"boom on $t") }
    bus.subscribe { case MovieDetailsComplete(t, _, _, _) => seen.append(t) }

    bus.publish(MovieDetailsComplete("First", None))
    bus.publish(MovieDetailsComplete("Second", None))

    // Both events reached the second subscriber even though the first one
    // throws on every event.
    seen.toList shouldBe List("First", "Second")
  }

  it should "support PartialFunctions composed with orElse on a single subscription" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[String]
    val handleWithYear: PartialFunction[DomainEvent, Unit] = {
      case MovieDetailsComplete(t, Some(y), _, _) => seen.append(s"with-year:$t/$y")
    }
    val handleNoYear: PartialFunction[DomainEvent, Unit] = {
      case MovieDetailsComplete(t, None, _, _) => seen.append(s"no-year:$t")
    }
    bus.subscribe(handleWithYear orElse handleNoYear)

    bus.publish(MovieDetailsComplete("A", Some(2024)))
    bus.publish(MovieDetailsComplete("B", None))

    seen.toList should contain theSameElementsInOrderAs Seq("with-year:A/2024", "no-year:B")
  }
}
