package services.events

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class EventBusSpec extends AnyFlatSpec with Matchers {

  "EventBus.publish" should "invoke a subscriber whose PartialFunction matches the event" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[ImdbIdMissing]
    bus.subscribe { case e: ImdbIdMissing => seen.append(e) }

    bus.publish(ImdbIdMissing("Drzewo Magii", Some(2024), "Drzewo Magii"))

    seen.toList shouldBe List(ImdbIdMissing("Drzewo Magii", Some(2024), "Drzewo Magii"))
  }

  it should "deliver to every subscriber when multiple are registered" in {
    val bus    = new InProcessEventBus
    val counts = (0 until 3).map(_ => new AtomicInteger(0))
    counts.foreach { c =>
      bus.subscribe { case _: ImdbIdMissing => c.incrementAndGet(); () }
    }

    bus.publish(ImdbIdMissing("X", None, "X"))

    counts.map(_.get) shouldBe Seq(1, 1, 1)
  }

  // Key contract for the PartialFunction-based API: a subscriber only needs
  // to pattern-match on the cases it cares about. The bus uses applyOrElse,
  // so events that don't match a subscriber's PF are silently skipped — no
  // explicit `case _ => ()` fallback required.
  it should "silently skip events the subscriber's PartialFunction doesn't match (applyOrElse)" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[ImdbIdMissing]
    // Subscriber only cares about events whose title starts with "Keep:".
    bus.subscribe { case e @ ImdbIdMissing(t, _, _) if t.startsWith("Keep:") => seen.append(e) }

    bus.publish(ImdbIdMissing("Skip me", None, "Skip me"))
    bus.publish(ImdbIdMissing("Keep: this one", Some(2025), "Keep: this one"))
    bus.publish(ImdbIdMissing("Skip me too", None, "Skip me too"))

    seen.toList shouldBe List(ImdbIdMissing("Keep: this one", Some(2025), "Keep: this one"))
  }

  it should "isolate handler exceptions so one bad subscriber can't break the bus" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[String]
    bus.subscribe { case ImdbIdMissing(t, _, _) => throw new RuntimeException(s"boom on $t") }
    bus.subscribe { case ImdbIdMissing(t, _, _) => seen.append(t) }

    bus.publish(ImdbIdMissing("First", None, "First"))
    bus.publish(ImdbIdMissing("Second", None, "Second"))

    // Both events reached the second subscriber even though the first one
    // throws on every event.
    seen.toList shouldBe List("First", "Second")
  }

  it should "support PartialFunctions composed with orElse on a single subscription" in {
    val bus  = new InProcessEventBus
    val seen = mutable.ListBuffer.empty[String]
    val handleWithYear: PartialFunction[DomainEvent, Unit] = {
      case ImdbIdMissing(t, Some(y), _) => seen.append(s"with-year:$t/$y")
    }
    val handleNoYear: PartialFunction[DomainEvent, Unit] = {
      case ImdbIdMissing(t, None, _) => seen.append(s"no-year:$t")
    }
    bus.subscribe(handleWithYear orElse handleNoYear)

    bus.publish(ImdbIdMissing("A", Some(2024), "A"))
    bus.publish(ImdbIdMissing("B", None, "B"))

    seen.toList should contain theSameElementsInOrderAs Seq("with-year:A/2024", "no-year:B")
  }
}
