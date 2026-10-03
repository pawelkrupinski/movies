package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class ChangeStreamFanoutSpec extends AnyFlatSpec with Matchers {

  "ChangeStreamFanout" should "deliver every upsert to ALL registered listeners" in {
    val fanout = new ChangeStreamFanout[String, String]("test")
    val a, b = ListBuffer.empty[String]
    fanout.register(a += _, _ => ())
    fanout.register(b += _, _ => ())

    fanout.dispatchUpsert("row-1")

    a.toList shouldBe List("row-1")
    b.toList shouldBe List("row-1")  // second listener is NOT clobbered by the first
  }

  it should "stop delivering to a listener once its handle is closed, leaving others attached" in {
    val fanout = new ChangeStreamFanout[String, String]("test")
    val a, b = ListBuffer.empty[String]
    val handleA = fanout.register(a += _, _ => ())
    fanout.register(b += _, _ => ())

    fanout.dispatchUpsert("first")
    handleA.close()
    fanout.dispatchUpsert("second")

    a.toList shouldBe List("first")            // detached before "second"
    b.toList shouldBe List("first", "second")  // still attached
  }

  it should "report isEmpty only once every listener has detached (the cursor-stop signal)" in {
    val fanout = new ChangeStreamFanout[String, String]("test")
    fanout.isEmpty shouldBe true
    val h1 = fanout.register(_ => (), _ => ())
    val h2 = fanout.register(_ => (), _ => ())
    fanout.isEmpty shouldBe false
    h1.close()
    fanout.isEmpty shouldBe false  // h2 still attached — cursor must stay open
    h2.close()
    fanout.isEmpty shouldBe true
  }

  it should "isolate a throwing listener so the others still receive the event" in {
    val fanout = new ChangeStreamFanout[String, String]("test")
    val seen = ListBuffer.empty[String]
    fanout.register(_ => throw new RuntimeException("boom"), _ => ())
    fanout.register(seen += _, _ => ())

    noException should be thrownBy fanout.dispatchUpsert("row")
    seen.toList shouldBe List("row")
  }

  it should "route deletes to every listener's onDelete by id" in {
    val fanout = new ChangeStreamFanout[String, String]("test")
    val deletedA, deletedB = ListBuffer.empty[String]
    fanout.register(_ => (), deletedA += _)
    fanout.register(_ => (), deletedB += _)

    fanout.dispatchDelete("foo|2024")

    deletedA.toList shouldBe List("foo|2024")
    deletedB.toList shouldBe List("foo|2024")
  }

  // Every dispatch caught Throwable, so an interrupt (a stopping worker) or a VirtualMachineError
  // thrown by a listener was logged as one failed apply and the loop carried on.
  it should "let a fatal throw from a listener through, while a non-fatal one still spares the others" in {
    val fanout = new ChangeStreamFanout[String, String]("test")
    val served = ListBuffer.empty[String]
    val failing = fanout.register(_ => throw new IllegalStateException("bad row"), _ => throw new IllegalStateException("bad id"),
      _ => throw new IllegalStateException("bad part"))
    fanout.register(served += _, served += _)
    fanout.dispatchUpsert("row")
    fanout.dispatchDelete("gone")
    fanout.dispatchPart("part") shouldBe Seq(VenueVerdict.Declined(ChangeStreamFanout.PartFailed), VenueVerdict.Declined(ChangeStreamFanout.NoPartHandler))
    served.toList shouldBe List("row", "gone")
    failing.close()

    fanout.register(_ => throw new InterruptedException, _ => throw new StackOverflowError, _ => throw new InterruptedException)
    an[InterruptedException] should be thrownBy fanout.dispatchUpsert("row")
    a[StackOverflowError] should be thrownBy fanout.dispatchDelete("gone")
    an[InterruptedException] should be thrownBy fanout.dispatchPart("part")
  }
}
