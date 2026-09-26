package services.alerts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

/** A whole-aggregator outage hands every venue on it to its fallback at once —
 *  ~1,135 German venues on Filmstarts — and each would page ENTER, then RECOVERED. */
class BurstLimitedNotifierSpec extends AnyFlatSpec with Matchers {

  private class Harness {
    val clock    = new MutableClock(Instant.parse("2026-09-26T12:00:00Z"))
    val sent     = ListBuffer.empty[String]
    val notifier = new BurstLimitedNotifier(message => { sent += message; () }, AlertBurst(3, 1.hour), clock, "ENTER")
  }

  "BurstLimitedNotifier" should "pass pages through while under the limit" in {
    val h = new Harness
    Seq("a", "b", "c").foreach(h.notifier.send)
    h.sent.toList shouldBe List("a", "b", "c")
  }

  it should "hold pages past the limit, saying so once rather than dropping them silently" in {
    val h = new Harness
    (1 to 10).map(i => s"page $i").foreach(h.notifier.send)
    h.sent should have size 4
    h.sent.take(3).toList shouldBe List("page 1", "page 2", "page 3")
    h.sent(3) should include ("/uptime")
    h.sent(3) should include ("ENTER")    // says WHICH pages it is holding
  }

  it should "count what it held when the next window opens" in {
    val h = new Harness
    (1 to 10).map(i => s"page $i").foreach(h.notifier.send)
    h.clock.advance(Duration.ofMinutes(61))
    h.notifier.send("later")
    h.sent.drop(4).toList.head should include ("7")   // pages 4..10 were held
    h.sent.last shouldBe "later"
  }

  // An outage's ENTER flood must not spend the budget the pages that close it out
  // — RECOVERED, and the one-off gone-venue page — need: each kind has its own.
  "BurstLimitedPager" should "give each kind of page its own budget" in {
    val clock = new MutableClock(Instant.parse("2026-09-26T12:00:00Z"))
    val sent  = ListBuffer.empty[String]
    val pager = new BurstLimitedPager(message => { sent += message; () }, AlertBurst(3, 1.hour), clock)
    (1 to 10).foreach(i => pager.pagerFor("ENTER")(s"enter $i"))
    pager.pagerFor("RECOVERED")("recovered 1")
    pager.pagerFor("gone-venue")("gone 1")
    sent should contain allOf ("recovered 1", "gone 1")
  }
}
