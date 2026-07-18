package views

import controllers.{BarData, ServiceRow}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import tools.{CdpPage, Chrome, TestHttpServer}

/**
 * JS-behaviour regression for the /uptime page's live SSE bar-append path
 * (`applyUpdate` in the inline `<script>` of uptime.scala.html).
 *
 * The server-rendered page emits a COMPLETE 15-min grid — every slot gets a
 * bar, grey "empty" where the service had no activity. But a live SSE frame
 * only ever arrives for a bucket that HAD activity (the poller fires a listener
 * only on a count change). A service idle for a few slots — a cinema on ~hourly
 * cadence, or a quiet enrichment window — therefore had its next active bar
 * appended straight after the last one, silently dropping the empty in-between
 * slots and collapsing the live tail to one bar per hour. A page refresh (full
 * re-render) hid the bug, so it only ever showed on the freshest bars.
 *
 * This drives the real global `applyUpdate` over CDP with a frame for a bucket
 * an hour past the last rendered bar and asserts the skipped slots are
 * backfilled as empty bars, keeping the same grid the full render produces.
 */
class UptimeLiveBarsSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val step   = UptimeMonitor.BucketDurationMs
  private val lastTs = UptimeMonitor.bucketTimestamp(1_752_400_000_000L)

  // One dense, contiguous row of 6 green 15-min bars ending at `lastTs`.
  private def bar(ts: Long): BarData =
    BarData("TestSvc", ts, "12:00", "12:15", "1 Jul", "green", 1, 0, 0, Seq.empty)
  private val row = ServiceRow("TestSvc", (5 to 0 by -1).map(i => bar(lastTs - i * step)))

  // A second row whose bars have real detail behind them: one bucket that
  // recorded a failure (with an error string) and one that recorded nothing.
  // The tooltip reads both from the shared `#uptime-bars` payload — the omitted
  // idle bucket is what "no data" looks like there.
  private val activeTs = lastTs - step
  private val idleTs   = lastTs
  private val detailRow = ServiceRow("DetailSvc", Seq(
    BarData("DetailSvc", activeTs, "09:30", "09:45", "13 Jul", "red", 0, 2, 0, Seq("connect timeout")),
    BarData("DetailSvc", idleTs, "09:45", "10:00", "13 Jul", "empty", 0, 0, 0, Seq.empty),
  ))

  private val uptimeHtml: String =
    views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq(row, detailRow)).body

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart()
    if (chrome.nonEmpty) server = new TestHttpServer({ case "/uptime" => uptimeHtml })
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onUptime(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/uptime")(body(_))
    case None    => cancel("Chrome not installed — skipping uptime JS behaviour test")
  }

  private def barSel(ts: Long): String =
    s"""document.querySelector('.row[data-service="TestSvc"] .bar[data-ts="$ts"]')"""
  private val allBars = """document.querySelectorAll('.row[data-service="TestSvc"] .bar').length"""

  "the /uptime live bar append" should
    "backfill empty bars for slots a service was idle so the tail keeps the 15-min grid" in {
    onUptime { page =>
      page.evalInt(allBars) shouldBe 6

      // An SSE frame for a bucket an hour later — the service recorded nothing in
      // the 3 intervening 15-min slots, so no frame ever arrives for them.
      val newTs = lastTs + 4 * step
      page.eval(
        s"applyUpdate({service:'TestSvc',bucketTs:$newTs,status:'green',fallback:false," +
        "successes:2,failures:0,zeroes:0,errors:[],timeFrom:'a',timeTo:'b',dateLabel:'c'})")

      // The new active bar lands…
      page.evalBool(s"${barSel(newTs)} !== null") shouldBe true
      page.evalString(s"${barSel(newTs)}.className") should include ("green")

      // …and the 3 skipped slots are present as empty bars (not collapsed away).
      for (i <- 1 to 3) {
        val ts = lastTs + i * step
        withClue(s"missing empty backfill bar at +$i slot ($ts): ") {
          page.evalBool(s"${barSel(ts)} !== null") shouldBe true
          page.evalString(s"${barSel(ts)}.className") should include ("empty")
        }
      }

      // 6 pre-rendered + 3 empty backfill + 1 new active.
      page.evalInt(allBars) shouldBe 10
    }
  }

  private def hover(service: String, ts: Long): String =
    s"""document.querySelector('.row[data-service="$service"] .bar[data-ts="$ts"]')""" +
      """.dispatchEvent(new MouseEvent('mouseenter'))"""
  private val overlayText = "document.getElementById('overlay').textContent"

  // The per-bucket detail moved out of a per-bar `data-info` attribute and into
  // one `#uptime-bars` payload (controllers.UptimeBarPayload) — the old shape
  // OOM'd the German deployment. These lock that hover still shows the same
  // thing, sourced from the payload.
  "the /uptime bar tooltip" should "read a bucket's detail from the shared payload" in {
    onUptime { page =>
      page.eval(hover("DetailSvc", activeTs))
      val text = page.evalString(overlayText)

      text should include ("DetailSvc")          // the row's service, not a per-bar copy
      text should include ("13 Jul")             // slot labels, shared across services
      text should include ("09:30")
      text should include ("2 failed")
      text should include ("connect timeout")    // the bucket's error survives
      page.evalBool("document.getElementById('overlay').classList.contains('visible')") shouldBe true
    }
  }

  it should "show 'No data' for a slot the payload omits" in {
    onUptime { page =>
      page.eval(hover("DetailSvc", idleTs))
      val text = page.evalString(overlayText)

      text should include ("No data")
      text should include ("09:45")              // the slot is still labelled …
      text should not include ("connect timeout")  // … it just has no detail of its own
    }
  }

  it should "fold a live SSE update into the payload so the tooltip shows the new counts" in {
    onUptime { page =>
      page.eval(
        s"applyUpdate({service:'DetailSvc',bucketTs:$activeTs,status:'green',fallback:false," +
        "successes:9,failures:0,zeroes:0,errors:[],timeFrom:'09:30',timeTo:'09:45',dateLabel:'13 Jul'})")
      page.eval(hover("DetailSvc", activeTs))
      val text = page.evalString(overlayText)

      text should include ("9 ok")
      text should not include ("connect timeout")  // the stale error is replaced
    }
  }
}
