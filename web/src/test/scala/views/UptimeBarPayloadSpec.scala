package views

import controllers.{BarData, ServiceRow, UptimeBarPayload}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The uptime page used to inline a full JSON blob into every bar's `data-info`
 * attribute — HTML-escaped, so each `"` cost 6 bytes. The German deployment
 * registers ~1550 services and the render emits a complete 96-slot grid for each
 * (empty slots included), so that was ~149k JSON-bearing cells ≈ 57 MB of HTML
 * built through nested Twirl StringBuilders. It OOM'd the 384 MB heap, and since
 * Pekko exits the JVM on a fatal error, one /uptime load took the whole site down.
 *
 * The detail then moved into ONE payload emitted next to the grid — but the grid
 * still rendered ~70k bar `<div>`s, and the payload repeated every key name and
 * error string per bucket: 12–17 MB raw, ~1 s server-side per load in prod. Now
 * the payload is the whole grid, every repeated value written once, and the page
 * script builds the bars from it.
 */
class UptimeBarPayloadSpec extends AnyFlatSpec with Matchers {

  private val ts = 1_752_400_000_000L

  private def active(service: String, at: Long) =
    BarData(service, at, "12:00", "12:15", "1 Jul", "green", 3, 1, 0, Seq("boom"))
  private def empty(service: String, at: Long) =
    BarData(service, at, "12:15", "12:30", "1 Jul", "empty", 0, 0, 0, Seq.empty)

  "the bar payload" should "carry a bucket that recorded activity" in {
    val payload = UptimePayload.of(UptimeBarPayload(Seq(ServiceRow("Kino Muza", Seq(active("Kino Muza", ts))))))

    payload.bucket("Kino Muza", ts) shouldBe Some(UptimePayload.Bucket("green", 3, 1, 0,
      fallback = false, thin = false, errors = Seq("boom")))
  }

  it should "carry the fallback and thin marks" in {
    val bar = active("Kino Muza", ts).copy(fallback = true, thin = true)
    val bucket = UptimePayload.of(UptimeBarPayload(Seq(ServiceRow("Kino Muza", Seq(bar))))).bucket("Kino Muza", ts).get
    bucket.fallback shouldBe true
    bucket.thin shouldBe true
  }

  it should "omit buckets that recorded nothing — an absent bucket is the empty bar" in {
    val row = ServiceRow("Kino Muza", Seq(active("Kino Muza", ts), empty("Kino Muza", ts + 1)))
    val payload = UptimePayload.of(UptimeBarPayload(Seq(row)))

    payload.bucket("Kino Muza", ts) should be (defined)
    payload.bucket("Kino Muza", ts + 1) shouldBe None
    // …but its slot is still a column of the grid.
    payload.slots.map(_.timestamp) shouldBe Seq(ts, ts + 1)
  }

  it should "label each slot once, not once per service" in {
    val rows = Seq(
      ServiceRow("Kino Muza",   Seq(empty("Kino Muza", ts))),
      ServiceRow("Kino Apollo", Seq(empty("Kino Apollo", ts))),
    )
    val json = UptimeBarPayload(rows)

    UptimePayload.of(json).slots shouldBe Seq(UptimePayload.Slot(ts, "12:15", "12:30", "1 Jul"))
    json.sliding("\"1 Jul\"".length).count(_ == "\"1 Jul\"") shouldBe 1
  }

  it should "write a repeated error string once" in {
    val rows = (1 to 3).map(i => ServiceRow(s"Kino $i", Seq(active(s"Kino $i", ts), active(s"Kino $i", ts + 1))))
    val json = UptimeBarPayload(rows)

    json.sliding("\"boom\"".length).count(_ == "\"boom\"") shouldBe 1
    UptimePayload.of(json).bucket("Kino 3", ts + 1).map(_.errors) shouldBe Some(Seq("boom"))
  }

  it should "include a cinema's enrichment sub-row, which renders its own bars" in {
    val enrichment = ServiceRow("Kino Muza|enrichment", Seq(active("Kino Muza|enrichment", ts)))
    val row = ServiceRow("Kino Muza", Seq(empty("Kino Muza", ts)), enrichment = Some(enrichment))

    UptimePayload.of(UptimeBarPayload(Seq(row))).bucket("Kino Muza|enrichment", ts) should be (defined)
  }

  it should "write a service rendered in two sections once" in {
    val row = ServiceRow("Kino Muza", Seq(active("Kino Muza", ts)))
    UptimePayload.of(UptimeBarPayload(Seq(row, row))).services shouldBe Set("Kino Muza")
  }

  it should "escape `<` so an error string can't break out of the <script> block" in {
    val row = ServiceRow("Kino Muza",
      Seq(active("Kino Muza", ts).copy(errors = Seq("</script><script>alert(1)</script>"))))

    UptimeBarPayload(Seq(row)) should not include ("</script>")
  }

  "the rendered uptime page" should "draw no bars server-side — the script builds them from the payload" in {
    val row = ServiceRow("Kino Muza", Seq(active("Kino Muza", ts)))
    val html = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq("Poznań" -> Seq(row)), Seq.empty, Seq.empty, current = models.Country.Poland).body

    html should not include ("data-info=")
    html should not include ("""class="bar """)
    html should include ("""<div class="bars"></div>""")
    UptimePayload.inPage(html).bucket("Kino Muza", ts) should be (defined)
  }

  // The OOM regression guard, and the load-time one: a roster the size of
  // Germany's — the first version put an escaped JSON blob in each of these
  // cells (~7 MB for this reduced roster), the second still ~45 B of markup.
  it should "keep the page small when a large roster renders mostly-empty slots" in {
    val services = (1 to 200).map(i => s"Kino $i")
    val rows = services.map { s =>
      ServiceRow(s, (0 until 96).map(slot => empty(s, ts + slot)))
    }
    val html = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq("Poznań" -> rows), Seq.empty, Seq.empty, current = models.Country.Poland).body

    info(s"rendered ${html.length / 1024} KB for 200 services × 96 empty slots (was ~8100 KB, then ~1000 KB)")
    html.length should be < 256 * 1024
  }

  // A busy roster — every bucket recorded, the same error recurring — is what
  // prod's PL page looks like. Full JSON objects per bucket made this ~3 MB.
  it should "keep the payload small when every bucket of a large roster recorded activity" in {
    val rows = (1 to 200).map { i =>
      val s = s"Kino $i"
      ServiceRow(s, (0 until 96).map(slot =>
        active(s, ts + slot).copy(errors = Seq("SocketTimeoutException: connect timed out after 30000 ms"))))
    }
    val json = UptimeBarPayload(rows)

    info(s"payload ${json.length / 1024} KB for 200 services × 96 recorded buckets")
    json.length should be < 640 * 1024
  }
}
