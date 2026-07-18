package views

import controllers.{BarData, ServiceRow, UptimeBarPayload}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

/**
 * The uptime page used to inline a full JSON blob into every bar's `data-info`
 * attribute — HTML-escaped, so each `"` cost 6 bytes. The German deployment
 * registers ~1550 services and the render emits a complete 96-slot grid for each
 * (empty slots included), so that was ~149k JSON-bearing cells ≈ 57 MB of HTML
 * built through nested Twirl StringBuilders. It OOM'd the 384 MB heap, and since
 * Pekko exits the JVM on a fatal error, one /uptime load took the whole site down.
 *
 * The fix moves the per-bucket detail into ONE payload emitted next to the grid:
 * the 96 slot labels once (not once per service), and bucket counts only for
 * buckets that actually recorded something. The bars keep just their status class
 * and `data-ts`, which is all the grid itself needs.
 */
class UptimeBarPayloadSpec extends AnyFlatSpec with Matchers {

  private val ts = 1_752_400_000_000L

  private def active(service: String, at: Long) =
    BarData(service, at, "12:00", "12:15", "1 Jul", "green", 3, 1, 0, Seq("boom"))
  private def empty(service: String, at: Long) =
    BarData(service, at, "12:15", "12:30", "1 Jul", "empty", 0, 0, 0, Seq.empty)

  "the bar payload" should "carry a bucket that recorded activity" in {
    val payload = Json.parse(UptimeBarPayload(Seq(ServiceRow("Kino Muza", Seq(active("Kino Muza", ts))))))

    val bucket = (payload \ "data" \ "Kino Muza" \ ts.toString).get
    (bucket \ "successes").as[Int] shouldBe 3
    (bucket \ "failures").as[Int] shouldBe 1
    (bucket \ "errors").as[Seq[String]] shouldBe Seq("boom")
  }

  it should "omit buckets that recorded nothing — the grid's empty class already says so" in {
    val row = ServiceRow("Kino Muza", Seq(active("Kino Muza", ts), empty("Kino Muza", ts + 1)))
    val payload = Json.parse(UptimeBarPayload(Seq(row)))

    (payload \ "data" \ "Kino Muza" \ ts.toString).toOption should be (defined)
    (payload \ "data" \ "Kino Muza" \ (ts + 1).toString).toOption shouldBe None
  }

  it should "label each slot once, not once per service" in {
    val rows = Seq(
      ServiceRow("Kino Muza",   Seq(empty("Kino Muza", ts))),
      ServiceRow("Kino Apollo", Seq(empty("Kino Apollo", ts))),
    )
    val payload = Json.parse(UptimeBarPayload(rows))

    val slot = (payload \ "slots" \ ts.toString).get
    (slot \ "timeFrom").as[String] shouldBe "12:15"
    (slot \ "dateLabel").as[String] shouldBe "1 Jul"
    // One shared slot entry covers both services — the label appears exactly once.
    payload.toString.sliding("\"1 Jul\"".length).count(_ == "\"1 Jul\"") shouldBe 1
  }

  it should "include a cinema's enrichment sub-row, which renders its own bars" in {
    val enrichment = ServiceRow("Kino Muza|enrichment", Seq(active("Kino Muza|enrichment", ts)))
    val row = ServiceRow("Kino Muza", Seq(empty("Kino Muza", ts)), enrichment = Some(enrichment))
    val payload = Json.parse(UptimeBarPayload(Seq(row)))

    (payload \ "data" \ "Kino Muza|enrichment" \ ts.toString).toOption should be (defined)
  }

  it should "escape `<` so an error string can't break out of the <script> block" in {
    val row = ServiceRow("Kino Muza",
      Seq(active("Kino Muza", ts).copy(errors = Seq("</script><script>alert(1)</script>"))))

    UptimeBarPayload(Seq(row)) should not include ("</script>")
  }

  "the rendered uptime page" should "not inline a JSON blob into every bar" in {
    val row = ServiceRow("Kino Muza", Seq(active("Kino Muza", ts)))
    val html = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq("Poznań" -> Seq(row)), Seq.empty, Seq.empty).body

    html should not include ("data-info=")
    html should include ("""id="uptime-bars"""")
    // The bar still carries what the grid needs: its status class and its slot.
    html should include (s"""data-ts="$ts"""")
  }

  // The OOM regression guard. A roster the size of Germany's, rendered as a full
  // 96-slot grid that is almost entirely empty. Before the fix each of those
  // ~19k cells carried an escaped JSON blob (~380 B) — ~7 MB for this reduced
  // roster, and ~57 MB for the real 1550-service one. The bars are now ~45 B.
  it should "keep the grid small when a large roster renders mostly-empty slots" in {
    val services = (1 to 200).map(i => s"Kino $i")
    val rows = services.map { s =>
      ServiceRow(s, (0 until 96).map(slot => empty(s, ts + slot)))
    }
    val html = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq("Poznań" -> rows), Seq.empty, Seq.empty).body

    info(s"rendered ${html.length / 1024} KB for 200 services × 96 empty slots (was ~8100 KB)")
    html.length should be < 2 * 1024 * 1024
  }
}
