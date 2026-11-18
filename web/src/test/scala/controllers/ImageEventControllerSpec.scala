package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import services.UptimeMonitor

/** `POST /uptime/img-event`: the browser-reported poster telemetry, as the uptime monitor's `img:` rows. */
class ImageEventControllerSpec extends AnyFlatSpec with Matchers {

  // ── img-event: the browser-reported poster telemetry ───────────────────────
  //
  // Until 2026-07-19 every event collapsed into one `img: images.weserv.nl`
  // row, because the tracker posted the host of the *proxied* URL. That hid a
  // real outage: m.media-amazon.com was 404ing for every poster it served
  // while the single merged row still looked mostly green from TMDB's traffic.
  // The tracker now posts the ORIGIN host plus whether the attempt was a
  // fallback, so each CDN gets its own row and a dead spare is distinguishable
  // from a poster the user actually saw break.

  // ANONYMOUS ON PURPOSE — no session. The beacon is written by every visitor's
  // browser, so a request carrying an admin session would be the one shape that
  // never happens in production, and is exactly what hid the endpoint being
  // 401ed for three months.
  private def imgEvents(events: JsObject*) =
    FakeRequest().withBody(Json.obj("events" -> JsArray(events)))

  private def imgController = {
    val monitor = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    (new ImageEventController(Helpers.stubControllerComponents(), new ImageLoadBeacon(monitor)), monitor)
  }

  "the /uptime img-event endpoint" should "give each origin host its own service row" in {
    val (ctl, monitor) = imgController
    status(ctl.imgEvent(imgEvents(
      Json.obj("host" -> "m.media-amazon.com", "success" -> false, "error" -> "img load failed"),
      Json.obj("host" -> "image.tmdb.org",     "success" -> true)
    ))) shouldBe NO_CONTENT

    val services = monitor.services
    services should contain ("img: m.media-amazon.com")
    services should contain ("img: image.tmdb.org")
  }

  it should "record a fallback attempt as its own row, separate from the primary" in {
    val (ctl, monitor) = imgController
    status(ctl.imgEvent(imgEvents(
      Json.obj("host" -> "m.media-amazon.com", "success" -> false, "fallback" -> true,
               "error" -> "img load failed"),
      Json.obj("host" -> "m.media-amazon.com", "success" -> false, "error" -> "img load failed")
    ))) shouldBe NO_CONTENT

    val services = monitor.services
    // A failed spare and a failed primary are different severities — the
    // primary is a poster the user watched break, the fallback is only a dead
    // spare behind a poster that may well have rendered fine.
    services should contain ("img: m.media-amazon.com · fallback")
    services should contain ("img: m.media-amazon.com")
  }

  it should "accept the beacon from a visitor who is not signed in" in {
    val (ctl, monitor) = imgController
    // The whole point of the endpoint: a CDN outage is only observable from the
    // browsers that tried to load the posters, and none of those is an admin's.
    status(ctl.imgEvent(imgEvents(Json.obj("host" -> "image.tmdb.org", "success" -> true)))) shouldBe NO_CONTENT
    monitor.services should contain ("img: image.tmdb.org")
  }

  it should "fold a payload with no usable host into the overflow row" in {
    val (ctl, monitor) = imgController
    status(ctl.imgEvent(imgEvents(
      Json.obj("success" -> false),
      Json.obj("host" -> "not a hostname", "success" -> false),
      Json.obj("host" -> "https://image.tmdb.org/p/a.jpg", "success" -> false)
    ))) shouldBe NO_CONTENT
    monitor.services should contain ("img: other")
    monitor.services.filter(_.startsWith("img: ")) should have size 1
  }

  it should "stop an anonymous caller growing the monitor a row at a time" in {
    val monitor = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val ctl = new ImageEventController(Helpers.stubControllerComponents(), new ImageLoadBeacon(monitor))
    // The host is client-controlled and becomes a KEY — one bucket map and one
    // Mongo document per distinct value. Past the cap they all share a row.
    val flood = (1 to ImageOriginRoster.DefaultLimit + 50).map(i =>
      Json.obj("host" -> s"cdn$i.example.com", "success" -> true))
    status(ctl.imgEvent(imgEvents(flood*))) shouldBe NO_CONTENT
    monitor.services.count(_.startsWith("img: ")) shouldBe ImageOriginRoster.DefaultLimit + 1
    monitor.services should contain ("img: other")
  }

  // ── The proxy's own row ────────────────────────────────────────────────────
  //
  // Unwrapping the proxy gave every CDN its own row but left weserv itself with
  // none, so its own failures — a policy block at its edge (`400 Domain or TLD
  // blocked by policy`, the acsta.net case), a 404 because the origin 403s
  // weserv's IP (amazon, multikino), a weserv outage — were all charged to the
  // origin. `proxied` restores a row for the proxy WITHOUT recreating the
  // collapsed one: a proxied success feeds it (so it has green bars and a
  // denominator), while a proxied FAILURE does not, because an <img> error
  // carries no status and could equally be the origin's fault. Only the
  // browser's direct re-probe can tell the two apart, and it posts its verdict
  // as an ordinary event against the proxy host.

  private def counts(monitor: UptimeMonitor, service: String) =
    monitor.history(service).foldLeft((0, 0)) { case ((s, f), b) => (s + b.successes, f + b.failures) }

  "the image proxy's row" should "count every poster weserv served, alongside the origin's own row" in {
    val (ctl, monitor) = imgController
    status(ctl.imgEvent(imgEvents(
      Json.obj("host" -> "image.tmdb.org",  "success" -> true, "proxied" -> true),
      Json.obj("host" -> "image.tmdb.org",  "success" -> true, "proxied" -> true),
      Json.obj("host" -> "www.multikino.pl", "success" -> true)   // SkipHosts: never went through weserv
    ))) shouldBe NO_CONTENT

    counts(monitor, "img: image.tmdb.org")   shouldBe ((2, 0))
    counts(monitor, "img: images.weserv.nl") shouldBe ((2, 0))   // the two proxied loads, not the direct one
  }

  it should "not blame the proxy for a failure that may well be the origin's" in {
    val (ctl, monitor) = imgController
    status(ctl.imgEvent(imgEvents(
      Json.obj("host" -> "image.tmdb.org", "success" -> false, "proxied" -> true,
               "error" -> "img load failed")
    ))) shouldBe NO_CONTENT

    counts(monitor, "img: image.tmdb.org")   shouldBe ((0, 1))
    // Unattributed: the proxy row stays out of it until the direct probe rules.
    monitor.services should not contain ("img: images.weserv.nl")
  }

  it should "record the probe's verdict when the origin served what the proxy could not" in {
    val (ctl, monitor) = imgController
    status(ctl.imgEvent(imgEvents(
      Json.obj("host" -> "image.tmdb.org", "success" -> false, "proxied" -> true,
               "error" -> "img load failed"),
      Json.obj("host" -> "images.weserv.nl", "success" -> false,
               "error" -> "proxy failed, origin served it (https://image.tmdb.org/p/a.jpg)")
    ))) shouldBe NO_CONTENT

    counts(monitor, "img: images.weserv.nl") shouldBe ((0, 1))
    monitor.history("img: images.weserv.nl").flatMap(_.errors).mkString should include ("proxy failed, origin served it")
  }
}
