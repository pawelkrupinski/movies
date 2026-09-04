package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The bound on an anonymous write. `UptimeController.imgEvent` takes its `host`
 * from a visitor's browser and uses it as a KEY in `UptimeMonitor`, so the two
 * things asserted here are the two ways that goes wrong: a host that is not a
 * host at all, and a caller that keeps inventing new ones.
 */
class ImageOriginRosterSpec extends AnyFlatSpec with Matchers {

  "the roster" should "pass through a real origin, once per spelling" in {
    val roster = new ImageOriginRoster()
    roster.label("image.tmdb.org")   shouldBe "image.tmdb.org"
    roster.label("IMAGE.TMDB.ORG")   shouldBe "image.tmdb.org"   // one row, not two
    roster.label("  image.tmdb.org ") shouldBe "image.tmdb.org"
    roster.size shouldBe 1
  }

  it should "refuse anything that is not shaped like a hostname" in {
    val roster = new ImageOriginRoster()
    // A URL, a path, a port, a credential, whitespace, an empty field — all of
    // them are a caller doing something other than naming the CDN that served
    // a poster, and none of them earns a row.
    Seq("", "   ", "not a hostname", "https://image.tmdb.org/p/a.jpg", "image.tmdb.org/p",
        "image.tmdb.org:443", "user@image.tmdb.org", "localhost", "-leading.dash.com",
        "x" * 120 + ".com")
      .foreach(host => roster.label(host) shouldBe ImageOriginRoster.Overflow)
    roster.size shouldBe 0
  }

  it should "fold every host past the cap into one row" in {
    val roster = new ImageOriginRoster(limit = 3)
    Seq("a.com", "b.com", "c.com").foreach(h => roster.label(h) shouldBe h)
    roster.label("d.com") shouldBe ImageOriginRoster.Overflow
    roster.label("e.com") shouldBe ImageOriginRoster.Overflow
    // The hosts already admitted keep their rows — the cap stops growth, it
    // does not start rejecting the origins the page is actually watching.
    roster.label("b.com") shouldBe "b.com"
    roster.size shouldBe 3
  }
}
