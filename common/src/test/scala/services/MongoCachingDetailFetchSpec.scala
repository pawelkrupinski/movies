package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

import scala.concurrent.duration._

class MongoCachingDetailFetchSpec extends AnyFlatSpec with Matchers {

  private class CountingFetch extends GetOnlyHttpFetch {
    var gets = 0
    override def get(url: String): String = { gets += 1; s"body-of-$url" }
  }

  "MongoCachingDetailFetch without a database" should "pass every GET straight through (no caching)" in {
    val under = new CountingFetch
    val fetch = new MongoCachingDetailFetch(under, db = None, ttl = 6.hours)
    fetch.get("https://x/film/1") shouldBe "body-of-https://x/film/1"
    fetch.get("https://x/film/1") shouldBe "body-of-https://x/film/1"
    under.gets shouldBe 2 // no Mongo → no dedup
  }
}
