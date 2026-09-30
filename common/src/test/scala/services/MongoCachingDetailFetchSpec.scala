package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RecordingHttpFetch

import scala.concurrent.duration._

class MongoCachingDetailFetchSpec extends AnyFlatSpec with Matchers {

  "MongoCachingDetailFetch without a database" should "pass every GET straight through (no caching)" in {
    val under = new RecordingHttpFetch
    val fetch = new MongoCachingDetailFetch(under, db = None, ttl = 6.hours, collectionName = "detailCache-test", ttlMismatches = new services.TtlIndexMismatches)
    fetch.get("https://x/film/1") shouldBe "body of https://x/film/1"
    fetch.get("https://x/film/1") shouldBe "body of https://x/film/1"
    under.calls shouldBe 2 // no Mongo → no dedup
  }
}
