package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RecordingHttpFetch

import scala.concurrent.duration._

class MongoCachingDetailFetchSpec extends AnyFlatSpec with Matchers {

  "MongoCachingDetailFetch without a database" should "pass every GET straight through (no caching)" in {
    val under = new RecordingHttpFetch
    val fetch = new MongoCachingDetailFetch(under, db = None, ttl = 6.hours, chain = DetailCacheChain("test"), ttlMismatches = new services.TtlIndexMismatches)
    fetch.get("https://x/film/1") shouldBe "body of https://x/film/1"
    fetch.get("https://x/film/1") shouldBe "body of https://x/film/1"
    under.calls shouldBe 2 // no Mongo → no dedup
  }

  "A stored detail body" should "round-trip exactly and shrink a recorded page several-fold, per chain" in {
    def fixture(path: String) = java.nio.file.Files.readString(java.nio.file.Paths.get(s"test/resources/fixtures/$path"))
    val pages = Seq(
      fixture("flicks/www.flicks.us/movie/pans-labyrinth.html"),
      fixture("flicks/www.flicks.co.uk/movie/empire-of-the-sun.html"),
      fixture("cinema-city-plaza/www.cinema-city.pl/filmy/diabel-ubiera-sie-u-prady-2/7795s2r"))
    pages.foreach { page =>
      val stored = MongoCachingDetailFetch.compress(page)
      MongoCachingDetailFetch.decompress(stored) shouldBe page
      stored.length should be < page.getBytes(java.nio.charset.StandardCharsets.UTF_8).length / 4
    }
  }

  it should "refuse a corrupt payload rather than hand back garbage, so the cache reads it as a miss" in {
    a [java.io.IOException] should be thrownBy MongoCachingDetailFetch.decompress("<html>not gzip</html>".getBytes)
  }
}
