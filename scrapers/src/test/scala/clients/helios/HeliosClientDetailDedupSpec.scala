package clients.helios

import org.scalatest.matchers.should.Matchers
import clients.tools.{FakeHttpFetch, RequestLogHttpFetch}
import org.scalatest.flatspec.AnyFlatSpec
import tools.CachingDetailFetch
import services.cinemas.pl.HeliosClient

import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Chain-level detail dedup: a film's `/api/movie/{id}` detail is identical
 * across every Helios location, so when all locations share ONE
 * CachingDetailFetch it is fetched once per chain per TTL instead of once per
 * location per pass.
 */
class HeliosClientDetailDedupSpec extends AnyFlatSpec with Matchers {

  private def logged() = new RequestLogHttpFetch(new FakeHttpFetch("helios/rest-enrichment"))

  /** GETs that hit the per-film detail endpoint. */
  private def movieGets(http: RequestLogHttpFetch) = http.gets.count(_.contains("/api/movie/"))

  "Two Helios locations sharing one detail cache" should "fetch each film's detail only once" in {
    val http         = logged()
    val sharedDetail = new CachingDetailFetch(http, 6.hours)
    val locationA   = new HeliosClient(http, detailHttp = Some(sharedDetail), titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay)
    val locationB   = new HeliosClient(http, detailHttp = Some(sharedDetail), titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay)

    locationA.fetch()
    val afterFirst = movieGets(http)
    afterFirst should be > 0 // the first location actually fetched details

    locationB.fetch()
    // The second location reused the shared cache — no new detail GETs.
    movieGets(http) shouldBe afterFirst
  }

  it should "re-fetch per location WITHOUT a shared cache (control — proves the cache is what dedups)" in {
    val http      = logged()
    val locationA = new HeliosClient(http, titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay) // detailHttp defaults to http — no caching
    val locationB = new HeliosClient(http, titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay)

    locationA.fetch()
    val afterFirst = movieGets(http)
    afterFirst should be > 0

    locationB.fetch()
    movieGets(http) shouldBe afterFirst * 2 // same films fetched again
  }
}
