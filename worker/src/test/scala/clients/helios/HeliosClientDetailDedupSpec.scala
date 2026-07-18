package clients.helios

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import tools.{CachingDetailFetch, HttpFetch}
import services.cinemas.pl.HeliosClient

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

/**
 * Chain-level detail dedup: a film's `/api/movie/{id}` detail is identical
 * across every Helios location, so when all locations share ONE
 * CachingDetailFetch it is fetched once per chain per TTL instead of once per
 * location per pass.
 */
class HeliosClientDetailDedupSpec extends AnyFlatSpec with Matchers {

  /** Counts GETimestamp that hit the per-film detail endpoint, delegating everything
   *  else to the fixture fetch. */
  private class CountingFetch(delegate: HttpFetch) extends HttpFetch {
    val movieGets = new AtomicInteger(0)
    override def get(url: String): String = {
      if (url.contains("/api/movie/")) movieGets.incrementAndGet()
      delegate.get(url)
    }
    override def getBytes(url: String): Array[Byte]                 = delegate.getBytes(url)
    override def getAsync(url: String): CompletableFuture[String]   = delegate.getAsync(url)
    override def post(url: String, body: String, ct: String): String = delegate.post(url, body, ct)
  }

  private def counting() = new CountingFetch(new FakeHttpFetch("helios/rest-enrichment"))

  "Two Helios locations sharing one detail cache" should "fetch each film's detail only once" in {
    val http        = counting()
    val sharedDetail = new CachingDetailFetch(http, 6.hours)
    val locationA   = new HeliosClient(http, detailHttp = Some(sharedDetail))
    val locationB   = new HeliosClient(http, detailHttp = Some(sharedDetail))

    locationA.fetch()
    val afterFirst = http.movieGets.get()
    afterFirst should be > 0 // the first location actually fetched details

    locationB.fetch()
    // The second location reused the shared cache — no new detail GETimestamp.
    http.movieGets.get() shouldBe afterFirst
  }

  it should "re-fetch per location WITHOUT a shared cache (control — proves the cache is what dedups)" in {
    val http      = counting()
    val locationA = new HeliosClient(http) // detailHttp defaults to http — no caching
    val locationB = new HeliosClient(http)

    locationA.fetch()
    val afterFirst = http.movieGets.get()
    afterFirst should be > 0

    locationB.fetch()
    http.movieGets.get() shouldBe afterFirst * 2 // same films fetched again
  }
}
