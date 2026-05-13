package clients.helios

import clients.HeliosClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch

import java.util.concurrent.CompletableFuture

class HeliosClientResilienceSpec extends AnyFlatSpec with Matchers {

  // A FakeHttpFetch that returns "[]" for the REST screening URL and fails for any
  // other REST endpoint, so the client must fall back to NUXT-only data.
  private class NuxtOnlyHttpFetch extends HttpFetch {
    private val nuxt = new FakeHttpFetch("helios/rest-enrichment")

    override def get(url: String): String =
      if (url.contains("restapi.helios.pl")) "[]"
      else nuxt.get(url)

    override def getAsync(url: String): CompletableFuture[String] =
      if (url.contains("restapi.helios.pl")) {
        val f = new CompletableFuture[String]()
        f.completeExceptionally(new java.io.IOException("REST unavailable"))
        f
      } else nuxt.getAsync(url)
  }

  private val client  = new HeliosClient(new NuxtOnlyHttpFetch)
  private val results = client.fetch()

  "HeliosClient.fetch" should "still return NUXT movies when the REST screening API is unavailable" in {
    results                       should not be empty
    results.flatMap(_.showtimes)  should not be empty
  }

  it should "not include REST-only movies when the REST screening API is unavailable" in {
    // Without REST, Latin-titled UA entries (REST-only) cannot appear; their Cyrillic NUXT siblings still do.
    val titles = results.map(_.movie.title).toSet
    titles should not contain "Mortal Kombat II - UA"
    titles should not contain "Dyyavol Nosyt' Prada 2 - UA "
    titles should contain ("МОРТАЛ КОМБАТ ІІ")
    titles should contain ("ДИЯВОЛ НОСИТЬ ПРАДА 2")
  }

  it should "leave all showtimes un-enriched (no room or format) when REST is unavailable" in {
    val all = results.flatMap(_.showtimes)
    all.exists(_.room.isDefined)   shouldBe false
    all.exists(_.format.nonEmpty) shouldBe false
  }

  it should "leave REST-derived metadata empty when REST is unavailable" in {
    results.forall(_.synopsis.isEmpty) shouldBe true
    results.forall(_.cast.isEmpty)     shouldBe true
    results.forall(_.director.isEmpty) shouldBe true
    results.forall(_.movie.releaseYear.isEmpty) shouldBe true
  }
}
