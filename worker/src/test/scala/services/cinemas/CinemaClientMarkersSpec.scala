package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

import java.time.LocalDate

/**
 * The /uptime client markers are derived from the live `CinemaScraperCatalog`:
 * a cinema is "shared:<Client>" when more than one cinema uses that client class,
 * "custom:<Client>" when it's bespoke. Building the real catalog here means a new
 * cinema (or a client that flips from bespoke to reused, or vice versa) is
 * reclassified automatically — and this spec catches a marker that stops matching
 * reality. Construction touches no network (clients only store the http ref), so
 * a throwing stub suffices.
 */
class CinemaClientMarkersSpec extends AnyFlatSpec with Matchers {

  private val stubHttp = new GetOnlyHttpFetch {
    def get(url: String): String = throw new UnsupportedOperationException(s"no network in this spec ($url)")
  }
  private val catalog = new CinemaScraperCatalog(stubHttp, LocalDate.of(2026, 6, 10))
  private val markers = CinemaClientMarkers.markers(catalog.all)

  "client markers" should "cover every cinema in the catalog" in {
    val cinemas = catalog.all.map(_.cinema.displayName).toSet
    markers.keySet shouldBe cinemas
  }

  it should "mark a widely-reused platform client as shared, naming the client" in {
    // FilmwebShowtimesClient backs ~100 venues — unambiguously shared.
    markers("Kino Tatry") shouldBe "shared:FilmwebShowtimesClient"
  }

  it should "mark a bespoke single-cinema client as custom, naming the client" in {
    // RialtoClient is used by exactly one cinema (Kino Rialto, Poznań).
    markers("Kino Rialto") shouldBe "custom:RialtoClient"
  }

  it should "treat the multi-venue chains (Helios / Cinema City) as shared" in {
    markers("Helios Posnania") shouldBe "shared:HeliosClient"
    markers("Cinema City Poznań Plaza") shouldBe "shared:CinemaCityScraper"
  }

  it should "derive shared vs custom purely from the catalog's per-client cinema count" in {
    val counts = catalog.all.groupBy(_.getClass.getSimpleName).view.mapValues(_.size).toMap
    markers.foreach { case (cinema, tag) =>
      val Array(kind, client) = tag.split(":", 2)
      val expected = if (counts(client) > 1) "shared" else "custom"
      withClue(s"$cinema -> $tag: ") { kind shouldBe expected }
    }
  }
}
