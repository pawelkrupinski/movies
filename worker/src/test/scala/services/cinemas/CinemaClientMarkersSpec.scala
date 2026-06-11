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
  private val sourceUrls = CinemaClientMarkers.sourceUrls(catalog.all)

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

  "source URLs" should "link a Filmweb-backed venue to its public showtimes page by id" in {
    // Kino Tatry is wired `new FilmwebShowtimesClient(http, 2305, …)`; the public
    // showtimes page keys on that id.
    sourceUrls("Kino Tatry") shouldBe "https://www.filmweb.pl/showtimes/-2305"
  }

  it should "link a Cinema City venue to its public venue page by externalCode" in {
    // Poznań Plaza is `cinemaCity("1078", …)`; the id is load-bearing in the URL.
    sourceUrls("Cinema City Poznań Plaza") shouldBe "https://www.cinema-city.pl/kina/cinema-city/1078"
  }

  it should "link a Helios venue to its own repertoire page" in {
    sourceUrls("Helios Posnania") shouldBe "https://helios.pl/poznan/kino-helios"
  }

  it should "link a bespoke own-site venue to its own site URL constant" in {
    // KinoPodBaranamiClient fetches from its `BaseUrl`.
    sourceUrls("Kino Pod Baranami") shouldBe "https://kinopodbaranami.pl"
  }

  it should "link a Bilety24-platform venue to its bilety24 base URL" in {
    // Kino Luna is wired `new Bilety24Client(http, "https://kinoluna.bilety24.pl", …)`,
    // and the client surfaces that base URL as its public page.
    sourceUrls("Kino Luna") shouldBe "https://kinoluna.bilety24.pl"
  }

  it should "omit a venue whose scraper has no stable public page (Multikino is slug-only)" in {
    // MultikinoClient stores only the 4-digit id, which maps to no public URL.
    sourceUrls.get("Multikino Stary Browar") shouldBe None
  }

  it should "only ever produce absolute http(s) URLs" in {
    sourceUrls.values.foreach(u => withClue(s"$u: ")(u should startWith regex "https?://".r))
  }

  "tagsFor" should "layer the FtFW tag on top of the client marker while in fallback" in {
    CinemaClientMarkers.tagsFor(Some("shared:FilmwebShowtimesClient"), None, inFallback = true) shouldBe
      Set("shared:FilmwebShowtimesClient", "fallback:FtFW")
  }

  it should "carry the source page as a url: tag alongside the client marker" in {
    CinemaClientMarkers.tagsFor(Some("custom:RialtoClient"), Some("https://www.kinorialto.poznan.pl"), inFallback = false) shouldBe
      Set("custom:RialtoClient", "url:https://www.kinorialto.poznan.pl")
  }

  it should "drop the FtFW tag once the cinema is no longer in fallback" in {
    CinemaClientMarkers.tagsFor(Some("custom:RialtoClient"), None, inFallback = false) shouldBe
      Set("custom:RialtoClient")
  }

  it should "yield the bare FtFW tag when there is no client marker or url" in {
    CinemaClientMarkers.tagsFor(None, None, inFallback = true) shouldBe Set("fallback:FtFW")
  }
}
