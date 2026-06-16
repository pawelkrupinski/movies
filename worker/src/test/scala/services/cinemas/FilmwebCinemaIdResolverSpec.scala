package services.cinemas

import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FilmwebCinemaIdResolver._
import tools.GetOnlyHttpFetch

import java.net.URI
import java.nio.file.{Files, Paths}

/** Exercises the runtime Filmweb-id resolution offline: parse a real (trimmed)
 *  `/showtimes/Poznań` listing, fuzzy-match our Poznań `Cinema.displayName`s
 *  against it, and confirm the override map wins first (incl. the suppressed
 *  Kino Apollo). No network — `resolveAll` is driven through a fixture-backed
 *  fetch stub. */
class FilmwebCinemaIdResolverSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val poznanListingHtml: String = {
    val p = Paths.get("worker/src/test/resources/fixtures/filmweb-showtimes-listing/poznan.html")
    new String(Files.readAllBytes(p), "UTF-8")
  }
  private val poznanListing = parseCinemaListing(poznanListingHtml)

  private val krakowListing = parseCinemaListing(
    new String(Files.readAllBytes(
      Paths.get("worker/src/test/resources/fixtures/filmweb-showtimes-listing/krakow.html")), "UTF-8"))

  "parseCinemaListing" should "extract (name, id) pairs from the showtimes links" in {
    val byId = poznanListing.map(c => c.id -> c.name).toMap
    byId(75)   shouldBe "Muza"
    byId(78)   shouldBe "Rialto"
    byId(633)  shouldBe "Multikino Stary Browar"
    byId(1618) shouldBe "Bułgarska 19" // URL-decoded, %C5%82 → ł
    byId(624)  shouldBe "Cinema City Kinepolis"
    byId.size  shouldBe 11
  }

  "bestMatch" should "fuzzy-match our display names to the right Filmweb id" in {
    bestMatch("Kino Muza", poznanListing).value.id              shouldBe 75
    bestMatch("Kino Rialto", poznanListing).value.id            shouldBe 78
    bestMatch("Multikino Stary Browar", poznanListing).value.id shouldBe 633
    bestMatch("Cinema City Kinepolis", poznanListing).value.id  shouldBe 624
    bestMatch("Helios Posnania", poznanListing).value.id        shouldBe 1943
    bestMatch("Kino Bułgarska 19", poznanListing).value.id      shouldBe 1618
    bestMatch("Kino Pałacowe", poznanListing).value.id          shouldBe 1854
    // "Cinema City Poznań Plaza" ↔ Filmweb "Cinema City Plaza" — shared tokens.
    bestMatch("Cinema City Poznań Plaza", poznanListing).value.id shouldBe 568
  }

  it should "not cross-match an unrelated venue" in {
    bestMatch("Kino Atlantic", poznanListing) shouldBe None
  }

  it should "pick the most-specific listing when one name is a prefix of another" in {
    // Regression: with a subset-boost-to-1.0 metric "Mikro Bronowice" tied with
    // the bare "Mikro" and lost the alphabetical tie-break, resolving to id 24
    // (Mikro) instead of 1785. The overlap-coefficient metric disambiguates.
    bestMatch("Mikro Bronowice", krakowListing).value.id shouldBe 1785
    bestMatch("Kino Mikro", krakowListing).value.id      shouldBe 24
  }

  "resolveOne" should "let the override map win over fuzzy matching" in {
    // Kino Amondo is overridden to 2077 even though no such listing is present.
    resolver.resolveOne(KinoAmondo, poznanListing) shouldBe
      Resolution(KinoAmondo, Some(2077), Override)
  }

  it should "pin Kinoteka to 55 even when the city listing is unavailable" in {
    // kinoteka.pl is down at the TCP layer, so the venue lives on the Filmweb
    // fallback. The override must resolve its id WITHOUT a successful
    // /showtimes/Warszawa fetch — an empty listing (the boot-time blip that
    // produced the red /uptime bar) must still yield id 55, not Unmatched.
    resolver.resolveOne(Kinoteka, Nil) shouldBe
      Resolution(Kinoteka, Some(55), Override)
  }

  it should "suppress Kino Apollo to NO_FILMWEB_ID despite a (empty) Filmweb listing" in {
    val r = resolver.resolveOne(KinoApollo, poznanListing)
    r.filmwebId shouldBe None
    r.source    shouldBe OverrideSuppressed
    r.resolved  shouldBe false
  }

  it should "report an unmatched cinema as NO_FILMWEB_ID, not an error" in {
    val r = resolver.resolveOne(KinoSwit, poznanListing) // no Świt in the Poznań listing
    r.filmwebId shouldBe None
    r.source    shouldBe Unmatched
    r.resolved  shouldBe false
  }

  "resolveAll" should "resolve Poznań cinemas end-to-end through a fixture-backed fetch" in {
    val byCinema = stubResolver.resolveAll(Set("poznan")).map(r => r.cinema -> r).toMap

    byCinema(KinoMuza).filmwebId.value  shouldBe 75
    byCinema(Rialto).filmwebId.value    shouldBe 78
    byCinema(Multikino).filmwebId.value shouldBe 633
    byCinema(KinoApollo).resolved       shouldBe false // suppressed override
    // Every Poznań cinema appears exactly once.
    byCinema.keySet shouldBe Cinema.poznan.toSet
  }

  private val resolver = new FilmwebCinemaIdResolver(NoNetworkFetch)

  /** A fetch that returns the Poznań listing fixture for the Poznań URL and an
   *  empty listing for any other city — enough to drive `resolveAll` offline. */
  private val stubResolver = new FilmwebCinemaIdResolver(new GetOnlyHttpFetch {
    override def get(url: String): String = {
      val path = new URI(url).getPath
      if (path.contains("Pozna")) poznanListingHtml else "<html></html>"
    }
  })

  private object NoNetworkFetch extends GetOnlyHttpFetch {
    override def get(url: String): String =
      throw new AssertionError(s"resolveOne/bestMatch must not hit the network (url=$url)")
  }
}
