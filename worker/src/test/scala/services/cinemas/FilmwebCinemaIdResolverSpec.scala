package services.cinemas

import models._
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tools.GetOnlyHttpFetch
import services.cinemas.pl.FilmwebCinemaIdResolver
import services.cinemas.pl.FilmwebCinemaIdResolver._

import java.nio.file.{Files, Paths}

/** Exercises the runtime Filmweb-id resolution offline: parse real town
 *  listings (`/api/v1/city/<id>/cinemas`, captured 2026-09-29), fuzzy-match our
 *  `Cinema.displayName`s against them, and confirm the override map wins first
 *  (incl. the suppressed Kino Apollo). No network — `resolveAll` is driven
 *  through a fixture-backed fetch stub serving the captured `/api/v1/cities`. */
class FilmwebCinemaIdResolverSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def fixture(name: String): String =
    new String(Files.readAllBytes(Paths.get(s"worker/src/test/resources/fixtures/filmweb-city-cinemas/$name")), "UTF-8")

  // Filmweb town ids, from the captured cities.json.
  private val KrakowId = 2
  private val PoznanId = 6
  private val JaroslawId = 147

  private val poznanListing = parseCinemaListing(fixture(s"city-$PoznanId.json"))
  private val krakowListing = parseCinemaListing(fixture(s"city-$KrakowId.json"))

  "parseCinemaListing" should "extract (name, id) pairs from a town's cinema listing" in {
    val byId = poznanListing.map(c => c.id -> c.name).toMap
    byId(75)   shouldBe "Muza"
    byId(78)   shouldBe "Rialto"
    byId(633)  shouldBe "Multikino Stary Browar"
    byId(1618) shouldBe "Bułgarska 19"
    byId(624)  shouldBe "Cinema City Kinepolis"
    byId.size  shouldBe 11
  }

  "parseTowns" should "map town names to Filmweb town ids" in {
    val towns = parseTowns(fixture("cities.json"))
    towns.find(_.name == "Jarosław").value.id shouldBe JaroslawId
    towns.count(_.name == "Skarżysko-Kamienna") shouldBe 2 // names are not unique
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

  it should "pin Multikino Rumia to 1464, not the bare 'Multikino' fuzzy match" in {
    // Regression: Filmweb's Gdynia/Trójmiasto listing carries a bare "Multikino"
    // (no district suffix). Against "Multikino Rumia" that scores exactly the
    // 0.5 AcceptThreshold, so unpinned it would silently resolve to the wrong
    // Multikino instead of Unmatched or the real Rumia id.
    val bareMultikinoListing = Seq(FilmwebCinema("Multikino", 999))
    bestMatch("Multikino Rumia", bareMultikinoListing).value.id shouldBe 999
    resolver.resolveOne(MultikinoRumia, bareMultikinoListing) shouldBe
      Resolution(MultikinoRumia, Some(1464), Override)
  }

  it should "pin Kinoteka to 55 even when the city listing is unavailable" in {
    // kinoteka.pl is down at the TCP layer, so the venue lives on the Filmweb
    // fallback. The override must resolve its id WITHOUT a successful
    // town-listing fetch — an empty listing (the boot-time blip that
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

  it should "resolve a venue outside the five big cities through its own town's listing" in {
    // Regression: the resolver only fetched the Poznań/Wrocław/Warszawa/Kraków/
    // Trójmiasto listings, so a Jarosław venue never got a Filmweb id and its
    // fallback could not engage. jaroslaw.kinonabiegunach.pl stopped answering on
    // 2026-09-22 and the venue went dark for a week while Filmweb 2172 carried
    // its whole programme.
    val byCinema = stubResolver.resolveAll(Set("jaroslaw")).map(r => r.cinema -> r).toMap

    byCinema(KinoNaBiegunach).filmwebId.value shouldBe 2172
    byCinema(KinoIkar).filmwebId.value        shouldBe 1707
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

  /** Serves the captured town list and each captured town listing; any other
   *  town answers an empty listing — enough to drive `resolveAll` offline. */
  private val stubResolver = new FilmwebCinemaIdResolver(new GetOnlyHttpFetch {
    private val TownListing = """.*/api/v1/city/(\d+)/cinemas""".r
    override def get(url: String): String = url match {
      case FilmwebCinemaIdResolver.TownsUrl                              => fixture("cities.json")
      case TownListing(id) if Set(KrakowId, PoznanId, JaroslawId)(id.toInt) => fixture(s"city-$id.json")
      case _                                                             => "[]"
    }
  })

  private object NoNetworkFetch extends GetOnlyHttpFetch {
    override def get(url: String): String =
      throw new AssertionError(s"resolveOne/bestMatch must not hit the network (url=$url)")
  }
}
