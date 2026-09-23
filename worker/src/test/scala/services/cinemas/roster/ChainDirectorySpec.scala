package services.cinemas.roster

import clients.tools.FakeHttpFetch
import models.{Country, GeoPoint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog
import services.cinemas.roster.ChainDirectory._
import services.cinemas.roster.RosterAuditFixtures._
import services.cinemas.roster.RosterFinding._
import services.cinemas.roster.SourceReading._
import tools.{FetchedPage, HttpStatusException}

import java.time.LocalDate

/** The chains' own venue lists, recorded 2026-09-23, read by the online roster
 *  audit — and the whole Polish roster's chain venues checked against them. */
class ChainDirectorySpec extends AnyFlatSpec with Matchers {

  private val Today = LocalDate.of(2026, 9, 23)

  /** Serves each chain's recorded list at the URL the audit asks it at; the
   *  Multikino home page answers empty, as a warm-up only needs its cookies. */
  private val recorded: String => FetchedPage = {
    val byUrl = Map(
      Helios.listUrl(Today)     -> page(HeliosCinemas),
      CinemaCity.listUrl(Today) -> page(CinemaCityCinemas),
      Multikino.listUrl(Today)  -> page(MultikinoCinemas),
      Multikino.warmUpUrl.get   -> "")
    url => byUrl.get(url).map(FetchedPage(url, _)).getOrElse(throw new HttpStatusException(404, "GET", url, None))
  }

  "Helios's list" should "publish each venue's town, street without the postcode Helios appends, and coordinates" in {
    Helios.parse(page(HeliosCinemas)).get("34b23726-7c53-483b-a0e5-13dfca6075ba") shouldBe
      Some(PublishedVenue("Legnica", Some("ul. Najświętszej Marii Panny 9"), Some(GeoPoint(51.210865, 16.163681))))
  }

  "Cinema City's list" should "publish each venue's town, street and coordinates" in {
    CinemaCity.parse(page(CinemaCityCinemas)).get("1100") shouldBe
      Some(PublishedVenue("Biała Podlaska", Some("ul. Brzeska 27"), Some(GeoPoint(52.03441, 23.12303))))
  }

  // The six ids the 2026-09-22 regional-hubs sweep wired off the showings
  // endpoint, before the list could be read: each is the town it was meant as.
  "Multikino's list" should "name the town of each venue, the six regional-hub ids among them" in {
    val listed = Multikino.parse(page(MultikinoCinemas))
    Seq("0039", "0038", "0051", "0048", "0044", "0031").map(id => listed.get(id).map(_.town)) shouldBe
      Seq("Pruszków", "Jaworzno", "Mielec", "Głogów", "Leszno", "Zgorzelec").map(Some(_))
    listed.get("0052").map(_.town) shouldBe Some("Warszawa G City Reduta")
  }

  "readDirectory" should "report a venue whose id the chain no longer lists as gone" in {
    val venue = AuditedVenue("leszno", "Multikino Leszno", "https://www.multikino.pl", Seq("Leszno"))
    val Right(readings) = RosterSourceReader.readDirectory(recorded, Today)(Multikino, Seq("0099" -> venue)): @unchecked
    readings.map(_._2) should matchPattern { case Seq(Gone(_)) => }
  }

  it should "note a list it could not read, once for all its venues, without failing the run" in {
    val blocked: String => FetchedPage = url => throw new HttpStatusException(403, "GET", url, None)
    val venues = Seq("0044", "0031").map(id => id -> AuditedVenue("leszno", s"M$id", "https://www.multikino.pl", Seq("Leszno")))
    RosterSourceReader.readDirectory(blocked, Today)(Multikino, venues) should matchPattern {
      case Left(DirectoryNotRead("Multikino", 2, _, false)) =>
    }
    DirectoryNotRead("Multikino", 2, "HTTP 403").failing shouldBe false
  }

  // The live run of 2026-09-23 found Cinema City Janki filed as a Warszawa
  // venue with no town of its own; the chain lists it in Janki.
  "every Polish chain venue" should "be listed by its chain, in the town we file it under and near its city page" in {
    val catalog = new CinemaScraperCatalog(new FakeHttpFetch("does-not-exist"), Today)
    val venues  = RosterSourceReader.chainVenuesOf(Country.Poland.cities, slug => catalog.byCity.getOrElse(slug, Nil))
    venues.map(_._1).distinct should contain theSameElementsAs ChainDirectory.all
    val readings = venues.groupMap(_._1)(v => v._2 -> v._3).toSeq.flatMap { case (directory, vs) =>
      RosterSourceReader.readDirectory(recorded, Today)(directory, vs).fold(e => fail(e.describe), identity)
    }
    RosterLocationAudit.findings(readings).map(_.describe) shouldBe empty
  }
}
