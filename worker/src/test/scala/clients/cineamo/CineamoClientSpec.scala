package clients.cineamo

import clients.tools.{FakeHttpFetch, WriteCineamo}
import models.{CinemaMovie, GermanCinema}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import services.cinemas.common.{CineamoClient, ListingReads}
import services.identity.{CatalogueId, SourceReliability}
import tools.{GetOnlyHttpFetch, HttpFetch}

import java.io.IOException
import java.time.{LocalDate, LocalDateTime}
import scala.collection.mutable.ListBuffer

/** Replays two venues' real Cineamo programmes, every `/showings` page, recorded on
 *  2026-10-11 by [[WriteCineamo]] (see it for why each venue is there). */
class CineamoClientSpec extends AnyFlatSpec with Matchers {

  private val Today  = LocalDate.of(2026, 10, 11)
  private val venues = WriteCineamo.Venues.toMap

  /** Counts the pages asked for, and fails the ones `failing` names. */
  private class Counting(inner: HttpFetch, failing: Int => Boolean = _ => false) extends GetOnlyHttpFetch {
    val pages = ListBuffer.empty[Int]
    def get(url: String): String = {
      val page = url.split("page=")(1).toInt
      synchronized(pages += page)
      if (failing(page)) throw new IOException(s"page $page refused")
      inner.get(url)
    }
  }

  private def client(name: String, fetch: HttpFetch = new FakeHttpFetch("cineamo")) =
    new CineamoClient(fetch, venues(name), new GermanCinema(name, name), today = Today)

  private lazy val babylon    = client("Babylon (Mitte)").fetch()
  private lazy val dettelbach = client("Cineplex Cineworld Dettelbach").fetch()

  private def film(movies: Seq[CinemaMovie], title: String): CinemaMovie =
    movies.find(_.movie.title == title).getOrElse(fail(s"no '$title' in ${movies.map(_.movie.title)}"))

  // Babylon Mitte is why Germany chooses a source per venue: Filmstarts lists 23 of its
  // shows in the next fortnight, Cineamo 52 — and Cineamo's run to June 2027.
  "CineamoClient" should "read every page of Babylon Mitte's programme, eight months of it" in {
    val fetch  = new Counting(new FakeHttpFetch("cineamo"))
    val movies = client("Babylon (Mitte)", fetch).fetch()
    fetch.pages.toSet shouldBe (1 to 12).toSet
    movies.flatMap(_.showtimes) should have size 114
    movies.flatMap(_.showtimes).count(_.dateTime.toLocalDate.isBefore(Today.plusDays(14))) shouldBe 52
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 6, 12)
  }

  it should "fail the scrape when the first page cannot be read" in {
    an[IOException] should be thrownBy client("Babylon (Mitte)", new Counting(new FakeHttpFetch("cineamo"), _ == 1)).fetch()
  }

  it should "keep the pages that answered when a later one fails, and mark the listing incomplete" in {
    val (movies, reads) = ListingReads.during(client("Babylon (Mitte)", new Counting(new FakeHttpFetch("cineamo"), _ == 5)).fetch())
    movies.flatMap(_.showtimes) should have size 104
    reads.complete shouldBe false
  }

  // A venue sells its readings and slams through Cineamo beside its films (prod 2026-10-12: "Lesung mit Cornelia
  // Tomaschko", "Poetry Slam November"). The recorded Dettelbach programme has none, so its "Hope" is billed as two
  // such real titles: both go, the venue's films stay.
  it should "leave out a reading or a slam the venue sells beside its films" in {
    val retitled = new GetOnlyHttpFetch {
      private val real = new FakeHttpFetch("cineamo")
      def get(url: String): String =
        real.get(url).replace("\"name\": \"Hope\"", "\"name\": \"Lesung mit Cornelia Tomaschko\"")
          .replace("\"name\": \"Digger\"", "\"name\": \"Poetry Slam November\"")
    }
    val titles = client("Cineplex Cineworld Dettelbach", retitled).fetch().map(_.movie.title)
    titles should not contain "Lesung mit Cornelia Tomaschko"
    titles should not contain "Poetry Slam November"
    titles should contain ("Shaun das Schaf - Spuk im Kürbisfeld")
  }

  it should "throw on a body without the paging envelope rather than read a venue with nothing on" in {
    an[IllegalStateException] should be thrownBy CineamoClient.parsePage("""{"title":"Not Found","status":404}""")
  }

  it should "list the venue's own German title, Cineamo's English record as the original title" in {
    val shaun = film(dettelbach, "Shaun das Schaf - Spuk im Kürbisfeld")
    shaun.movie.originalTitle shouldBe Some("Shaun the Sheep: The Beast of Mossy Bottom")
    val rearWindow = film(babylon, "Hitchcock & Chabrol: Rear Window")
    rearWindow.movie.originalTitle shouldBe Some("Rear Window")
    rearWindow.movie.runtimeMinutes shouldBe Some(120)
    rearWindow.synopsis shouldBe None   // Cineamo's description is English
  }

  // `premiereDate` is the film's GERMAN premiere: Hitchcock's Rope (1948) premiered in Germany in 1963, Vertigo (1958)
  // in 1959, Rear Window (1954) in 1955. A year read off it denied the film. Only a premiere of the last seasons, billed
  // as no re-release, is the year of a film opening now.
  it should "read no year off an old German premiere, only off a film opening now" in {
    film(babylon, "Hitchcock & Chabrol: Rope").movie.releaseYear shouldBe None
    film(babylon, "Hitchcock & Chabrol: Vertigo").movie.releaseYear shouldBe None
    film(babylon, "Hitchcock & Chabrol: Rear Window").movie.releaseYear shouldBe None
    film(dettelbach, "Shaun das Schaf - Spuk im Kürbisfeld").movie.releaseYear shouldBe Some(2026)
  }

  // The film record is Cineamo's claim of which film the slot is, as a feed's.
  it should "file Cineamo's film record id as a feed's catalogue id" in {
    film(babylon, "Hitchcock & Chabrol: Rear Window").externalIds shouldBe Map("cineamo" -> "567m")
    SourceReliability.feedStated(Seq(CatalogueId(CineamoClient.CatalogueSource, "567m")), None) shouldBe true
  }

  it should "give each showing its German wall-clock start and its booking link" in {
    val first = film(babylon, "Hitchcock & Chabrol: Rear Window").showtimes.head
    first.dateTime shouldBe LocalDateTime.of(2026, 10, 11, 19, 45)   // "2026-10-11T17:45:00Z"
    first.bookingUrl shouldBe Some("https://www.kinoheld.de/cinema/berlin/babylon-berlin/show/64212/hitchcock-chabrol-rear-window")
  }

  it should "mark 3D, D-BOX and Atmos screenings as Filmstarts does" in {
    val formats = dettelbach.flatMap(_.showtimes).map(_.format)
    formats.count(_.contains("3D")) shouldBe 12
    formats.count(_.contains("DBOX")) shouldBe 15
    formats.count(_.contains("ATMOS")) shouldBe 22
  }

  // Astor Film Lounge HafenCity bills its Dolby Atmos screenings "ATMOS: Dune: Part Three" and "ATMOS/OmU: Verity -
  // Dunkle Geheimnisse": the format comes off the title as a tag, so the listing is the film's own title beside
  // its other screenings.
  it should "peel a screen format billed in front of the title into the showing's format" in {
    val astor = client("Astor Film Lounge HafenCity").fetch()
    astor.map(_.movie.title).filter(_.toUpperCase.startsWith("ATMOS")) shouldBe empty
    val dune = film(astor, "Dune: Part Three")
    dune.showtimes.count(_.format.contains("ATMOS")) should be > 0
    film(astor, "Verity - Dunkle Geheimnisse").showtimes.flatMap(_.format).toSet should contain allOf ("ATMOS", "OmU")
  }

  "CineamoClient.formatTokens" should "mark the language version: OmU, OmeU, OV, or nothing for German and dubs" in {
    def tokens(json: String) = CineamoClient.formatTokens(Json.parse(json))
    tokens("""{"isSubtitled":true,"subtitledLanguage":"deu"}""") shouldBe List("OmU")
    tokens("""{"isSubtitled":true,"subtitledLanguage":"eng"}""") shouldBe List("OmeU")
    tokens("""{"isOriginalLanguage":true,"originalLanguage":"eng"}""") shouldBe List("OV")
    tokens("""{"isOriginalLanguage":true,"originalLanguage":"deu"}""") shouldBe Nil
    tokens("""{"isOriginalLanguage":false,"originalLanguage":"eng","isImax":true}""") shouldBe List("IMAX")
  }
}
