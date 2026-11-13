package clients.savoy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{CinemaMovie, Movie, UkRoster}
import services.cinemas.common.VenueCategory
import services.cinemas.uk.SavoyClient

import java.time.LocalDate

/** A mixed venue's live shows — the tribute acts, pantos, plays and talks a town's cinema sells beside its films —
 *  read off the category each venue files them under, replayed from 2026-11-13 captures: Regal Stowmarket's `WhatsOn`
 *  literal (its `TypeDescription`), The Beehive Honiton's and The Poly Falmouth's box offices with their category tabs.
 *  Four of them the precision sample found taking a film (2026-11-13): the stage "Pinocchio", a Garth Brooks tribute, a
 *  community theatre's "Little Mermaid", a live talk. */
class SavoyLiveShowsSpec extends AnyFlatSpec with Matchers {

  private val today = LocalDate.of(2026, 11, 13)
  private val http  = new FakeHttpFetch("savoy")

  private def titles(movies: Seq[CinemaMovie]) = movies.map(_.movie.title).toSet

  private lazy val regal = titles(new SavoyClient(http, "https://regalstowmarket.co.uk/RegalStowmarket.dll/",
    UkRoster("RegalStowmarket"), today).fetch())
  private lazy val beehive = titles(new SavoyClient(http, "http://thebeehive.savoysystems.co.uk/TheBeehive.dll/",
    UkRoster("TheBeehiveHoniton"), today, SavoyClient.BoxOffice).fetch())
  private lazy val poly = titles(new SavoyClient(http, "http://thepoly.savoysystems.co.uk/ThePoly.dll/",
    UkRoster("ThePolyFalmouth"), today, SavoyClient.BoxOffice).fetch())

  "SavoyClient" should "drop the live shows a venue files under 'Live Music' or 'Theatre', keeping its screenings and relays" in {
    regal should contain noneOf ("Sods Presents: Pinocchio", "Blame It All On My Roots", "ABBA Sensation", "The Bootleg Bee Gees",
      "Chris Drew: An Evening of Mediumship", "The Secret Elephant", "The Arendelle Tea Party")
    // filed "Theatre On Screen", "Concerts", "Musicals", "Exhibition On Screen", "Kids Event Cinema": screened, whatever is staged
    regal should contain allOf ("NTLive: Golden Boy", "GIANT- The Play", "André Rieu- Let It Snow",
      "Andrew Lloyd Webber's Starlight Express- Live On Stage", "Monet", "Festive Fun with Peppa Cinema Experience",
      "Royal Ballet & Opera- Swan Lake", "The Metropolitan Opera- Parsifal", "Dune: Part Three", "RX Family- Home Alone")
    regal.size shouldBe 50 // of its 69 events: the 19 it files under "Live Music", "Theatre", "Kids Shows", "Cafe Events" and "Interviews and Talks" go
  }

  it should "drop the live shows a box office's category tabs file, keeping what its film tab lists" in {
    beehive should contain noneOf ("HCTC Presents: \"The Little Mermaid\"", "Mick Jogger & The Stones Experience", "This Is Floyd (26)",
      "A Very Dickensian Murder")
    beehive should contain allOf ("NTL: Hay Fever", "RBO: Tosca 27", "The Met: Otello 27", "Andre Rieu's Let it Snow 26",
      "Andrew Lloyd Webbers Starlight Express - Filmed Live On Stage", "Exhibition On Screen: Monet", "Pressure",
      "Nostalgic Cinema: The Princess Bride")
    poly should contain noneOf ("Beneath The Tracksuit: A Way With Words", "Starlite Presents: Shrek JR", "Martin Harley",
      "An Evening with Jack the Ripper")
    poly should contain allOf ("Sense and Sensibility", "It's a Wonderful Life (1946)", "Kendal Mountain Festival Live Tour 2027",
      "Guardian & Baby Screening: You Can See Everything")
  }

  private def listing(title: String, year: Option[Int] = None, director: Seq[String] = Nil, synopsis: Option[String] = None) =
    CinemaMovie(Movie(title, releaseYear = year), UkRoster("RegalStowmarket"), None, None, synopsis, Nil, director, Nil)

  "VenueCategory.filedAsLiveShow" should "read only a live performance's own category, never a relay's or a film genre's" in {
    VenueCategory.filedAsLiveShow(Seq("Live Music"), listing("ABBA Sensation")) shouldBe true
    VenueCategory.filedAsLiveShow(Seq("On Stage"), listing("Sleeping Beauty")) shouldBe true
    // Keswick Alhambra's relays, The Odyssey's films, Regal Stowmarket's André Rieu
    Seq("Live / Event", "Live Event", "Live Arts", "Comedy", "Concerts", "Musicals", "Theatre On Screen").foreach { category =>
      VenueCategory.filedAsLiveShow(Seq(category), listing("Royal Ballet 2026/7: Swan Lake")) shouldBe false
    }
    // filed under a stage's tab and the screen's too
    VenueCategory.filedAsLiveShow(Seq("Theatre", "Cinema"), listing("Hadestown")) shouldBe false
  }

  it should "keep a screening its venue files as theatre: a film's facts, a broadcast's name, a capture's synopsis" in {
    VenueCategory.filedAsLiveShow(Seq("Theatre"), listing("GIANT - The Play", year = Some(2025), director = Seq("Nicholas Hytner"))) shouldBe false
    VenueCategory.filedAsLiveShow(Seq("Theatre"), listing("National Theatre Live: Golden Boy")) shouldBe false
    // Empire Theatre Halstead, 2026-11-13: its "Theatre" files the Starlight Express capture beside the Stane Street Players' panto
    VenueCategory.filedAsLiveShow(Seq("Theatre"), listing("Starlight Express",
      synopsis = Some("Filmed live at the specially designed Starlight Auditorium at Troubadour Wembley Park Theatre"))) shouldBe false
    VenueCategory.filedAsLiveShow(Seq("Theatre"), listing("Alice in Wonderland",
      synopsis = Some("The pantomime everyone's going mad for! Award-winning drama group Stane Street Players returns"))) shouldBe true
  }
}
