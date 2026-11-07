package clients.ticketsolve

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.{FakeHttpFetch, FixtureFile}
import models.{ChapterCardiff, NeuaddDwyforPwllheli, NerveCentreLondonderry, Number8Pershore, PocklingtonArtsCentre,
  PontardaweArtsCentre, TheatrColwyn, TheatrMwldanCardigan, WatershedBristol}
import services.cinemas.uk.TicketsolveClient

import java.time.{LocalDate, LocalDateTime}

/** Replays 2026-10-12 captures of every rostered Ticketsolve feed (`WriteTicketsolve --record`),
 *  `<client>.ticketsolve.com/shows.xml`: Mwldan, the Nerve Centre, Pocklington, Watershed,
 *  Chapter, Theatr Colwyn, Pontardawe, Neuadd Dwyfor and Number 8.
 *
 *  Theatr Mwldan's three screens and the Nerve Centre's cinema had no showtime from
 *  Flicks, their only source; both box offices publish the whole programme as XML. */
class TicketsolveClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today = LocalDate.of(2026, 10, 12)
  private def scrape(client: String, cinema: models.Cinema, on: LocalDate = today) =
    new TicketsolveClient(new FakeHttpFetch("ticketsolve"), client, cinema, on).fetch()

  private val mwldan  = scrape("mwldan", TheatrMwldanCardigan)
  private val byTitle = mwldan.map(cm => cm.movie.title -> cm).toMap

  "TicketsolveClient.fetch" should "read Theatr Mwldan's whole cinema programme off the feed" in {
    mwldan.size shouldBe 46
    mwldan.flatMap(_.showtimes).size shouldBe 238
    mwldan.map(_.cinema).toSet shouldBe Set(TheatrMwldanCardigan)
    mwldan.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 6, 12)
  }

  it should "read each screening's local time, screen and booking link, once though the feed repeats it per venue" in {
    val film = byTitle("Forgotten Island")
    film.showtimes.take(3).map(s => (s.dateTime, s.room.value)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 16, 18, 45) -> "Mwldan 1",
      LocalDateTime.of(2026, 10, 17, 13, 30) -> "Mwldan 1",
      LocalDateTime.of(2026, 10, 17, 16, 15) -> "Mwldan 1")
    film.showtimes.head.bookingUrl.value shouldBe "https://mwldan.ticketsolve.com/shows/873688734/events/129693858/seats"
    film.showtimes.map(_.dateTime).distinct should have size film.showtimes.size.toLong
    film.externalIds shouldBe Map("ticketsolve" -> "873688734")
    film.posterUrl.value should startWith ("https://d235gwso45fsgz.cloudfront.net/as-assets/variants/jn91ef78ydozznx28h3xb3gse2qo/")
  }

  it should "lift the certificate the box office appends to the title into the age rating" in {
    val film = byTitle("Forgotten Island")
    film.ageRating shouldBe Some("PG")
    film.movie.rawTitle shouldBe Some("Forgotten Island (PG)")
    // "(12A tbc)" is still a 12A.
    byTitle("Avengers: Doomsday").ageRating shouldBe Some("12A")
  }

  // Watershed tails the certificate with a short box-office code: "Fatherland (12A) S",
  // "(12A) PS", "(18) CTBA" — 18 of its shows. Left on, the title searched TMDB as "Fatherland (12A) S".
  it should "lift a certificate that a short box-office code follows" in {
    val watershed = scrape("watershed", WatershedBristol).map(cm => cm.movie.title -> cm).toMap
    watershed.get("Fatherland").value.ageRating shouldBe Some("12A")
    watershed.get("Golden Boy").value.ageRating shouldBe Some("18")
    watershed.keySet.filter(_.matches(""".*\((U|PG|12A?|15|18)\b.*""")) shouldBe empty
  }

  it should "keep the relays filed under a broadcast category, but not the gigs filed as live" in {
    // All 27 "Live Broadcast Events" shows are relays (NT Live, Royal Ballet & Opera, Met Opera,
    // Exhibition on Screen); Clwb Mwldan's club nights are filed under "Live TM / TM Byw".
    byTitle("Royal Ballet & Opera: Manon").ageRating shouldBe Some("15")
    byTitle.keySet should contain allOf ("National Theatre Live: Hay Fever", "Exhibition On Screen - Monet")
    mwldan.map(_.movie.title).filter(_.startsWith("Clwb Mwldan")) shouldBe empty
  }

  it should "keep the film society's screenings, filed under its acronym" in {
    // "TMFS / CFFTM" — Theatr Mwldan Film Society / Cymdeithas Ffilm — names no film word:
    // Pálfi's "Hen" (2026) and Hadi's "The President's Cake" (2025), one screening each.
    byTitle("Hen").showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 25, 18, 30))
    byTitle("Hen").ageRating shouldBe Some("15")
    byTitle.keySet should contain ("The President's Cake")
    // The negative control: its gigs ("Live TM / TM Byw") stay out.
    byTitle.keySet.filter(t => t.startsWith("Clwb Mwldan") || t.startsWith("Cardi Bach") || t.startsWith("Sandro Ford")) shouldBe empty
  }

  it should "drop screenings before today" in {
    val later = scrape("mwldan", TheatrMwldanCardigan, LocalDate.of(2026, 10, 17))
    later.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 17)
  }

  "A multi-arts Ticketsolve client" should "keep only what it files as film, not its gigs, comedy or film courses" in {
    val nerve = scrape("nervecentre", NerveCentreLondonderry)
    nerve.size shouldBe 14
    val titles = nerve.map(_.movie.title)
    titles should contain allOf ("Corpse Bride", "The Shining (Extended Cut)")
    titles should not contain "Bell X1 | Good Bones Tour 2026"
    // "BFI Film Academy" files online masterclasses, not screenings.
    titles.filter(_.contains("Masterclass")) shouldBe empty
    nerve.find(_.movie.title == "Corpse Bride").value.showtimes.map(s => (s.dateTime, s.room)) should contain (
      LocalDateTime.of(2026, 10, 30, 14, 0) -> Some("Cinema One"))
  }

  private def titles(client: String, cinema: models.Cinema) = scrape(client, cinema).map(_.movie.title).toSet

  "Each client's own filing" should "keep its films and relays, and none of its gigs, talks, workshops or seat sponsorships" in {
    val chapter = titles("chapter", ChapterCardiff)
    chapter should have size 40
    chapter should contain ("BFI London Film Festival 2026: The Debut")
    chapter should not contain "Drones Comedy Club"                            // "Events"
    chapter should not contain "Tara Clerkin Trio + support from ZIEGENKITZ"   // "Music"

    val pontardawe = titles("pontardaweartscentre", PontardaweArtsCentre)
    pontardawe should have size 20
    pontardawe should contain ("Kendal Mountain Film Festival 27")
    pontardawe should not contain "Lindisfarne"                                                         // "Music"
    pontardawe should not contain "Christmas Wreath Making Class with Master Florist Jonathan Lloyd-Davies" // "Workshops"

    val dwyfor = titles("neuadddwyfor", NeuaddDwyforPwllheli)
    dwyfor should have size 18
    dwyfor should contain ("NT Live: The Misanthrope") // "Darllediad Byw / Live Broadcast"
    dwyfor should not contain "TOM MEIGHAN - RAW 26"   // uncategorised, and no certificate
    dwyfor.filter(_.startsWith("Diod a Dylunio")) shouldBe empty // "Gweithdy / Workshop"

    val number8 = titles("number8", Number8Pershore)
    number8 should have size 29
    number8 should contain ("Royal Opera: Carmen")                      // "Event Cinema"
    number8 should not contain "King Pleasure and the Biscuit Boys"     // "Live Event"
    number8.filter(_.startsWith("Pop Up Painting")) shouldBe empty      // "Adult Workshops"
    number8.filter(_.startsWith("Number 8 Quiz")) shouldBe empty        // "Foyer Event"

    titles("pocklingtonartscentre", PocklingtonArtsCentre) should not contain "Seat Sponsorship" // uncategorised

    val watershed = titles("watershed", WatershedBristol)
    watershed.filter(_.startsWith("Golden Boy")) should not be empty           // "Event Cinema"
    watershed.filter(_.startsWith("Seat Sponsorship")) shouldBe empty          // "Seat Sponsorship"
    watershed should not contain "The Knot Makers"                             // "Lunchtime Talk"
    watershed should not contain "Make Shift - Come Unstuck Sessions"          // "Workshop"
  }

  it should "keep a relay filed as recorded live" in {
    // Theatr Colwyn files the National Theatre relay as "Recorded Live".
    titles("theatrcolwyn", TheatrColwyn) should contain ("NT Live: Golden Boy")
  }

  it should "not read a stage show as film for a film word used of its genre" in {
    // "Movie musicals": a touring song-and-dance show of film songs, with a live cast.
    val colwyn = titles("theatrcolwyn", TheatrColwyn)
    colwyn should not contain "Epic Movie Musicals"
    colwyn should contain ("Grease (Sing-a-long)") // the film itself, filed as "Film"
  }

  it should "keep what the box office reports to Comscore as film, whatever its category" in {
    // Theatr Colwyn tags its films and relays "reports_comscorefilm" (Neuadd Dwyfor: "comscore"),
    // the Disney Junior screenings under "Children & Family" and the Paris ballet relays under "Ballet".
    val colwyn = titles("theatrcolwyn", TheatrColwyn)
    colwyn should contain allOf ("Disney Junior Cinema Club 2026", "Opéra de Paris Ballet Season - La Bayadère")
    // Negative controls in the same categories, untagged: a live bubble show and a dance school's Nutcracker.
    colwyn should not contain "The Bubble Wizard's Giant Bubble Show"
    colwyn should not contain "Step One's The Nutcracker"
    colwyn should have size 10
  }

  it should "keep an uncategorised show that carries a film certificate" in {
    // Watershed leaves two previews uncategorised; its other uncategorised shows carry no certificate.
    val watershed = titles("watershed", WatershedBristol)
    watershed should contain allOf ("Preview: The Debut", "Preview: All Rivers Spill Their Stories to the Sea + director Q&A")
    watershed should not contain "Spooky Family Creative Technology Workshop"
    watershed should not contain "Supporters Evening 2026"
    watershed should not contain "Spike Island x Watershed: 2026 Film London Jarman Award"
    watershed should have size 57
  }

  "TicketsolveClient.parse" should "not read a venue's age limit on an uncategorised show as a film certificate" in {
    // Mwldan's own "Clwb Mwldan: Sonic Sounds (18+)" club night, as its recorded feed holds it (show 873691292), its
    // category blanked: no feed has billed an uncategorised one yet, and so filed it would be no film.
    val recorded = FixtureFile.read("test/resources/fixtures/ticketsolve/mwldan.ticketsolve.com/shows.xml")
    val from     = recorded.indexOf("<show id=\"873691292\">")
    val show     = recorded.substring(from, recorded.indexOf("</show>", from) + "</show>".length)
      .replaceAll("(?s)<event_category>.*?</event_category>", "<event_category></event_category>")
    val feed     = s"<venues><venue><shows>$show</shows></venue></venues>"
    TicketsolveClient.parse(feed, TheatrMwldanCardigan, today) shouldBe empty
    TicketsolveClient.parse(feed.replace("(18+)", "(18)"), TheatrMwldanCardigan, today).map(_.movie.title) shouldBe
      Seq("Clwb Mwldan: Sonic Sounds")
  }

  it should "refuse a page that is no feed rather than call it an empty programme" in {
    an[IllegalArgumentException] should be thrownBy
      TicketsolveClient.parse("<html><body>Maintenance</body></html>", TheatrMwldanCardigan, today)
  }
}
