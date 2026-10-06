package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{FilmTable, Listing}

/** [[NonFilmEvents]] on the titles that decided its rules: the events the unmatched clusters bill, and the films and
 *  relays whose titles read like one. */
class NonFilmEventsSpec extends AnyFlatSpec with Matchers {

  private def titled(title: String): Listing = FilmTable.listing(KinoMuza, title)

  "a listing" should "be an event when its title bills one no film database holds" in {
    Map(
      "Koncert: Leszek Możdżer - solo"              -> "live event",
      "Spektakl teatralny \" brzydkie kaczątko \""  -> "live event",
      "Halloweenowy Maraton Horrorów"               -> "marathon",
      "Rawska \"Noc horrorów\"-edycja 2026"         -> "marathon",
      "Dismember the Alamo 2026 - Brooklyn"         -> "marathon",
      "Secret Screening - 12th October"             -> "mystery screening",
      "Scream Unseen (20 October)"                  -> "mystery screening",
      "$5 Secret Movie 10/5/26"                     -> "mystery screening",
      "League of Legends Worlds 26 | Finals in Cinema" -> "esports",
      "Fiu fiu milonga potancówka tangowa"          -> "class",
      "MEDYTACJE/NEUROJOGA"                         -> "class",
      "Slajdy podróżnicze: Francja południowa"      -> "class",
      "12. Splat!FilmFest | Karnet"                 -> "pass",
      "Keine Vorstellung(en)"                       -> "no screening",
      "Dziś nie gramy, wybierz inną datę."          -> "no screening",
      "Screen Hire - 2 Hours"                       -> "no screening",
      "Unrestricted View Horror Film Festival 2026: Opening Night" -> "programme slot"
    ).foreach { case (title, why) => withClue(title) { NonFilmEvents.of(titled(title)) shouldBe Some(why) } }
  }

  it should "not be one when it names a film beside the event, or a relay of cinema content" in {
    Seq(
      "Reporterzy wolności - film i spotkanie z twórcami",       // a talk beside its film
      "SKRZYŻOWANIE - 50 na 51 - spotkanie z Janem Englertem",   // the film, its guest beside it
      "Okładka „Short Stories”",                                 // a venue's caption on a film
      "11. UFF - Teatr weteranów",                               // a festival's documentary
      "Balet z Opery Paryskiej 2026-2027: Bajadera",             // a house's season relay
      "ReTransmisje Met: Na żywo w HD - Così fan tutte",         // the Met's
      "Hauser symfonicznie z Royal Albert Hall",                 // a concert film
      "André Rieu: Niech żyje Maastricht. Letni koncert 2026",   // event cinema
      "Maraton",                                                 // a bare word may be a film's name
      "Tango"
    ).foreach(title => withClue(title) { NonFilmEvents.of(titled(title)) shouldBe None })
  }

  it should "not be one when it states a film's record — a director and a year — whatever its title says" in {
    NonFilmEvents.of(titled("Koncert").copy(directors = Seq("Michał Rosa"), year = Some(2009))) shouldBe None
    NonFilmEvents.of(titled("Koncert").copy(year = Some(2009))) shouldBe Some("live event")
  }

  "a cluster" should "be an event only when every listing of it is" in {
    NonFilmEvents.of(Seq(titled("Maraton Halloween"), titled("MARATON HALLOWEEN"))) shouldBe Some("marathon")
    NonFilmEvents.of(Seq(titled("Maraton Halloween"), titled("Halloween"))) shouldBe None
    NonFilmEvents.of(Nil) shouldBe None
  }
}
