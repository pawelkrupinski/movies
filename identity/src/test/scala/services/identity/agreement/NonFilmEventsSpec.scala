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
      "Koncert jazzowy połączony z degustacją wina"  -> "live event",
      "Halloweenowy Maraton Horrorów"               -> "marathon",
      "Rawska \"Noc horrorów\"-edycja 2026"         -> "marathon",
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
      "Unrestricted View Horror Film Festival 2026: Opening Night" -> "programme slot",
      // the feeds' own language-neutral filter (DE Kino in der Lagerhalle, capture #232: no 2007 "Schöne Aussicht")
      "Schöne Aussicht - Die Slam Poetry & Songwriting Show - mit The Lake and the Wolf und Andreas Weber" -> "non-screening",
      "Melanie Raabe liest aus: Daisy"              -> "non-screening",
      // a reading billed first, the fields piped (DE Museum-Lichtspiele Tübingen, recording 3166: Wikidata asked for "Broder")
      "Lesung und Gespräch | Dörte Hansen moderiert von Ingrid Abeln | Broder" -> "non-screening"
    ).foreach { case (title, why) => withClue(title) { NonFilmEvents.of(titled(title)) shouldBe Some(why) } }
  }

  it should "read a marathon by its genre or its night, never by one circuit's series name" in {
    // Alamo Drafthouse's "Dismember the Alamo" had a marker of its own; the model reads its city piece as the
    // venue's (CandidateScoring.namesOnlyItsVenue) and its year as the edition's, as any venue's own series
    NonFilmEvents.of(titled("Dismember the Alamo 2026 - Brooklyn")) shouldBe None
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
      "DYRYGENT - POŁĄCZONY Z KONCERTEM MUZYKI NA ŻYWO",          // Wajda's film, a concert joined to its screening
      "Maraton",                                                 // a bare word may be a film's name
      "Tango"
    ).foreach(title => withClue(title) { NonFilmEvents.of(titled(title)) shouldBe None })
  }

  it should "be a live show when it bills a performer live, at the venue, or a theatre company's staging" in {
    // DE, recording 38355584482: each took the film one piece of its title names — a 1999 short "Ausverkauft!" by the
    // sold-out tag, the 1951 "Follow the Sun" by the show's name, the 2019 film of the play the company stages
    def at(venue: String, title: String) = titled(title).copy(key = services.movies.ListingKey.Published(venue, title, None, Nil))
    NonFilmEvents.of(at("Apollo Hannover", "Jan Logemann LIVE: Herr Logemann hebt ab! -AUSVERKAUFT!-")) shouldBe Some("live show")
    NonFilmEvents.of(at("Apollo Hannover", "Jan Preuß LIVE")) shouldBe Some("live show")
    NonFilmEvents.of(at("Beluga Kino", "HELMUT ZIERL - live im Beluga Kino - FOLLOW THE SUN - Der Sommer meines Lebens")) shouldBe Some("live show")
    NonFilmEvents.of(at("Kino in der Lagerhalle", "Gut gegen Nordwind - Thiele-Neumann-Theater")) shouldBe Some("theatre company")
    // what stays cinema: a concert relayed "live im Kino" names no venue, a house's relay and live music to a film are
    // the screening's, a performer's concert film is billed "live in/at", and a cinema's own name is no company's
    Seq("Helene Fischer - Live im Kino", "RBO Live 2026/27: Götterdämmerung (Royal Opera)", "NT Live: All My Sons",
      "Stray Kids World Tour <RUN IT> LIVE VIEWING", "Hungarian Rhapsody: Queen - Live In Budapest", "SPECIAL: Music for Cinemas: NADJA Live Score",
      "STUMMFILM-PROGRAMM: ZIRKUS UND JAHRMARKT MIT LIVE-MUSIK", "Das Boot - Filmtheater", "Live and Let Die", "Live by Night",
      "Theater of Blood").foreach(title => withClue(title) { NonFilmEvents.of(at("Beluga Kino", title)) shouldBe None })
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
