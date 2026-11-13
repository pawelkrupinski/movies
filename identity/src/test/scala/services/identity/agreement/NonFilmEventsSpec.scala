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

  it should "be an event when it bills a series' episodes, a franchise marathon, a gaming session or a panto" in {
    // the 2026-11-13 corpus audit's film-less events, each left unmatched and counted as a miss
    Map(
      "Heated Rivalry: Season 01 - Epi 05 & 06"      -> "tv episodes",
      "Heated Rivalry: Season 1 Marathon"            -> "tv episodes",
      "Maraton Diuny"                                -> "franchise marathon",
      "Maraton Saga Zmierzch"                        -> "franchise marathon",
      "The Hunger Games Marathon"                    -> "franchise marathon",
      "Gaming im Kino - 3 Stunden"                   -> "gaming",
      "Gaming im Lumos"                              -> "gaming",
      "CBeebies Panto 2026: Treasure Island"         -> "panto",
      "The CBeebies Panto 2026: Treasure Island"     -> "panto",
      "CBeebies Panto Treasure Island – Sensory Friendly Screening" -> "panto"
    ).foreach { case (title, why) => withClue(title) { NonFilmEvents.of(titled(title)) shouldBe Some(why) } }
    // one film's marathon screening (judged right), a house's season, a series' film
    Seq("Inna Mamusia - maraton", "RBO Cinema Season 2026-27: La Fanciulla Del West", "Marathon Man", "Świąteczna zabawa z Peppą")
      .foreach(title => withClue(title) { NonFilmEvents.of(titled(title)) shouldBe None })
  }

  "an event's billing" should "state its edition, episodes and session length as facts" in {
    def facts(title: String) = NonFilmEvents.facts(titled(title))
    facts("League of Legends Worlds 26 | Finals in Cinema") shouldBe
      Some(NonFilmEvents.Facts("esports", Some(2026), None, None, Seq("league", "of", "legends", "worlds", "finals", "in", "cinema")))
    facts("League of Legends Worlds 2026 - Finals in Cinema").map(_.edition) shouldBe Some(Some(2026))
    // DE: "Worlds 2026" and "Worlds 26" billed apart as two films
    facts("League of Legends Worlds 2026 - Finals in Cinema").map(_.name) shouldBe facts("League of Legends Worlds 26 - Finals in Cinema").map(_.name)
    facts("CBeebies Panto 2026: Treasure Island") shouldBe Some(NonFilmEvents.Facts("panto", Some(2026), None, None, Seq("cbeebies", "panto", "treasure", "island")))
    facts("The CBeebies Panto 2026: Treasure Island").map(_.name) shouldBe facts("Cbeebies Panto: Treasure Island").map(_.name)
    facts("Heated Rivalry: Season 01 - Epi 05 & 06").flatMap(_.episodes) shouldBe Some("s1e5-6")
    facts("Heated Rivalry: Season 1 Marathon").flatMap(_.episodes) shouldBe Some("s1")
    facts("Gaming im Kino - 3 Stunden").flatMap(_.minutes) shouldBe Some(180)
    facts("Interstellar") shouldBe None
  }

  it should "tell two events apart only by a fact both state otherwise" in {
    def facts(title: String) = NonFilmEvents.facts(titled(title)).get
    facts("CBeebies Panto 2026: Treasure Island").apartFrom(facts("Cbeebies Panto: Treasure Island")) shouldBe false
    facts("League of Legends Worlds 26 | Finals in Cinema").apartFrom(facts("League Of Legends Worlds 26 - Finals In Cinema")) shouldBe false
    facts("League of Legends Worlds 26 | Finals in Cinema").apartFrom(facts("League of Legends Worlds 2025 - Finals in Cinema")) shouldBe true
    facts("Heated Rivalry: Season 01 - Epi 01 & 02").apartFrom(facts("Heated Rivalry: Season 01 - Epi 03 & 04")) shouldBe true
    facts("Heated Rivalry: Season 1 Marathon").apartFrom(facts("Heated Rivalry: Season 01 - Epi 03 & 04")) shouldBe true
    facts("Gaming im Kino - 3 Stunden").apartFrom(facts("Gaming im Kino - 4 Stunden")) shouldBe true
  }

  it should "be film-less only of a kind no film record is: never a live show or a concert, which TMDB may hold" in {
    NonFilmEvents.facts(titled("League of Legends Worlds 26 | Finals in Cinema")).map(_.filmless) shouldBe Some(true)
    NonFilmEvents.facts(titled("Secret Screening - 12th October")).map(_.filmless) shouldBe Some(true)
    // UK what-if 2026-11-13: tmdb 1711927 at Kavanagh Cinema Herne Bay, prod's take
    NonFilmEvents.facts(titled("Berliner Philharmoniker LIVE: New Year’s Eve Concert 2026")).exists(_.filmless) shouldBe false
    NonFilmEvents.facts(titled("Koncert: Leszek Możdżer - solo")).map(_.filmless) shouldBe Some(false)
  }

  it should "be one production at many venues only of a kind billed alike everywhere, never a venue's own night" in {
    NonFilmEvents.facts(titled("League of Legends Worlds 26 | Finals in Cinema")).map(_.distributed) shouldBe Some(true)
    NonFilmEvents.facts(titled("CBeebies Panto 2026: Treasure Island")).map(_.distributed) shouldBe Some(true)
    // FilmScheduleEndToEndSpec, 2026-11-13: two venues' "Maraton Horrorów" joined as one film when it was
    NonFilmEvents.facts(titled("Maraton Horrorów")).map(_.distributed) shouldBe Some(false)
    NonFilmEvents.facts(titled("Secret Screening - 12th October")).map(_.distributed) shouldBe Some(false)
  }

  "a cluster" should "be an event only when every listing of it is" in {
    NonFilmEvents.of(Seq(titled("Maraton Halloween"), titled("MARATON HALLOWEEN"))) shouldBe Some("marathon")
    NonFilmEvents.of(Seq(titled("Maraton Halloween"), titled("Halloween"))) shouldBe None
    NonFilmEvents.of(Nil) shouldBe None
  }
}
