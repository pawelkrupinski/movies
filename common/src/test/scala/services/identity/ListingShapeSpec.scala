package services.identity

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The one reading of a listing's shape every rule shares ([[ListingShape]]): the predicates the agreement, posters,
 *  catalogue take, broadcast join and fill each used to define for themselves. */
class ListingShapeSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  "a listing's facts" should "be its venue's own unless a feed states them or it links a listings site's catalogue entry" in {
    val own = listing(KinoMuza, "NT Live: All My Sons", year = Some(2026))
    ListingShape.venueStated(own) shouldBe true
    // Flicks' film page linked as its page: the entry's claim, read onto it or not
    ListingShape.venueStated(own.copy(page = Some("https://www.flicks.us/movie/national-theatre-live-all-my-sons/"))) shouldBe false
    ListingShape.venueStated(own.copy(page = Some("https://www.flicks.us/movie/national-theatre-live-all-my-sons/"), pageFacts = true)) shouldBe false
    // a showtimes feed's catalogue id
    ListingShape.venueStated(own.copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Webedia.source, "227420")))) shouldBe false
    // the venue's own page
    ListingShape.venueStated(own.copy(page = Some("https://kinomuza.pl/film/all-my-sons"))) shouldBe true
  }

  "a listing's backer in its title group" should "be its venue's name itself for the venue's own claim, a feed's entry joined to it" in {
    // held for every listing for the corpus's life: a venue's own claim costs no string of its own
    val own = listing(KinoMuza, "Karla")
    IdentityMeasures.Backer(own.venue, CatalogueSources.factSource(own)) should be theSameInstanceAs own.venue
    // one feed entry listed at many venues is one source, its venue kept beside it
    val fed    = own.copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Webedia.source, "1000023315")))
    val backer = IdentityMeasures.Backer(fed.venue, CatalogueSources.factSource(fed))
    IdentityMeasures.Backer.venue(backer) shouldBe own.venue
    IdentityMeasures.Backer.source(backer) shouldBe CatalogueId(CatalogueSources.Webedia.source, "1000023315").key
    IdentityMeasures.Backer.source(IdentityMeasures.Backer(own.venue, CatalogueSources.factSource(own))) shouldBe CatalogueSources.factSource(own)
  }

  "a programme slot" should "be a festival's or season's banner and a slot of its programme, nothing else" in {
    ListingShape.programmeSlotOf("Unrestricted View Horror Film Festival 2026: Opening Night") shouldBe Some("Opening Night")
    ListingShape.programmeSlotOf("Festiwal Filmowy Kino Lato 2026: Gala otwarcia") shouldBe Some("Gala otwarcia")
    ListingShape.programmeSlotOf("Fantasy Filmfest 2026 - Überraschungsfilm") shouldBe Some("Überraschungsfilm")
    ListingShape.programmeSlotOf("Semana de Cine Fantástico | Sesión inaugural") shouldBe Some("Sesión inaugural")
    // a film's own title beside the slot is that film's screening
    ListingShape.programmeSlotOf("Ars Independent Festival 2026: Gala otwarcia + \"Czarna godzina\"") shouldBe None
    // the bare slot is a film's title (Cassavetes' Opening Night), and a date is no festival's banner
    ListingShape.programmeSlotOf("Opening Night") shouldBe None
    ListingShape.programmeSlotOf("Secret Screening - 5th October") shouldBe None
    // a work under a festival's banner is the work
    ListingShape.programmeSlotOf("Splat!FilmFest: The Taxidermist") shouldBe None
  }

  "a piece billed beside the film" should "be a subtitle, performer or show of its own words, never the venue's wording around a film" in {
    def film(title: String, original: Option[String] = None, directors: Seq[String] = Nil) =
      IdentityMeasures.Film(title, original, directors = Some(directors))
    def beside(raw: String, films: IdentityMeasures.Film*) = ListingShape.billedBeside(listing(KinoMuza, raw), films)
    // the identity-unmatched capture's agreed wrong takes (2026-10-13): a documentary's subtitle, a dance show, a comedian,
    // a programme's piece before the film, an event named like a film, a concert line-up
    beside("Femme - Frauen heilen die Welt", film("Femme")) shouldBe defined
    beside("Dave - Music in MotionTBA", film("Dave")) shouldBe defined
    beside("Athanor - Startblock", film("Athanor")) shouldBe defined
    beside("SKRZYŻOWANIE - 50 na 51 - spotkanie z Janem Englertem", film("Skrzyżowanie")) shouldBe None
    beside("Giorgia Fumo - OUT OF OFFICE", film("Out of Office")) shouldBe defined
    beside("FBF: Freud and Gale: Making It Up", film("Making It Up")) shouldBe defined
    beside("Kids Club: CBeebies Panto 2026: Treasure Island", film("Treasure Island")) shouldBe defined
    beside("The Tea Alchemist - World Premiere + Cast & Crew Q&A OmeU, keine Angabe", film("World Premiere")) shouldBe defined
    beside("Wohnzimmerkonzerte Festival - Toi Et Moi | Ronja Maltzahn | Finn Moriz | Flora Falls", film("Du und ich", Some("Toi et moi"))) shouldBe
      defined
    // the venue's own wording: a banner in its slot, an event or edition, a programme, a note, the film's director or quotes
    beside("Somali Film Festival: Lifeline", film("Lifeline")) shouldBe None
    // a strand's banner a colon closes, a screening programme among its pieces, all of it — DE Filmrauschpalast's numbered
    // Hong Kong strand before Fruit Chan's film (capture 2026-10-13)
    beside("SPECIAL: 2. Dangerous Encounters – Hong Kong im Filmrausch: MADE IN HONG KONG", film("Made in Hong Kong", Some("Xiang Gang Zhi Zao"))) shouldBe
      None
    beside("IT FOLLOWS - HALLOWEEN SPECIAL", film("It Follows")) shouldBe None
    beside("Fatherland - Film Club", film("Fatherland")) shouldBe None
    beside("DKF Szpula – DRUGIE ŻYCIE", film("Drugie życie")) shouldBe None
    beside("Dolina Issy | Poniedziałki z Konwickim: pisarz – scenarzysta – reżyser", film("Dolina Issy", directors = Seq("Tadeusz Konwicki"))) shouldBe
      None
    beside("KÖNIG DROSSELBART - EIN ADVENTSMÄRCHEN FÜR KINDER", film("König Drosselbart")) shouldBe None
    beside("Geliefert (ver.di)", film("Geliefert")) shouldBe None
    beside("Digger (15) 129 mins", film("Digger")) shouldBe None
    beside("DAS LEHRERZIMMER - ABO 9", film("Das Lehrerzimmer")) shouldBe None
    beside("Judas and the Black Messiah + Panel Discussion", film("Judas and the Black Messiah")) shouldBe None
    beside("Wicker (15) 105 mins *MATINEE*", film("Wicker")) shouldBe None
    beside("Zaproszenie | Zobacz przedpremierowo", film("Zaproszenie")) shouldBe None
    beside("Hilfe, es weihnachtet sehr - DELUXE ABEND", film("Schöne Bescherung").copy(alternativeTitles = Seq("Hilfe, es Weihnachtet sehr"))) shouldBe
      None
    beside("Klątwa doliny węży – z autorską narracją Łony (pokaz jednorazowy)", film("Klątwa doliny węży")) shouldBe None
    beside("Olympia 1936 / 2026 - Leni Riefenstahl: OLYMPIA 1 - FEST DER VÖLKER", film("Olympia - Fest der Völker", directors = Seq("Leni Riefenstahl"))) shouldBe
      None
    beside("Festiwal Korelacje: „Zemsta” z autorską narracją Artura Andrusa", film("Zemsta")) shouldBe None
    beside("Dni Kina Niemego - 100 lat temu: SYMFONIA ZMYSŁÓW", film("Flesh and the Devil").copy(alternativeTitles = Seq("Symfonia zmysłów"))) shouldBe None
    // the listing's own original title beside its title, and a billing the film's title names no whole piece of
    ListingShape.billedBeside(listing(KinoMuza, "Hacer Mucho con Poco - Do More With Less").copy(originalTitle = Some("Do More With Less")),
      Seq(film("Hacer Mucho con Poco"))) shouldBe None
    beside("Johnny English Strikes Again", film("Johnny English")) shouldBe None
  }

  "a relay" should "be told by its stage work, its house or season, or a concert film's venue" in {
    ListingShape.stagesAWork(listing(KinoMuza, "OPERA-COSI FAN TUTTE")) shouldBe true
    // ES mk2's ballet relay, billed bare: Adam's ballet, which Wikidata files under "ballet" (Q15079786), a class the
    // generated table never asks for — not Allégret's unfinished 1939 film the families agreed on (taquilla's "(Cine)" tag
    // the normaliser strips)
    ListingShape.stagesAWork(listing(KinoMuza, "Le Corsaire (Cine)").copy(cleanTitle = "Le Corsaire")) shouldBe true
    ListingShape.billsAHouse(listing(KinoMuza, "OPERA-MAKBET - retransmisja")) shouldBe true
    ListingShape.billsAHouse(listing(KinoMuza, "Klondike")) shouldBe false
    val concert = listing(KinoMuza, "Hauser symfonicznie z Royal Albert Hall")
    ListingShape.relays(concert, concert.rawTitle.toLowerCase(java.util.Locale.ROOT)) shouldBe true
    val film = listing(KinoMuza, "Klondike")
    ListingShape.relays(film, film.rawTitle.toLowerCase(java.util.Locale.ROOT)) shouldBe false
  }
}
