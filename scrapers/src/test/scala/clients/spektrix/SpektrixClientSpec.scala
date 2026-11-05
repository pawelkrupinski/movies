package clients.spektrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{BreweryArtsCentreKendal, BroadwayCinemaNottingham, DundeeContemporaryArtsDCA, GalaCinemaDurham, HydeParkPictureHouse, InstituteOfContemporaryArts, LighthousePoole, OldMarketHallShrewsbury, TauntonBrewhouse, TheDukesLancaster}
import play.api.libs.json.{JsObject, Json}
import services.cinemas.uk.SpektrixClient

import java.time.{LocalDate, LocalDateTime}

/** Replays 2026-10-12 captures of the ICA's public Spektrix v3 API (`WriteSpektrix`):
 *  `system.spektrix.com/ica/api/v3/events?instanceStart_from=2026-10-12` and
 *  `…/instances?startFrom=2026-10-12`.
 *
 *  The ICA had never had a showtime from Flicks, its only source, though its own box
 *  office sells the cinema two months ahead beside gigs, talks and exhibitions. */
class SpektrixClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today   = LocalDate.of(2026, 10, 12)
  private val results = new SpektrixClient(new FakeHttpFetch("spektrix"), "ica", InstituteOfContemporaryArts, today).fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "SpektrixClient.fetch" should "read the ICA's cinema programme Flicks never listed" in {
    results.size shouldBe 122
    results.flatMap(_.showtimes).size shouldBe 209
    results.map(_.cinema).toSet shouldBe Set(InstituteOfContemporaryArts)
    results.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2026, 12, 17)
  }

  it should "join each performance to its event, with the screen the box office names" in {
    val film = byTitle("Case 137")
    film.movie.runtimeMinutes shouldBe Some(115)
    film.showtimes.map(s => (s.dateTime, s.room.value)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 16, 20, 30) -> "Cinema 2",
      LocalDateTime.of(2026, 10, 17, 18, 30) -> "Cinema 2",
      LocalDateTime.of(2026, 10, 18, 18, 0)  -> "Cinema 2",
      LocalDateTime.of(2026, 10, 20, 18, 30) -> "Cinema 1",
      LocalDateTime.of(2026, 10, 21, 17, 30) -> "Cinema 1",
      LocalDateTime.of(2026, 10, 22, 20, 20) -> "Cinema 2")
    film.externalIds shouldBe Map("spektrix" -> "787201AVMBLKGGCTLQMLSMRGKKRJGKMGS")
  }

  it should "keep only what the box office files as film, not its gigs, talks or exhibition slots" in {
    // ICA files them under `attribute_Category`: Films / Live / Talks & Engagement / Exhibitions.
    byTitle.keySet should contain ("Barrio Triste")
    byTitle.keySet should not contain "Zukovstheworld"
    byTitle.keySet should not contain "Alabaster dePlume"
    byTitle.keySet.filter(_.startsWith("Autumn Knight")) shouldBe empty
    byTitle.keySet.filter(_.startsWith("Book Launch + Talk")) shouldBe empty
  }

  // 2026-10-12 capture of `galadurham`, which sells the Gala in Durham, the Consett
  // Empire and Bishop Auckland Town Hall through one client.
  private def gala(venue: String) =
    new SpektrixClient(new FakeHttpFetch("spektrix"), "galadurham", GalaCinemaDurham, today, Some(venue)).fetch()

  "A multi-venue Spektrix client" should "list only the building the venue names" in {
    gala("Gala Theatre & Cinema").flatMap(_.showtimes) should have size 33
    gala("Consett Empire").flatMap(_.showtimes) should have size 23
  }

  it should "take the box office's building tag off the title, where it read as a gala night" in {
    // "Verity (15) (Gala)" with `attribute_Sitename: "Gala"`: the live-event filter's
    // `gala` marker dropped every film billed so until the tag came off.
    val films = gala("Gala Theatre & Cinema").map(cm => cm.movie.title -> cm).toMap
    films("Verity (15)").showtimes should have size 5
    films("Verity (15)").movie.rawTitle shouldBe Some("Verity (15) (Gala)")
    films.keySet.filter(_.contains("(Gala)")) shouldBe empty
  }

  "SpektrixClient on Broadway Nottingham" should "read a boolean film filing and the film's credits off its attributes" in {
    // 2026-10-12 capture of `broadwaynottingham`: films carry `attribute_Film: true`, the
    // opera and ballet relays `attribute_EventCinema: true`, and no category text at all.
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "broadwaynottingham", BroadwayCinemaNottingham, today)
      .fetch().map(cm => cm.movie.title -> cm).toMap
    films.values.flatMap(_.showtimes) should have size 305
    films.keySet should contain ("Met Opera: Macbeth")
    val arrietty = films("Arrietty (Dubbed)")
    arrietty.director shouldBe Seq("Hiromasa Yonebayashi")
    arrietty.cast shouldBe Seq("Saoirse Ronan", "Tom Holland", "Olivia Colman")
    arrietty.movie.releaseYear shouldBe Some(2011)
    arrietty.ageRating shouldBe Some("U")
    arrietty.showtimes.map(s => (s.dateTime, s.room)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 17, 14, 15) -> Some("4"),
      LocalDateTime.of(2026, 10, 18, 14, 15) -> Some("4"))
  }

  it should "read the production countries a box office files, one each" in {
    // Broadway files `attribute_CountryOfOrigin: "Poland/Germany"`; Derby QUAD `attribute_FilmCountryOfOrigin`.
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "broadwaynottingham", BroadwayCinemaNottingham, today)
      .fetch().map(cm => cm.movie.title -> cm).toMap
    films("Fatherland").movie.countries shouldBe Seq("Poland", "Germany")
    films("Arrietty (Dubbed)").movie.countries shouldBe Seq("Japan")
  }

  "SpektrixClient on Old Market Hall" should "drop the ticket products and test events filed among the films" in {
    // 2026-10-12 capture of `theatresevern` (Theatre Severn + Old Market Hall).
    val titles = new SpektrixClient(new FakeHttpFetch("spektrix"), "theatresevern", OldMarketHallShrewsbury, today, Some("Old Market Hall"))
      .fetch().map(_.movie.title).toSet
    titles should contain ("The Incomer (12A)")
    titles should not contain "WatchWord Glasses"   // captioning-glasses hire, 48 "performances"
    titles should not contain "Name a Seat"
    titles should not contain "Test Walker Cinema"
    titles should not contain "NATIONAL THEATRE LIVE: GOLDEN BOY" // Theatre Severn's, not this building's
  }

  "SpektrixClient.filmsOf" should "list nothing for a box office that files nothing as film" in {
    // OnFife sells four halls' gigs, panto and films with no film filing a rule can read;
    // a guess would list its concerts, so the venue keeps its Flicks fallback instead.
    val events = objects("""[{"id":"1","name":"Hamnet","attribute_Genre":"Drama"},
                             {"id":"2","name":"Wicked","attribute_Genre":""}]""")
    SpektrixClient.filmsOf(events) shouldBe empty
  }

  it should "read the film filing off whichever attribute the client named, word-bounded" in {
    val events = objects("""[{"id":"1","name":"Hamnet","attribute_EventType":"Cinema"},
                             {"id":"2","name":"Filmmaker talk","attribute_EventType":"Filmmaker Talks"},
                             {"id":"3","name":"Jazz Night","attribute_EventType":"Music"},
                             {"id":"4","name":"Nosferatu","attribute_Genre1":"Screen"},
                             {"id":"5","name":"Tosca","attribute_EventCinema":true},
                             {"id":"6","name":"Stand-up","attribute_Comedy":true}]""")
    SpektrixClient.filmsOf(events).map(e => (e \ "id").as[String]) shouldBe Seq("1", "4", "5")
  }

  // Replays a 2026-10-16 capture of Leeds Heritage Theatres' Spektrix events and instances (`WriteSpektrix --record
  // leedsheritagetheatres`), trimmed to its films: the only Spektrix client billing `attribute_Website` ("HPPH", the
  // Hyde Park Picture House) beside `attribute_TheatreWebsiteGenre` and `attribute_TheatreWebsiteCommission`.
  "SpektrixClient.fetch (Leeds Heritage)" should "read the building off its site and venue attributes, not a website's" in {
    // each carries "site" inside another word: read as the building, they put the performance elsewhere and their value
    // came off the title as a building tag
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "leedsheritagetheatres", HydeParkPictureHouse,
      LocalDate.of(2026, 10, 16), Some("Hyde Park Picture House")).fetch().map(cm => cm.movie.title -> cm).toMap
    films.keySet.filter(_.contains("HPPH")) shouldBe empty
    films("Blue Velvet").showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 24, 22, 30))
  }

  // Replays a 2026-10-13 capture of DCA's Spektrix events and instances (`WriteSpektrix --record dundeeca`): its
  // festival films open the description with a facts sheet, one "Label: value" line each, which the recording of run
  // 38326582997 carried as The Everest Dream's synopsis.
  "SpektrixClient.fetch (DCA)" should "read a synopsis past the facts sheet heading the description" in {
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "dundeeca", DundeeContemporaryArtsDCA, LocalDate.of(2026, 10, 13))
      .fetch().map(cm => cm.movie.title -> cm).toMap
    val everest = films("The Everest Dream + Q&A").synopsis.value
    everest should not startWith "Director:"
    everest should include ("inspiring mountaineering documentary")
    films.values.flatMap(_.synopsis).filter(_.matches("""(?s)(?i)\s*(director|country|year|duration)\s*:.*""")) shouldBe empty
  }

  // Replays a 2026-10-13 capture of Taunton Brewhouse's Spektrix events and instances (`WriteSpektrix --record
  // brewhousetaunton`): its Royal Ballet & Opera relays list "Cast:" and "Creatives:" first and head the story
  // "Synopsis:", which the recording of run 38326582997 carried whole as five relays' synopsis.
  "SpektrixClient.fetch (Taunton Brewhouse)" should "read a synopsis from its heading on, past the cast and creatives" in {
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "brewhousetaunton", TauntonBrewhouse, LocalDate.of(2026, 10, 13))
      .fetch().map(cm => cm.movie.title -> cm).toMap
    val carmen = films("RBO Live - Carmen").synopsis.value
    carmen should startWith ("Under the oppressive heat of the Spanish sun")
    carmen should not include "Creatives:"
    films.values.flatMap(_.synopsis).filter(_.matches("""(?s)(?i)[\s ]*cast\s*:.*""")) shouldBe empty
  }

  private def objects(json: String): Seq[JsObject] = Json.parse(json).as[Seq[JsObject]]

  // 2026-11-03 captures of `lighthousepoole` (`duration: 0` on every event, the film's running time as text in
  // `attribute_RunningTime`) and `dukeslancaster` (`attribute_AgeCertificate`).
  private val november = LocalDate.of(2026, 11, 3)
  private lazy val lighthouse =
    new SpektrixClient(new FakeHttpFetch("spektrix"), "lighthousepoole", LighthousePoole, november).fetch()
  private def lighthouseFilm(prefix: String) = lighthouse.find(_.movie.title.startsWith(prefix)).value

  "SpektrixClient.fetch (Lighthouse Poole)" should "read the film's running time off the text the box office states it in" in {
    lighthouseFilm("Anna & The Apocalypse").movie.runtimeMinutes shouldBe Some(93)  // "1h 33m (+short film, Q+A)"
    lighthouseFilm("Fjord").movie.runtimeMinutes shouldBe Some(146)                 // "2h 26m (plus ads and trailers)"
    lighthouseFilm("Dementia Friendly: White Christmas").movie.runtimeMinutes shouldBe Some(120) // "2h (plus 15m interval)"
    lighthouse.count(_.movie.runtimeMinutes.isDefined) should be > 20
  }

  it should "state no running time where the figure includes an interval or is not settled" in {
    lighthouseFilm("NT Live: Golden Boy").movie.runtimeMinutes shouldBe None    // "3h TBC (no ads or trailers)"
    lighthouseFilm("RBO: Romeo & Juliet").movie.runtimeMinutes shouldBe None    // "2h 50m (… incl two intervals)"
  }

  "SpektrixClient.fetch (Broadway)" should "take the film's own duration over the slot it is booked into" in {
    // Broadway's `duration` is the slot, ads and trailers included: Die Hard (132 min) is booked for 152.
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "broadwaynottingham", BroadwayCinemaNottingham, today)
      .fetch().map(cm => cm.movie.title -> cm).toMap
    films("Die Hard").movie.runtimeMinutes shouldBe Some(132)
    films("Elf").movie.runtimeMinutes shouldBe Some(97)
    // "99min / 87", a double bill's two films, states no one runtime: the slot stands, as before.
    films("Double-bill: Candyman (1992 & 2021)").movie.runtimeMinutes shouldBe Some(236)
  }

  "SpektrixClient.fetch (Leeds Heritage)" should "read the original title a box office files" in {
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "leedsheritagetheatres", HydeParkPictureHouse,
      LocalDate.of(2026, 10, 16), Some("Hyde Park Picture House")).fetch().map(cm => cm.movie.title -> cm).toMap
    films("The Secret World of Arrietty").movie.originalTitle shouldBe Some("Karigurashi no Arietti")
    films("Case 137").movie.originalTitle shouldBe Some("Ddossier 137")
    films("Blue Velvet").movie.originalTitle shouldBe None
  }

  "SpektrixClient.fetch (The Dukes)" should "read the certificate off `attribute_AgeCertificate`" in {
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "dukeslancaster", TheDukesLancaster, november)
      .fetch().map(cm => cm.movie.title -> cm).toMap
    films("Fjord (15)").ageRating shouldBe Some("15")
    films("Wildwood (12A)").ageRating shouldBe Some("12A")
  }

  "SpektrixClient.fetch (Brewery Arts Centre Kendal)" should "take the certificate from the attribute that holds one, not from `attribute_Classification: Cinema`" in {
    // 2026-11-05 capture of `breweryarts`: every event files `attribute_Classification: "Cinema"` (a category) before
    // `attribute_FilmCertificate: "12A"`.
    val films = new SpektrixClient(new FakeHttpFetch("spektrix"), "breweryarts", BreweryArtsCentreKendal, LocalDate.of(2026, 11, 5))
      .fetch().map(cm => cm.movie.title -> cm).toMap
    films("3000 Feet +Live Q&A").ageRating shouldBe Some("12A")
    films("Die Hard").ageRating shouldBe Some("15")
    films.values.count(_.ageRating.isDefined) should be > 60
  }

  "SpektrixClient.parse" should "read the production country a box office files as `attribute_Country`" in {
    // MAC Birmingham files `attribute_Country: "Switzerland"`.
    val events    = Json.arr(Json.obj("id" -> "1", "name" -> "A Talent For Murder", "duration" -> 121,
      "attribute_Category" -> "Film", "attribute_Country" -> "Switzerland"))
    val instances = Json.arr(Json.obj("start" -> "2026-11-04T18:00:00", "event" -> Json.obj("id" -> "1")))
    SpektrixClient.parse(events, instances, LighthousePoole, november).map(_.movie.countries) shouldBe Seq(Seq("Switzerland"))
  }

  "SpektrixClient.runtimeIn" should "read the minutes a running-time attribute states for the film alone" in {
    Seq("106" -> Some(106), "140 minutes" -> Some(140), "1h" -> Some(60), "1h 33m" -> Some(93),
      "1hr 50 mins" -> Some(110), "2hrs 15mins (plus ads and trailers)" -> Some(135),
      "1h 39m (no ads and trailers, plus Q&A)" -> Some(99)).foreach { case (text, minutes) =>
      withClue(text)(SpektrixClient.runtimeIn(text) shouldBe minutes)
    }
  }

  it should "read nothing from a figure that is not the film's alone, not settled or not a time" in {
    Seq("2h (including interval)", "1hr 31mins (incl interval)", "3h TBC (no ads or trailers)", "TBC", "0",
      "99min / 87", "approx 2h", "2h (approx)", "", "Ninety minutes", "1h 30m (a) (b)").foreach { text =>
      withClue(text)(SpektrixClient.runtimeIn(text) shouldBe None)
    }
  }
}
