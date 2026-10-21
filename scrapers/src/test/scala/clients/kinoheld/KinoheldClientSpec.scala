package clients.kinoheld

import clients.tools.{FakeHttpFetch, WriteKinoheld}
import models.{CinemaMovie, GermanCinema}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{KinoheldClient, ListingReads}
import services.identity.SourceReliability
import tools.HttpFetch

import java.io.IOException
import java.time.{LocalDate, LocalDateTime}
import scala.collection.mutable.ListBuffer

/** Replays seven venues' real kinoheld.de programmes, every `programShows` page,
 *  recorded on 2026-10-11 (Arthaus-Kino Bonn's on 2026-10-12, Filmwerk Gütersloh's on 2026-10-14) by [[WriteKinoheld]]
 *  (see it for why each venue is there). */
class KinoheldClientSpec extends AnyFlatSpec with Matchers with LoneElement {

  private val Today  = LocalDate.of(2026, 10, 11)
  private val venues = WriteKinoheld.Venues.toMap

  /** Counts the pages asked for, and fails the ones `failing` names. */
  private class Counting(inner: HttpFetch, failing: Int => Boolean = _ => false) extends HttpFetch {
    val pages = ListBuffer.empty[Int]
    def get(url: String): String = inner.get(url)
    override def post(url: String, body: String, contentType: String): String = {
      val page = (play.api.libs.json.Json.parse(body) \ "variables" \ "page").as[Int]
      pages += page
      if (failing(page)) throw new IOException(s"page $page refused")
      inner.post(url, body, contentType)
    }
  }

  private def client(name: String, fetch: HttpFetch = new FakeHttpFetch("kinoheld")) =
    new KinoheldClient(fetch, venues(name), new GermanCinema(name, name), today = Today)

  private def scrape(name: String): Seq[CinemaMovie] = client(name).fetch()

  private lazy val capitol    = scrape("Capitol Kino Bochum")
  private lazy val uci        = scrape("UCI Kinowelt Ruhr Park")
  private lazy val casablanca = scrape("Casablanca Filmtheater Bochum")
  private lazy val lumen      = scrape("Das Lumen Filmtheater Solingen")
  private lazy val nestkino   = scrape("nestkino Hamburg")

  private def film(movies: Seq[CinemaMovie], title: String): CinemaMovie =
    movies.find(_.movie.title == title).getOrElse(fail(s"no '$title' in ${movies.map(_.movie.title)}"))

  "KinoheldClient" should "read a Bochum venue Filmstarts never listed — every film and showtime" in {
    capitol should have size 31
    capitol.flatMap(_.showtimes) should have size 115
    capitol.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 1, 13)
    capitol.map(_.cinema.displayName).distinct shouldBe Seq("Capitol Kino Bochum")
    client("Capitol Kino Bochum").sourceUrl shouldBe Some("https://www.kinoheld.de/kino/bochum/capitol")
  }

  // `shows(limit:)` caps at 500 and a multiplex lists half a year ahead: only walking the
  // paginator reaches the end of the programme, and a horizon cap is what loses films.
  it should "walk every page of a multiplex's programme, not stop at the first" in {
    val fetch = new Counting(new FakeHttpFetch("kinoheld"))
    val movies = client("UCI Kinowelt Ruhr Park", fetch).fetch()
    fetch.pages.toList shouldBe List(1, 2, 3, 4, 5)
    movies.flatMap(_.showtimes) should have size 427
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 6, 5)
    uci.flatMap(_.showtimes).count(_.format.contains("IMAX")) shouldBe 72
    uci.flatMap(_.showtimes).count(_.format.contains("3D")) shouldBe 24
  }

  it should "fail the scrape when the first page cannot be read" in {
    val fetch = new Counting(new FakeHttpFetch("kinoheld"), failing = _ == 1)
    an[IOException] should be thrownBy client("UCI Kinowelt Ruhr Park", fetch).fetch()
  }

  it should "keep the pages before a failed later one, and mark the listing incomplete" in {
    val fetch = new Counting(new FakeHttpFetch("kinoheld"), failing = _ == 3)
    val (movies, reads) = ListingReads.during(client("UCI Kinowelt Ruhr Park", fetch).fetch())
    fetch.pages.toList shouldBe List(1, 2, 3)
    movies.flatMap(_.showtimes) should have size 200
    reads.complete shouldBe false
  }

  it should "throw on a GraphQL error rather than read a venue with nothing on" in {
    val errors = """{"errors":[{"message":"Variable \"$cinema\" of required type \"ID!\" was not provided."}]}"""
    an[IllegalStateException] should be thrownBy KinoheldClient.parsePage(errors, latestYear = Today.getYear + 1)
  }

  it should "give each show its start in German wall-clock time, its room and its booking deep-link" in {
    val first = film(capitol, "Spider-Man: Brand New Day").showtimes.head
    first.dateTime shouldBe LocalDateTime.of(2026, 10, 12, 15, 0)   // "2026-10-12T15:00:00+02:00"
    first.room shouldBe Some("Kino 1")
    first.bookingUrl shouldBe Some("https://kinotickets.express/bochum-capitol/booking/62590")
  }

  // A venue that sells through kinoheld itself gets no `deeplink`: its show page books it.
  it should "book a show of a kinoheld-ticketed venue on the show's own kinoheld page" in {
    film(lumen, "Digger").showtimes.flatMap(_.bookingUrl).head should fullyMatch regex
      """https://www\.kinoheld\.de/kino/solingen/das-lumen-filmtheater-solingen/vorstellung/\d+"""
  }

  it should "list a film under kinoheld's German catalogue title, the venue's name being only its spelling" in {
    // The venue lists "Forgotten Island"; Filmstarts and kinoheld both call it "Die vergessene Insel".
    val island = film(lumen, "Die vergessene Insel")
    island.movie.originalTitle shouldBe Some("Forgotten Island")
    // "Der kleine Eisbär MeK" (Mein erster Kinobesuch) is the venue's label for the slot.
    film(lumen, "Der kleine Eisbär").showtimes should have size 3
    lumen.map(_.movie.title) should not contain ("Der kleine Eisbär MeK")
  }

  // Casablanca's travel talk "Nordische Inseln" is linked to "Life Itself": listing it under
  // that title would put a wrong film's card, poster and ratings on the venue's slot.
  it should "keep the venue's own name, and none of the linked film's facts, when the link names another film" in {
    casablanca.map(_.movie.title) should not contain ("Life Itself")
    val talk = film(casablanca, "Nordische Inseln")
    talk.filmUrl shouldBe None
    talk.posterUrl shouldBe None
    talk.director shouldBe empty
    talk.movie.releaseYear shouldBe None
    // A link the venue's name does share a word with stands: "Schottland" is the film's own word.
    film(casablanca, "Mary - Königin von Schottland").movie.originalTitle shouldBe Some("Mary, Queen of Scots")
  }

  it should "carry the linked film's runtime, year, countries, genres, credits, synopsis, poster, trailer and FSK" in {
    val shaun = film(capitol, "Shaun das Schaf – Spuk im Kürbisfeld")
    shaun.movie.runtimeMinutes shouldBe Some(80)
    shaun.movie.releaseYear shouldBe Some(2026)
    shaun.movie.countries shouldBe Seq("United Kingdom")
    shaun.movie.genres should contain ("Animation")
    shaun.movie.originalTitle shouldBe Some("Shaun The Sheep: The Beast Of Mossy Bottom")
    shaun.director shouldBe Seq("Matthew Walker", "Steve Cox")
    shaun.cast should contain ("John Sparkes")
    shaun.synopsis.get should startWith ("Auf der Mossy-Bottom-Farm")
    shaun.posterUrl.get should startWith ("https://static.kinoheld.de/images/film/shaun-das-schaf-spuk-im-kuerbisfeld-96025")
    shaun.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=Ik8diQEcRiI")
    shaun.ageRating shouldBe Some("FSK 0")
  }

  // kinoheld's catalogue files "Hirten" (Bergers, 2025) under productionYear "2040", and other films under 2034 and
  // 2051 (recorded DE corpus, 2026-10-12): a slip no film screened today can have, read as no year rather than one
  // that vetoes every candidate.
  it should "read a catalogue year past next year as unpublished" in {
    val bonn = scrape("Arthaus-Kino Bonn")
    film(bonn, "Hirten").movie.releaseYear shouldBe None
    film(bonn, "Hirten").movie.runtimeMinutes shouldBe Some(113)
    film(bonn, "Spaziergang nach Syrakus").movie.releaseYear shouldBe Some(2026)
  }

  // The linked film's facts are kinoheld's catalogue claim, which the identity resolver must
  // weigh as a feed's, not as the venue's own: a mislinked entry only repeats the wrong film.
  it should "point the film at kinoheld's catalogue page, which the resolver reads as a feed's claim" in {
    val shaun = film(capitol, "Shaun das Schaf – Spuk im Kürbisfeld")
    shaun.filmUrl shouldBe Some("https://www.kinoheld.de/film/shaun-das-schaf-spuk-im-kuerbisfeld-2026")
    SourceReliability.feedStated(Nil, shaun.filmUrl) shouldBe true
  }

  it should "leave out a sneak preview, which names no film" in {
    lumen.map(_.movie.title) should not contain ("Sneak Preview")
    lumen.map(_.movie.title).filter(_.toLowerCase(java.util.Locale.ROOT).contains("sneak")) shouldBe empty
  }

  // Filmwerk Gütersloh bills November's sneak "Sneak_nov", which kinoheld links to an 8-minute short "Sneak Over":
  // the two share only the word "sneak", and a sneak names no film for any link to be right about.
  it should "leave out a sneak billed by its month, whatever film kinoheld linked it to" in {
    val filmwerk = scrape("Filmwerk Gütersloh")
    filmwerk.map(_.movie.title).filter(_.toLowerCase(java.util.Locale.ROOT).contains("sneak")) shouldBe empty
    filmwerk.flatMap(_.showtimes).flatMap(_.bookingUrl).filter(_.contains("1000-57804")) shouldBe empty
  }

  it should "mark the language version as Filmstarts does: OV, OmU, OmeU" in {
    film(lumen, "Avengers: Doomsday").showtimes.count(_.format.contains("OV")) shouldBe 8
    film(lumen, "Avengers: Doomsday").showtimes.count(_.format.contains("DBOX")) should be > 0
    // The subtitle language decides: the venue's "CALL ME QUEEN OmeU" slot carries German subtitles.
    film(nestkino, "Call Me Queen").showtimes.map(_.format).distinct shouldBe Seq(List("OmU"))
    film(nestkino, "Naza").showtimes.count(_.format == List("OmeU")) shouldBe 4
    nestkino.flatMap(_.showtimes).count(_.format == List("OmU")) should be > 20
    capitol.flatMap(_.showtimes).flatMap(_.format) shouldBe empty   // German films and dubs: unmarked
  }

  "KinoheldClient.formatTokens" should "read the subtitle language before the venue's flags" in {
    KinoheldClient.formatTokens(Seq("LANGUAGE" -> "subtitled"), Some("en")) shouldBe List("OmeU")
    KinoheldClient.formatTokens(Nil, Some("de")) shouldBe List("OmU")
    KinoheldClient.formatTokens(Seq("LANGUAGE" -> "omdu"), None) shouldBe List("OmU")
    KinoheldClient.formatTokens(Seq("TECHNOLOGY" -> "imax", "TECHNOLOGY" -> "3d", "LANGUAGE" -> "original"), None) shouldBe
      List("IMAX", "3D", "OV")
    KinoheldClient.formatTokens(Seq("EVENT" -> "sneak-preview", "MISCELLANEOUS" -> "kids"), None) shouldBe Nil
  }

  "KinoheldClient.linkTrusted" should "trust a link the venue's name shares a word with, or a placeholder name" in {
    KinoheldClient.linkTrusted("Forgotten Island", "Die vergessene Insel", Some("Forgotten Island")) shouldBe true
    KinoheldClient.linkTrusted("KINOHELD_DUMMY_BUEHNEMITKLEINKUNST", "Dan Popek - The joy of piano", None) shouldBe true
    KinoheldClient.linkTrusted("Nordische Inseln", "Life Itself", None) shouldBe false
    // A shared article is not a shared film.
    KinoheldClient.linkTrusted("Die Assistentin", "Die Mutter", None) shouldBe false
    // A mystery screening names no film, so no film linked to it is the venue's.
    KinoheldClient.linkTrusted("Sneak_nov", "Sneak Over", Some("Sneak Over")) shouldBe false
  }

  "KinoheldClient.parsePage" should "keep the venue's own name, and none of a renamed entry's facts" in {
    val body =
      """{"data":{"programShows":{"paginatorInfo":{"hasMorePages":false},"data":[{
        |"name":"Tyketto + Special Guest","beginning":"2026-11-07T20:00:00+01:00","urlSlug":"1","deeplink":null,
        |"flags":[],"subtitleLanguage":null,"auditorium":null,"cinema":null,
        |"movie":{"id":"74283","title":"Tyketto + Special Guest","titleOriginal":"Rock Around the Clock",
        |"urlSlug":"ausser-rand-und-band-rock-around-the-clock","duration":76,"productionYear":null,
        |"description":"In der Provinz entdeckt der Bandmanager Steve Hollis eine Rock-'n'-Roll-Band",
        |"directors":[{"name":"Fred F. Sears"}],"actors":[{"name":"Bill Haley"}],
        |"thumbnailImage":{"url":"https://static.kinoheld.de/images/film/ausser-rand-und-band-74283.v17459015592183.jpg"}}}]}}}""".stripMargin
    val show = KinoheldClient.parsePage(body, latestYear = Today.getYear + 1).shows.loneElement
    show.film shouldBe KinoheldClient.Film(id = None, title = "Tyketto + Special Guest")
    show.renamedEntry shouldBe Some("74283")
  }

  private def poster(stem: String) = Some(s"https://static.kinoheld.de/images/film/$stem.v17483032301696.jpg")

  // kinoheld's entries are reused: a venue renames an old film's entry for its next event, and the entry keeps the old
  // film's slug, poster, original title, credit, runtime and synopsis (prod 2026-10-21: entry 60643, made for "Hodder
  // rettet die Welt", titled "Über Unterbiberger - Vom Woid in die Welt" at Filmforum Landsberg; 74283, made for "Rock
  // Around the Clock", titled "Tyketto + Special Guest" at Kaminwerk).
  "KinoheldClient.entryRenamed" should "name an entry its title shares no word of the original title with, nor half its slug and poster" in {
    KinoheldClient.entryRenamed("Über Unterbiberger - Vom Woid in die Welt", Some("En som Hodder"),
      Some("hodder-rettet-die-welt"), poster("hodder-rettet-die-welt-60643")) shouldBe true
    KinoheldClient.entryRenamed("Tyketto + Special Guest", Some("Rock Around the Clock"),
      Some("ausser-rand-und-band-rock-around-the-clock"), poster("ausser-rand-und-band-74283")) shouldBe true
    KinoheldClient.entryRenamed("Guido Cantz - Komische Zeiten", Some("The Love Punch"),
      Some("wie-in-alten-zeiten"), poster("wie-in-alten-zeiten-931")) shouldBe true
    KinoheldClient.entryRenamed("Über Unterbiberger - Vom Woid in die Welt", Some("Октябрь"), Some("der-7-oktober-die-stadt-und-das-leben"),
      poster("oktober-oktober-1917-zehn-tage-die-die-welt-erschuetterten-71321")) shouldBe true
  }

  it should "not name an entry one of whose names spells its title, nor one stating no original title" in {
    // a German title beside a foreign original, the poster named for the German title
    KinoheldClient.entryRenamed("Mit leiser Stimme", Some("À voix basse"), Some("in-a-whisper"), poster("mit-leiser-stimme-94793")) shouldBe false
    // a slug left from the entry's first title, the poster and the credit the film's
    KinoheldClient.entryRenamed("Der Leopard", Some("Il gattopardo"), Some("rampart"), poster("der-leopard-36349")) shouldBe false
    KinoheldClient.entryRenamed("Regio Tales", Some("Heads and Tales"), Some("regio-tales"), None) shouldBe false
    KinoheldClient.entryRenamed("Die vergessene Insel", Some("Forgotten Island"), Some("die-vergessene-insel"), None) shouldBe false
    // a German title translating the original, which it shares a word with
    KinoheldClient.entryRenamed("Festtagsspaß mit Peppa - Das Kinoerlebnis", Some("Festive Fun with Peppa"),
      Some("festive-fun-with-peppa-cinema-experience"), None) shouldBe false
    // an entry made under its original title, then given its German one
    KinoheldClient.entryRenamed("Schach dem Teufel", Some("Beat the Devil"), Some("beat-the-devil"), None) shouldBe false
    KinoheldClient.entryRenamed("Nur geträumt", Some("Juste une illusion"), Some("juste-une-illusion"), None) shouldBe false
    // the Filmmuseum bills Fellini by his own title: no original title says which film the entry was made for
    KinoheldClient.entryRenamed("E la nave va", None, Some("fellinis-schiff-der-traeume"), poster("fellinis-schiff-der-traeume-1")) shouldBe false
    // a venue's own entry, named by a hash: nothing to compare
    KinoheldClient.entryRenamed("Alte Liebe", Some("Old Love"), Some("6a879d07bd18315fab310531"), None) shouldBe false
  }

  "KinoheldClient.parsePage" should "keep the venue's own name, and none of the facts kinoheld guessed for an event's entry" in {
    val body =
      """{"data":{"programShows":{"paginatorInfo":{"hasMorePages":false},"data":[{
        |"name":"Ping Pong Session","beginning":"2026-11-07T20:00:00+01:00","urlSlug":"1","deeplink":null,
        |"flags":[],"subtitleLanguage":null,"auditorium":null,"cinema":null,
        |"movie":{"id":"88823","title":"Ping Pong Session","titleOriginal":"Ping Pong","urlSlug":"ping-pong-session",
        |"duration":null,"productionYear":null,"description":null,"genres":[{"name":"Animation"}],
        |"directors":[{"name":"Yu Tianji"}],"actors":[],"thumbnailImage":null}}]}}}""".stripMargin
    val show = KinoheldClient.parsePage(body, latestYear = Today.getYear + 1).shows.loneElement
    show.film shouldBe KinoheldClient.Film(id = None, title = "Ping Pong Session")
    show.guessedEntry shouldBe Some("88823")
  }

  // A venue's event gets a catalogue entry of its own name, which kinoheld fills from a film database by that name: no
  // year, no runtime, and the original title, credit and cast of a film titled with part of it (prod 2026-10-22:
  // "Ping Pong Session" -> Ping Pong (2025), "Helmut Schleich" -> Helmut, "Sunday Night Jazz" -> Sunday Night).
  "KinoheldClient.entryGuessed" should "name a yearless, runtimeless entry whose original title is part of its title" in {
    KinoheldClient.entryGuessed("Ping Pong Session", Some("Ping Pong"), year = None, runtime = None) shouldBe true
    KinoheldClient.entryGuessed("Helmut Schleich", Some("Helmut"), None, None) shouldBe true
    KinoheldClient.entryGuessed("ICONIC MOVIE", Some("ICONIC"), None, None) shouldBe true
    KinoheldClient.entryGuessed("Regio Tales", Some("Heads and Tales"), None, None) shouldBe true
    KinoheldClient.entryGuessed("Christmas Time Machine", Some("Time Machine"), None, None) shouldBe true
  }

  it should "not name an entry with a year or a runtime, a translated title, or a title the original title spells out" in {
    // a catalogue film: its year and runtime are the distributor's
    KinoheldClient.entryGuessed("Ping Pong Session", Some("Ping Pong"), year = Some(2025), runtime = None) shouldBe false
    KinoheldClient.entryGuessed("Ping Pong Session", Some("Ping Pong"), year = None, runtime = Some(15)) shouldBe false
    // a festival film's own title, or none beside it
    KinoheldClient.entryGuessed("Nuisance Bear", None, None, None) shouldBe false
    KinoheldClient.entryGuessed("Falcon Lake", Some("Falcon Lake"), None, None) shouldBe false
    // a German title translating the original
    KinoheldClient.entryGuessed("Red Rocks", Some("Les Roches Rouges"), None, None) shouldBe false
    KinoheldClient.entryGuessed("Festtagsspaß mit Peppa - Das Kinoerlebnis", Some("Festive Fun with Peppa"), None, None) shouldBe false
    // the venue's title is the original's, shortened
    KinoheldClient.entryGuessed("Cloud", Some("Cloud クラウド"), None, None) shouldBe false
    KinoheldClient.entryGuessed("Ghost in the Cell", Some("Ghost in the Cell 2"), None, None) shouldBe false
    // a strand's or a translation's billing, one part of which is the film's original title
    KinoheldClient.entryGuessed("FRAUENWELTEN 2026: KILLING ANNA", Some("Killing Anna"), None, None) shouldBe false
    KinoheldClient.entryGuessed("Viva a Vida - Auf das Leben", Some("Viva a Vida"), None, None) shouldBe false
    KinoheldClient.entryGuessed("Bratři | Brüder", Some("Bratři"), None, None) shouldBe false
    // a distributor's entry, its year and runtime not yet known
    KinoheldClient.entryGuessed("MINIONS & MONSTER", Some("Minions 3"), None, None, countries = Seq("Vereinigte Staaten")) shouldBe false
  }
}
