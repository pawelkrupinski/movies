package clients.kino_kuznica

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import clients.tools.FakeHttpFetch
import models._
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.JsString
import services.cinemas.pl.{EventCategory, Institution, SystemBiletowyClient}

import java.time.LocalDateTime
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Replays every VisualSoft instance's `service.php/repertoire/list.json` feed,
 *  captured live 2026-09-26 into `fixtures/visualsoft/`, through the one generic
 *  client.
 *
 *  The client used to scrape each instance's `index.php` in one of four HTML
 *  skins, which drifted independently (Bochnia's titles moved from `h3` to `h2`
 *  on ~2026-09-21 and the venue read empty for five days). Against these
 *  fixtures the HTML client fails outright: it asks for `index.php`, which is
 *  not recorded here. */
class SystemBiletowyClientSpec extends AnyFlatSpec with Matchers with OptionValues with TableDrivenPropertyChecks {

  private val http = new FakeHttpFetch("visualsoft")

  private def client(base: String, cinema: Cinema, filmGroups: Set[EventCategory] = Set.empty,
                     institution: Option[Institution] = None) =
    new SystemBiletowyClient(http, base, cinema, titles = titleNormalizer, filmGroups = filmGroups,
      institution = institution)

  // (instance, cinema, title fragment, screening, exact booking link)
  private val venues = Table(
    ("base", "cinema", "title", "when", "booking"),
    ("https://kgl.systembiletowy.pl", KinoKawiarnia, "zapomniana wyspa", LocalDateTime.of(2026, 10, 8, 16, 30),
      "https://kgl.systembiletowy.pl/kup-bilet/zapomniana-wyspa-2026-10-08-16-30"),
    ("https://shd.systembiletowy.pl", KinoKuznica, "lalka", LocalDateTime.of(2026, 10, 2, 19, 0),
      "https://shd.systembiletowy.pl/kup-bilet/lalka-premiera-2026-10-02-16-00-1"),
    ("https://bilety.pckul.pl", KinoPckulKino, "marsupilami", LocalDateTime.of(2026, 10, 10, 14, 0),
      "https://bilety.pckul.pl/kup-bilet/marsupilami-2026-10-09-14-00-2"),
    ("https://bilety.mok.zory.pl", KinoNaStarowce, "misja zeus", LocalDateTime.of(2026, 10, 1, 14, 0),
      "https://bilety.mok.zory.pl/kup-bilet/100-dni-misja-zeus-2026-10-01-14-00"),
    ("https://kck.systembiletowy.pl", KinoCentrum3D, "lalka", LocalDateTime.of(2026, 10, 3, 15, 45),
      "https://kck.systembiletowy.pl/kup-bilet/lalka-2026-10-03-15-45"),
    ("https://bilety.kino.bochnia.pl", KinoRegis, "dzień dziecka", LocalDateTime.of(2026, 10, 3, 17, 30),
      "https://bilety.kino.bochnia.pl/kup-bilet/dzien-dziecka-ksiedza-jana-kaczkowskiego-2026-10-03-17-30"),
    ("https://ckp.systembiletowy.pl", KinoKalejdoskop, "zapomniana wyspa", LocalDateTime.of(2026, 10, 3, 12, 0),
      "https://ckp.systembiletowy.pl/kup-bilet/zapomniana-wyspa-2026-10-03-12-00"),
    ("https://sta.systembiletowy.pl", KinoKadrStaszow, "lalka", LocalDateTime.of(2026, 10, 2, 20, 20),
      "https://sta.systembiletowy.pl/kup-bilet/lalka-2026-10-02-20-20"),
    ("https://bdk.systembiletowy.pl", KinoBieszczadzkiDK, "obcy", LocalDateTime.of(2026, 10, 9, 18, 0),
      "https://bdk.systembiletowy.pl/kup-bilet/obcy-2d-napisy-pl-2026-10-09-18-00"),
    ("https://kht.systembiletowy.pl", Kino1410, "vivaldi i ja", LocalDateTime.of(2026, 10, 10, 19, 0),
      "https://kht.systembiletowy.pl/kup-bilet/vivaldi-i-ja-2026-10-10-19-00"),
    ("https://oks.systembiletowy.pl", KinoCKiBNowaSarzyna, "lalka", LocalDateTime.of(2026, 10, 16, 18, 30),
      "https://oks.systembiletowy.pl/kup-bilet/lalka-2026-10-16-15-30-1"),
    // The two instances without the advanced template: the plain feed, with the
    // booking link built from the screening id.
    ("https://kfb.systembiletowy.pl", KinoFarys, "misja zeus", LocalDateTime.of(2026, 9, 27, 17, 0),
      "https://kfb.systembiletowy.pl/index.php/repertoire.html?id=7599"),
    ("https://udk.systembiletowy.pl", KinoOrzelUstrzyki, "podręcznik dla suprbohaterów", LocalDateTime.of(2026, 10, 2, 17, 0),
      "https://udk.systembiletowy.pl/index.php/repertoire.html?id=1128"),
  )

  forAll(venues) { (base, cinema, title, when, booking) =>
    it should s"read a real screening off the feed — ${cinema.displayName}" in {
      val movies = client(base, cinema).fetch()
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      // Every row carrying the title: a programme suffix ("… Tani Poniedziałek")
      // keeps a film's screening on a row of its own.
      val showtimes = movies.filter(_.movie.title.toLowerCase.contains(title)).flatMap(_.showtimes)
      showtimes.find(_.dateTime == when).value.bookingUrl.value shouldBe booking
    }
  }

  "SystemBiletowyClient" should "merge a film's dubbed and subtitled screenings into one row carrying each format" in {
    // "OBCY (2D, NAPISY PL)" and its dubbed twin fold onto one "Obcy".
    val obcy = client("https://bdk.systembiletowy.pl", KinoBieszczadzkiDK).fetch()
      .filter(_.movie.title.toLowerCase.startsWith("obcy"))
    obcy.map(_.movie.title) shouldBe Seq("Obcy")
    obcy.head.showtimes.map(_.format).toSet should contain(List("2D", "NAP"))
  }

  it should "peel Kino Orzeł's '-Film 2D dubbing' boilerplate into the format" in {
    val film = client("https://udk.systembiletowy.pl", KinoOrzelUstrzyki).fetch()
      .find(_.movie.title.toLowerCase.startsWith("podręcznik")).value
    film.movie.title shouldBe "Podręcznik dla suprbohaterów"
    film.showtimes.head.format should contain allOf ("2D", "DUB")
  }

  it should "carry the poster the advanced feed names" in {
    client("https://bilety.kino.bochnia.pl", KinoRegis).fetch().flatMap(_.posterUrl).head should
      startWith("https://bilety.kino.bochnia.pl/uploads/")
  }

  // ── Scopes: instances selling more than one venue's events ─────────────────

  it should "keep only BCKino's film events, peeling their 'BCKino – ' prefix" in {
    val movies = client("https://bck.systembiletowy.pl", KinoBCKBytom, filmGroups = Set(EventCategory("BCKino"))).fetch()
    movies.map(_.movie.title) should contain("Koniec imprezy")
    all(movies.map(_.movie.title.toLowerCase)) should not include "bckino"
    // The instance's 47 other events — "BECEK CZYTA" readings, workshops — stay out.
    movies.flatMap(_.showtimes).size shouldBe 6
  }

  it should "keep only Kino Frajda's Imprezy SDK events, dropping Chorzów's own" in {
    val movies = client("https://bilety.chck.pl", KinoFrajda, filmGroups = Set(EventCategory("Imprezy SDK"))).fetch()
    movies.map(_.movie.title.toLowerCase).exists(_.contains("zagadka klary muu")) shouldBe true
    // "KOSZMAREK" screens at both: 10-20 as "Imprezy ChCK", 10-24 as "Imprezy SDK".
    val koszmarek = movies.find(_.movie.title.toLowerCase.contains("koszmarek")).value.showtimes.map(_.dateTime)
    koszmarek should contain(LocalDateTime.of(2026, 10, 24, 15, 0))
    koszmarek should not contain LocalDateTime.of(2026, 10, 20, 17, 0)
  }

  it should "scope Oświęcim's instance to Nasze Kino, dropping the culture centre's concerts and plays" in {
    val movies = client("https://ock.systembiletowy.pl", KinoNaszeKino, institution = Some(Institution("Nasze Kino"))).fetch()
    movies.map(_.movie.title.toLowerCase).exists(_.contains("mistyczka")) shouldBe true
    // A comedy play and a concert sold by the centre on the same instance; the
    // old HTML scrape let "Ale kino, czyli muzyka filmowa…" through as a film.
    movies.map(_.movie.title.toLowerCase).exists(_.contains("dobrze się kłamie")) shouldBe false
    movies.map(_.movie.title.toLowerCase).exists(_.contains("ale kino, czyli muzyka filmowa")) shouldBe false
  }

  it should "split the one Mikro instance between its two screens" in {
    val mikro     = client("https://bilety.kinomikro.pl", KinoMikro, institution = Some(Institution("Kino Mikro"))).fetch()
    val bronowice = client("https://bilety.kinomikro.pl", MikroBronowice, institution = Some(Institution("Mikro Bronowice"))).fetch()
    mikro.map(_.cinema).toSet shouldBe Set(KinoMikro)
    bronowice.map(_.cinema).toSet shouldBe Set(MikroBronowice)
    mikro.find(_.movie.title == "Orlando").value.showtimes.map(_.dateTime) should
      contain(LocalDateTime.of(2026, 9, 28, 16, 45))
    // "Marsupilami- dubbing" is Bronowice-only, folded onto its film with DUB.
    val marsupilami = bronowice.find(_.movie.title == "Marsupilami").value
    all(marsupilami.showtimes.map(_.format)) should contain("DUB")
    mikro.map(_.movie.title) should not contain "Marsupilami"
  }

  it should "give the two Mikro screens distinct source keys" in {
    val mikro     = client("https://bilety.kinomikro.pl", KinoMikro, institution = Some(Institution("Kino Mikro")))
    val bronowice = client("https://bilety.kinomikro.pl", MikroBronowice, institution = Some(Institution("Mikro Bronowice")))
    mikro.sourceKey should not be bronowice.sourceKey
  }

  it should "keep a screening's wall-clock time across the CEST→CET switch" in {
    // "2026-10-29T21:00:00+01:00" on the Mikro feed.
    client("https://bilety.kinomikro.pl", KinoMikro, institution = Some(Institution("Kino Mikro"))).fetch()
      .flatMap(_.showtimes).map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 29, 21, 0))
  }

  it should "parse the director out of the event description, stopping at the next label" in {
    val mikro = client("https://bilety.kinomikro.pl", KinoMikro, institution = Some(Institution("Kino Mikro"))).fetch()
    // `Reżyseria: François Ozon  Występują: Benjamin Voisin, …`
    mikro.find(_.movie.title == "Obcy").value.director shouldBe Seq("François Ozon")
    // `Reżyseria: Louis Malle  Muzyka: Miles Davis  Scenariusz: …`
    mikro.find(_.movie.title == "Windą na szafot").value.director shouldBe Seq("Louis Malle")
    // `Reżyseria: Sam Raimi | Produkcja: USA, 1987 | …`
    mikro.find(_.movie.title == "Martwe zło 2").value.director shouldBe Seq("Sam Raimi")
  }

  // Director layouts not showing in the recorded weeks, through the public parser.
  private def directorOf(description: String): Seq[String] = {
    val json =
      s"""{"repertoires":{"1":{"id":1,"title":"Probe","date":"2026-10-15T18:00:00+02:00",
         |"event":{"description":${JsString(description)}}}}}""".stripMargin
    SystemBiletowyClient.parse(json, KinoMikro, "https://x.example", titleNormalizer).head.director
  }

  "SystemBiletowyClient.parse" should "read a no-colon director terminated by the next label" in {
    directorOf("<div>Reżyseria George Sluizer</div><div>Obsada Bernard-Pierre Donnadieu</div>") shouldBe
      Seq("George Sluizer")
  }

  it should "split co-directors and return no director when the Reżyseria marker is absent" in {
    directorOf("<div>Reżyseria: Joel Coen, Ethan Coen</div><div>Gatunek dramat</div>") shouldBe
      Seq("Joel Coen", "Ethan Coen")
    directorOf("<div>Gatunek: dramat</div>") shouldBe empty
  }

  // Casing is repaired only where the source shouts: a mixed-case title is the
  // venue's own spelling and stays as given.
  it should "down-case a shouted title but keep a mixed-case one as given" in {
    client("https://bilety.kino.bochnia.pl", KinoRegis).fetch().map(_.movie.title) should contain("Marsupilami")
    client("https://bilety.kinomikro.pl", KinoMikro, institution = Some(Institution("Kino Mikro"))).fetch().map(_.movie.title) should
      contain("Birthday Party")
  }
}
