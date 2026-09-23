package clients.kino_kuznica

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{KinoBCKBytom, KinoCentrum3D, KinoFarys, KinoKadrStaszow, KinoKawiarnia, KinoKuznica, KinoOrzelUstrzyki, KinoPckulKino, KinoRegis}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.SystemBiletowyClient

import java.time.LocalDateTime
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Replays the recorded `shd.systembiletowy.pl/index.php` repertoire (the
 *  Suchedniów cultural centre's Kino Kuźnica instance) through the generic
 *  systembiletowy client.
 *
 *  Kino Kuźnica was previously scraped from Filmweb, whose API had silently
 *  gone empty for it (every poll returned `[]`) though the cinema is open —
 *  this fixture is the proof its programme is real and reachable on its own
 *  ticketing portal. */
class SystemBiletowyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new SystemBiletowyClient(new FakeHttpFetch("kino-kuznica"), "https://shd.systembiletowy.pl", KinoKuznica, titles = titleNormalizer).fetch()

  "SystemBiletowyClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKuznica)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "merge a film's dubbing + napisy screenings into one row" in {
    // The fixture lists "… MANDALORIAN & GROGU  dubbing" and "… napisy" as
    // separate rows; stripping the version tag must fold them into ONE film
    // carrying both the 16:00 (dubbed) and 18:30 (subtitled) screenings on 06-12.
    val mandalorian = movies.filter(_.movie.title.toLowerCase.contains("mandalorian"))
    mandalorian.size shouldBe 1
    val times = mandalorian.head.showtimes.map(_.dateTime)
    times should contain(LocalDateTime.of(2026, 6, 12, 16, 0))
    times should contain(LocalDateTime.of(2026, 6, 12, 18, 30))
  }

  it should "carry a per-screening booking link" in {
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).head should include("repertoire.html?id=")
  }

  // ── Kino Farys (Biecz, the kfb.systembiletowy.pl instance) ──────────────────
  private val farys =
    new SystemBiletowyClient(new FakeHttpFetch("kino-farys"), "https://kfb.systembiletowy.pl", KinoFarys, titles = titleNormalizer).fetch()

  "SystemBiletowyClient (Farys)" should "parse the Biecz instance off the same client" in {
    farys should not be empty
    farys.map(_.cinema).toSet shouldBe Set(KinoFarys)
    val film = farys.find(_.movie.title.toLowerCase.contains("willow")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 15, 0))
  }

  // ── Alternate div.event-item skin (Kino PCKul, Pszczyna) ────────────────────
  private val pckul =
    new SystemBiletowyClient(new FakeHttpFetch("kino-pckul"), "https://bilety.pckul.pl", KinoPckulKino, titles = titleNormalizer).fetch()

  "SystemBiletowyClient (alt skin)" should "parse the div.event-item Bootstrap skin" in {
    pckul should not be empty
    pckul.map(_.cinema).toSet shouldBe Set(KinoPckulKino)
    val film = pckul.find(_.movie.title.toLowerCase.contains("mumbo jumbo")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 10, 13, 30))
  }

  // ── Current `/css/visual9` skin: div.event-item[data-date][data-time] ─────────
  // The vendor's latest UI carries the ISO date + time as data attributes and the
  // title in `h3.event-title`. These three venues were each previously scraped
  // from Filmweb (cinema ids 117 / 1513 / 1294) — the fixtures (recorded into the
  // 08-06-2026 corpus, replayed here) prove each programme is real and reachable
  // on its own VisualSoft portal, served under both the vendor subdomain
  // (kgl/kck.systembiletowy.pl) and a venue's own domain (bilety.kino.bochnia.pl).
  private def visual9(base: String, cinema: models.Cinema) =
    new SystemBiletowyClient(new FakeHttpFetch("08-06-2026"), base, cinema, titles = titleNormalizer).fetch()

  "SystemBiletowyClient (visual9 skin)" should "parse Kino Kawiarnia (kgl.systembiletowy.pl)" in {
    val movies = visual9("https://kgl.systembiletowy.pl", KinoKawiarnia)
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKawiarnia)
    val film = movies.find(_.movie.title.toLowerCase.contains("toy story")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 17, 17, 0))
  }

  it should "parse Centrum 3D Kalisz (kck.systembiletowy.pl)" in {
    val movies = visual9("https://kck.systembiletowy.pl", KinoCentrum3D)
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoCentrum3D)
    val film = movies.find(_.movie.title.toLowerCase.contains("kumotry")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 17, 18, 0))
  }

  it should "parse Regis Bochnia on a venue's own domain + strip the /napisy/ tag" in {
    val movies = visual9("https://bilety.kino.bochnia.pl", KinoRegis)
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoRegis)
    // "STRASZNY FILM /napisy/" → version tag stripped, sentence-cased, so the
    // dubbed + subtitled screenings of one film fold into a single row.
    val film = movies.find(_.movie.title.toLowerCase.contains("dzień objawienia")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 17, 17, 0))
    // booking link is the VisualSoft kup-bilet deep link
    film.showtimes.flatMap(_.bookingUrl).head should include("kup-bilet")
  }

  // Kino Kadr (Staszów, sta.systembiletowy.pl) — found in the 2026-09-23
  // nearby-towns sweep (assigned to Ostrowiec Świętokrzyski's catchment) and
  // verified against the live site: real dated screenings through mid-October
  // 2026, on the same visual9 skin as Kawiarnia/Centrum 3D/Regis above. Its own
  // fixture directory (not the shared 08-06-2026 corpus the other visual9
  // venues replay), captured live 2026-09-23.
  "SystemBiletowyClient (visual9 skin)" should "parse Kino Kadr Staszów (sta.systembiletowy.pl)" in {
    val movies = new SystemBiletowyClient(
      new FakeHttpFetch("kino-kadr-staszow"), "https://sta.systembiletowy.pl", KinoKadrStaszow,
      titles = titleNormalizer).fetch()
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKadrStaszow)
    val film = movies.find(_.movie.title.toLowerCase.contains("psi patrol")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 25, 15, 40))
    film.showtimes.flatMap(_.bookingUrl).head should include("kup-bilet")
  }

  it should "carry the stripped language onto each showing as a format badge, merging the editions" in {
    // The version tag was already stripped from the title; now it's also surfaced
    // as a per-screening format, so the dubbed + subtitled showings share one row
    // AND keep their language.
    val html =
      """<div class="event-item" data-date="2026-07-02" data-time="18:00">
        |<h3 class="event-title">Toy Story 5 - dubbing</h3><a href="/kup-bilet/1">buy</a></div>
        |<div class="event-item" data-date="2026-07-02" data-time="20:00">
        |<h3 class="event-title">Toy Story 5 - napisy</h3><a href="/kup-bilet/2">buy</a></div>""".stripMargin
    val movies = SystemBiletowyClient.parse(html, KinoKawiarnia, "https://kawiarnia.systembiletowy.pl", titleNormalizer)
    movies should have size 1
    movies.head.movie.title.toLowerCase        should include("toy story 5")
    movies.head.showtimes.map(_.format).toSet shouldBe Set(List("DUB"), List("NAP"))
  }

  // ── BCKino (Bytom) — visual9 skin, but the venue also sells theatre/
  // workshops/concerts through the same listing, tagged by a `data-group`
  // attribute per event ("BCKino" for films). `filmGroups` keeps only those and
  // peels the "BCKino – " title prefix the listing glues on. ──────────────────
  private val bck =
    new SystemBiletowyClient(new FakeHttpFetch("bck-bytom"), "https://bck.systembiletowy.pl", KinoBCKBytom,
      titles = titleNormalizer, filmGroups = Set("BCKino")).fetch()

  "SystemBiletowyClient (filmGroups)" should "keep only the events in the given data-group, stripping its title prefix" in {
    bck should not be empty
    bck.map(_.cinema).toSet shouldBe Set(KinoBCKBytom)
    val film = bck.find(_.movie.title.toLowerCase.contains("kandydaci")).value
    film.movie.title.toLowerCase should not include "bckino"
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 23, 18, 0))
  }

  it should "drop the venue's non-film events (workshops, book club, concerts, author talks)" in {
    // "BAŚKA tworzy: Słoik mocy" (data-group="Warsztaty") and the reading-club
    // "BECEK CZYTA" events carry no film-vocabulary the national classifier
    // would catch — only the data-group filter keeps them out.
    bck.map(_.movie.title.toLowerCase).exists(_.contains("baśka")) shouldBe false
    bck should have size 8   // the 8 events tagged data-group="BCKino" in the fixture
  }

  // ── Kino Orzeł (Ustrzyki Dolne) — the "repertoire-once" skin: one
  // div.repertoire-once.row per screening, with a "-Film"/"- Film" boilerplate
  // word ahead of the format tag. ─────────────────────────────────────────────
  private val orzel =
    new SystemBiletowyClient(new FakeHttpFetch("kino-orzel-ustrzyki"), "https://udk.systembiletowy.pl", KinoOrzelUstrzyki,
      titles = titleNormalizer).fetch()

  "SystemBiletowyClient (repertoire-once skin)" should "parse the Ustrzyki Dolne instance, stripping the '-Film' boilerplate" in {
    orzel should not be empty
    orzel.map(_.cinema).toSet shouldBe Set(KinoOrzelUstrzyki)
    val film = orzel.find(_.movie.title.toLowerCase.contains("mistyczka")).value
    film.movie.title.toLowerCase should not include "film"
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 4, 19, 0))
    film.showtimes.flatMap(_.bookingUrl).head should include("repertoire.html?id=")
  }

  it should "peel the dubbing/2D format tags off a '- Film' suffixed title into a format badge" in {
    val film = orzel.find(_.movie.title.toLowerCase.contains("podręcznik")).value
    film.showtimes.map(_.format) should contain(List("2D", "DUB"))
    // 03.10 dubbed screening + the same film's 02.10 screening merge onto one row.
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 10, 2, 17, 0), LocalDateTime.of(2026, 10, 3, 17, 0)
    )
  }
}
