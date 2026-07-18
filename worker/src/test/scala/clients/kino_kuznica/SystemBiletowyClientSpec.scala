package clients.kino_kuznica

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{KinoCentrum3D, KinoFarys, KinoKawiarnia, KinoKuznica, KinoPckulKino, KinoRegis}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.SystemBiletowyClient

import java.time.LocalDateTime

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
    new SystemBiletowyClient(new FakeHttpFetch("kino-kuznica"), "https://shd.systembiletowy.pl", KinoKuznica).fetch()

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
    new SystemBiletowyClient(new FakeHttpFetch("kino-farys"), "https://kfb.systembiletowy.pl", KinoFarys).fetch()

  "SystemBiletowyClient (Farys)" should "parse the Biecz instance off the same client" in {
    farys should not be empty
    farys.map(_.cinema).toSet shouldBe Set(KinoFarys)
    val film = farys.find(_.movie.title.toLowerCase.contains("willow")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 15, 0))
  }

  // ── Alternate div.event-item skin (Kino PCKul, Pszczyna) ────────────────────
  private val pckul =
    new SystemBiletowyClient(new FakeHttpFetch("kino-pckul"), "https://bilety.pckul.pl", KinoPckulKino).fetch()

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
    new SystemBiletowyClient(new FakeHttpFetch("08-06-2026"), base, cinema).fetch()

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

  it should "carry the stripped language onto each showing as a format badge, merging the editions" in {
    // The version tag was already stripped from the title; now it's also surfaced
    // as a per-screening format, so the dubbed + subtitled showings share one row
    // AND keep their language.
    val html =
      """<div class="event-item" data-date="2026-07-02" data-time="18:00">
        |<h3 class="event-title">Toy Story 5 - dubbing</h3><a href="/kup-bilet/1">buy</a></div>
        |<div class="event-item" data-date="2026-07-02" data-time="20:00">
        |<h3 class="event-title">Toy Story 5 - napisy</h3><a href="/kup-bilet/2">buy</a></div>""".stripMargin
    val movies = SystemBiletowyClient.parse(html, KinoKawiarnia, "https://kawiarnia.systembiletowy.pl")
    movies should have size 1
    movies.head.movie.title.toLowerCase        should include("toy story 5")
    movies.head.showtimes.map(_.format).toSet shouldBe Set(List("DUB"), List("NAP"))
  }
}
