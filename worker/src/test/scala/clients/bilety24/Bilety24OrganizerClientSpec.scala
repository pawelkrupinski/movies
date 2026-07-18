package clients.bilety24

import models._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.Bilety24OrganizerClient

import java.time.LocalDateTime

/** Kino Kosmos, Kino Światowid and Kino Elektronik had their per-venue
 *  `*.bilety24.pl` subdomains decommissioned (DNS pointing at a dead host →
 *  red `ConnectException` bars on /uptime). bilety24.pl moved them onto the
 *  main domain at `www.bilety24.pl/kino/organizator/<slug>-<id>`. This replays
 *  each migrated organizer page through the new client and pins a concrete
 *  screening, proving one fetch recovers the full programme off the new URL. */
class Bilety24OrganizerClientSpec
    extends AnyFlatSpec
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  // (label, fixtureDirectory, organizerUrl, cinema, pinned title substring (lower-cased), pinned showtime)
  private val venues = Table(
    ("label", "dir", "url", "cinema", "title", "when"),
    ("Kino Kosmos", "kino-kosmos", "https://www.bilety24.pl/kino/organizator/kino-kosmos-1501",
      KinoKosmos: Cinema, "dzień objawienia", LocalDateTime.of(2026, 6, 11, 16, 30)),
    ("Kino Światowid", "kino-swiatowid", "https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503",
      KinoSwiatowid, "erupcja", LocalDateTime.of(2026, 6, 10, 16, 15)),
    ("Kino Elektronik", "kino-elektronik", "https://www.bilety24.pl/kino/organizator/kino-elektronik-631",
      KinoElektronik, "kumotry", LocalDateTime.of(2026, 6, 12, 18, 15)),
    ("Kino CK Jędrzejów", "kino-ck", "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458",
      KinoCK, "toy story 5", LocalDateTime.of(2026, 6, 19, 15, 0)),
    ("Kino Metalowiec Kraśnik", "kino-metalowiec", "https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529",
      KinoMetalowiec, "mandalorian i grogu", LocalDateTime.of(2026, 6, 12, 17, 0)),
    ("Kino Sokolnia Słupca", "kino-sokolnia", "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423",
      KinoSokolnia, "dzień objawienia", LocalDateTime.of(2026, 6, 12, 19, 0))
  )

  forAll(venues) { (label, directory, url, cinema, titleSub, when) =>
    lazy val movies = new Bilety24OrganizerClient(new FakeHttpFetch(directory), url, cinema).fetch()

    it should s"return a non-empty, single-cinema film list — $label" in {
      movies should not be empty
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      all(movies.map(_.showtimes)) should not be empty
    }

    it should s"pin a concrete screening that books on www.bilety24.pl — $label" in {
      val film = movies.find(_.movie.title.toLowerCase.contains(titleSub)).value
      val slot = film.showtimes.find(_.dateTime == when).value
      slot.bookingUrl.value should startWith("https://www.bilety24.pl/kino/")
    }
  }

  // ── Deferred per-film detail (DetailEnricher) ──────────────────────────────

  // bilety24 organizer rows carry no film identity beyond the title, so each
  // film's `/kino/<slug>` event page is exposed as `filmUrl` and fetched for
  // DISPLAY enrichment. Those pages turn out to hold only a synopsis and an
  // og:image poster — no structured year/director/cast — so the detail enriches
  // display without supplying a TMDB-identity hint. (Recorded event page:
  // https://www.bilety24.pl/kino/1501-dzien-objawienia-157217?id=935542)
  private val kosmos =
    new Bilety24OrganizerClient(new FakeHttpFetch("kino-kosmos"),
      "https://www.bilety24.pl/kino/organizator/kino-kosmos-1501", KinoKosmos)

  it should "expose each film's /kino event page as filmUrl for deferred detail" in {
    val film = kosmos.fetch().find(_.movie.title.toLowerCase.contains("dzień objawienia")).value
    film.filmUrl.value shouldBe "https://www.bilety24.pl/kino/1501-dzien-objawienia-157217?id=935542"
  }

  it should "harvest synopsis and poster off the event page (bilety24 exposes no year/director)" in {
    val ref    = kosmos.fetch().find(_.movie.title.toLowerCase.contains("dzień objawienia")).value.filmUrl.value
    val detail = kosmos.fetchFilmDetail(ref).value

    detail.synopsis.value should startWith("Gdybyś dowiedział się, że nie jesteśmy sami")
    detail.synopsis.value should include("Emily Blunt")
    detail.posterUrl.value shouldBe
      "https://image.bilety24.pl/original/dealer-default/1501/dzien-objawienia-emily-digital-1080x1920px.jpg"
    // The page has no structured identity block, so these stay empty — the row
    // resolves from the listing title, hence defersTmdbResolution is false.
    detail.releaseYear shouldBe None
    detail.director    shouldBe empty
    detail.cast        shouldBe empty
    kosmos.defersTmdbResolution shouldBe false
  }

  // Forum Bolesławiec glues the version word to the title with an underscore
  // ("Supergirl_dubbing", "Supergirl_napisy"). Those must un-glue, merge into one
  // film, and surface the version as a Showtime.format badge — otherwise each
  // language variant fragments into its own card with no format info.
  "Bilety24OrganizerClient.parse" should
    "merge underscore-glued format variants into one film and surface the format tokens" in {
    val html =
      """<html><body>
        |<a href="/kino/1-supergirl-10?id=1" title="Film: Supergirl_dubbing - 2026-06-19 18:00 - Bolesławiec">buy</a>
        |<a href="/kino/1-supergirl-11?id=2" title="Film: Supergirl_napisy - 2026-06-20 20:30 - Bolesławiec">buy</a>
        |</body></html>""".stripMargin
    val movies = Bilety24OrganizerClient.parse(html, KinoForumBoleslawiec)
    movies.map(_.movie.title) shouldBe Seq("Supergirl")
    movies.head.showtimes.map(_.format).toSet shouldBe Set(List("DUB"), List("NAP"))
  }

  // Forum Bolesławiec also glues a programme-strand marker with an underscore —
  // "Monterey Pop_DKF" (Dyskusyjny Klub Filmowy), "…_FKS". Unlike the format
  // words it carries no version meaning, so it must be stripped (not surfaced as
  // a badge) — otherwise the DKF screening becomes its own "Monterey Pop_DKF"
  // card instead of merging onto the regular "Monterey Pop" run.
  it should "strip a trailing _DKF/_FKS programme tag so the screening merges onto the clean title" in {
    val html =
      """<html><body>
        |<a href="/kino/1-monterey-pop-10?id=1" title="Film: Monterey Pop_DKF - 2026-06-29 19:00 - Bolesławiec">buy</a>
        |<a href="/kino/1-monterey-pop-11?id=2" title="Film: Monterey Pop - 2026-06-30 20:00 - Bolesławiec">buy</a>
        |</body></html>""".stripMargin
    val movies = Bilety24OrganizerClient.parse(html, KinoForumBoleslawiec)
    movies.map(_.movie.title) shouldBe Seq("Monterey Pop")
    movies.head.showtimes.map(_.format).toSet shouldBe Set(List.empty[String])
  }
}
