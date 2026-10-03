package clients.ekobilet

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{KinoCKiTIlza, KinoDKGora, KinoJaworzyna, KinoMeduza, KinoMilenium, KinoOpolanka, KinoRadosc, KinoRejs, KinoStarowka, KinoTon, KinoWielickaMediateka, KinoZaciszeWasosz}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.EkobiletClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded ekobilet.pl landing + per-film detail pages for Kino
 *  Meduza (Opole) through the client, proving the two-fetch path recovers dated
 *  showtimes that live only on the detail pages. `today` is pinned to the
 *  fixture capture date so the year-inference resolves into June 2026.
 *
 *  Kino Meduza was previously scraped from Filmweb. */
class EkobiletClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new EkobiletClient(new FakeHttpFetch("kino-meduza"), "opolskielamy", KinoMeduza,
      today = LocalDate.of(2026, 6, 8)).fetch()

  "EkobiletClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMeduza)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening read off the film detail page" in {
    val film = movies.find(_.movie.title == "Młode matki").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 10, 18, 0))
    film.showtimes.flatMap(_.bookingUrl).head should startWith("https://ekobilet.pl/")
  }

  it should "strip format tags from titles (no '2D napisy' suffix)" in {
    movies.map(_.movie.title).foreach(_.toLowerCase should not include "2d napisy")
  }

  /** Kino Jaworzyna captured on a day the venue was dark (11.06.2026): the bare
   *  landing renders "Brak wydarzeń na dzisiaj" with zero `event-card`s, so the
   *  films live only behind the date strip's `?date=` pages. Before the per-day
   *  sweep this returned an empty list; now it recovers the full repertoire. */
  private val jaworzyna =
    new EkobiletClient(new FakeHttpFetch("ekobilet-jaworzyna"), "kino-jaworzyna", KinoJaworzyna,
      today = LocalDate.of(2026, 6, 11)).fetch()

  it should "sweep the date strip when today's landing is empty" in {
    // The bare landing fixture is the live "Brak wydarzeń na dzisiaj" page with
    // zero event-cards; before the date-strip sweep this returned an empty list.
    jaworzyna should not be empty
    jaworzyna.map(_.cinema).toSet shouldBe Set(KinoJaworzyna)
    all(jaworzyna.map(_.showtimes)) should not be empty
  }

  it should "pin a future-day screening discovered only via the date strip" in {
    val film = jaworzyna.find(_.movie.title == "Milczenie owiec").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 18, 20))
    film.showtimes.flatMap(_.bookingUrl).head should startWith("https://ekobilet.pl/")
  }

  // ── Per-film detail page (deferred enrichment) ─────────────────────────────

  private val jaworzynaClient =
    new EkobiletClient(new FakeHttpFetch("ekobilet-jaworzyna"), "kino-jaworzyna", KinoJaworzyna,
      today = LocalDate.of(2026, 6, 11))

  it should "fail, not report an empty listing, when every date-strip page fails behind an empty landing" in {
    val replay = new FakeHttpFetch("ekobilet-jaworzyna")
    val stripDown = new tools.HttpFetch {
      def get(url: String): String =
        if (url.contains("?date=")) throw new java.io.IOException(s"proxy: Tunnel failed ($url)") else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    a[java.io.IOException] should be thrownBy
      new EkobiletClient(stripDown, "kino-jaworzyna", KinoJaworzyna, today = LocalDate.of(2026, 6, 11)).fetch()
  }

  it should "read a row still on the page the day after it screened as past, not as next year" in {
    // Captured 11 June; read on 13 June, the 12 June rows must stay in 2026.
    val dayLate = new EkobiletClient(new FakeHttpFetch("ekobilet-jaworzyna"), "kino-jaworzyna", KinoJaworzyna,
      today = LocalDate.of(2026, 6, 13)).fetch()
    dayLate.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "expose each film's detail page as filmUrl for deferred enrichment" in {
    jaworzyna.flatMap(_.filmUrl) should not be empty
    all(jaworzyna.flatMap(_.filmUrl).map(_.startsWith("https://ekobilet.pl/kino-jaworzyna/"))) shouldBe true
  }

  it should "harvest the synopsis off the detail page reached via filmUrl" in {
    // Take the film's own filmUrl (the ref the listing scrape leaves) and run the
    // deferred fetchFilmDetail against it, exactly as the EnrichDetails task does.
    val ref    = jaworzyna.find(_.movie.title == "Milczenie owiec").value.filmUrl.value
    val detail = jaworzynaClient.fetchFilmDetail(ref).value
    detail.synopsis.value shouldBe
      "Seryjny morderca i inteligentna agentka łączą siły, by znaleźć przestępcę obdzierającego ze skóry swoje ofiary."
    // Jaworzyna's page has no leading metadata paragraph (see the Kino Rejs case
    // below), so nothing beyond the synopsis is invented.
    detail.releaseYear shouldBe None
    detail.director    shouldBe empty
    detail.cast        shouldBe empty
    detail.countries   shouldBe empty
  }

  it should "read the film synopsis only, not the venue's own about-the-cinema blurb" in {
    val ref    = jaworzyna.find(_.movie.title == "Romeria").value.filmUrl.value
    val detail = jaworzynaClient.fetchFilmDetail(ref).value
    detail.synopsis.value should startWith("Marina wraca do rodzinnej Galicji")
    detail.synopsis.value should not include "Małopolskiej Sieci Kin Cyfrowych" // the venue blurb
  }

  // ── 2026-09-27 detail pages with and without the metadata paragraph ────────
  // Some venues (Kino Rejs, Słupsk) open the off-canvas info panel with a
  // metadata paragraph — "<strong>Francja, Belgia 2026, 88 min</strong><br>
  // <strong>reżyseria: </strong>Philippe Riche" — BEFORE the synopsis <p>.
  // Reading the first <p> stored that line as the synopsis and dropped the plot.
  private val rejsClient =
    new EkobiletClient(new FakeHttpFetch("ekobilet-detail"), "kinorejs", KinoRejs, today = LocalDate.of(2026, 9, 27))

  it should "read the synopsis past a leading metadata paragraph and parse that paragraph's fields" in {
    val detail = rejsClient.fetchFilmDetail("https://ekobilet.pl/kinorejs/luna-i-rozgadana-swinka-63624").value
    detail.synopsis.value should startWith("Jedenastoletnia Luna, fanka serii książek")
    detail.synopsis.value should not include "reżyseria"
    detail.countries      shouldBe Seq("Francja", "Belgia")
    detail.releaseYear    shouldBe Some(2026)
    detail.runtimeMinutes shouldBe Some(88)
    detail.director       shouldBe Seq("Philippe Riche")
  }

  it should "keep an ordinary synopsis-only detail page's synopsis and invent no metadata" in {
    val tonClient = new EkobiletClient(new FakeHttpFetch("ekobilet-detail"), "kinoton", KinoTon, today = LocalDate.of(2026, 9, 27))
    val detail = tonClient.fetchFilmDetail("https://ekobilet.pl/kinoton/ice-cream-man-63797").value
    detail.synopsis.value should startWith("Fabuła filmu przenosi nas do idyllicznego, wakacyjnego miasteczka")
    detail.countries      shouldBe empty
    detail.releaseYear    shouldBe None
    detail.runtimeMinutes shouldBe None
    detail.director       shouldBe empty
  }

  it should "return None when the detail-page fetch fails (no fixture)" in {
    jaworzynaClient.fetchFilmDetail("https://ekobilet.pl/kino-jaworzyna/nie-ma-takiego-filmu-99999") shouldBe None
  }

  // ── 2026-09-23 nearby-towns sweep ──────────────────────────────────────────
  // Five venues found in that sweep, all still on ekobilet.pl, on two skins:
  //   - the card-grid skin (Góra, Żuromin) the client already handled above.
  //   - the "chrono-row" skin (Wąsosz, Milejów, Opole Lubelskie) — a flat
  //     per-SHOWTIME landing list carrying its own title inline and no separate
  //     film detail page — which `EkobiletClient.parseChronoRows` was added to
  //     serve. `filmUrl` staying `None` on these three is itself the proof the
  //     chrono-row branch ran, not the card-grid one.
  private val gora =
    new EkobiletClient(new FakeHttpFetch("ekobilet-gora"), "dom-kultury-w-gorze-7114", KinoDKGora,
      today = LocalDate.of(2026, 9, 23)).fetch()

  "EkobiletClient (nearby-towns sweep)" should "parse Dom Kultury w Górze off the card-grid skin" in {
    gora should not be empty
    gora.map(_.cinema).toSet shouldBe Set(KinoDKGora)
    val film = gora.find(_.movie.title.toLowerCase.contains("folwark zwierzęcy")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 25, 17, 0))
    film.filmUrl.value should startWith("https://ekobilet.pl/dom-kultury-w-gorze-7114/")
  }

  private val zuromin =
    new EkobiletClient(new FakeHttpFetch("ekobilet-zuromin"), "kinoton", KinoTon,
      today = LocalDate.of(2026, 9, 23)).fetch()

  it should "parse Kino Ton (Żuromin) off the card-grid skin" in {
    zuromin should not be empty
    zuromin.map(_.cinema).toSet shouldBe Set(KinoTon)
    val film = zuromin.find(_.movie.title.toLowerCase.contains("psi patrol")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 2, 17, 0))
    film.filmUrl.value should startWith("https://ekobilet.pl/kinoton/")
  }

  // Iłża's first screening sits two weeks out (2026-10-08), so the scrape must
  // reach past the default near-term days to find anything at all.
  private lazy val ilza =
    new EkobiletClient(new FakeHttpFetch("ekobilet-ilza"), "centrum-kultury-i-turystyki-w-ilzy-8211", KinoCKiTIlza,
      today = LocalDate.of(2026, 9, 23)).fetch()

  it should "parse Kino CKiT (Iłża), whose programme starts two weeks out" in {
    ilza.map(_.movie.title) should contain allOf ("Lalka", "Mistyczka", "Toy Story 5")
    ilza.map(_.cinema).toSet shouldBe Set(KinoCKiTIlza)
    all(ilza.flatMap(_.showtimes).map(_.dateTime.toLocalDate)) should be >= LocalDate.of(2026, 10, 8)
    ilza.find(_.movie.title == "Lalka").value.showtimes should have size 4
  }

  private val wasosz =
    new EkobiletClient(new FakeHttpFetch("ekobilet-wasosz"), "zpkwasosz", KinoZaciszeWasosz,
      today = LocalDate.of(2026, 9, 23)).fetch()

  it should "parse Kino Zacisze (Wąsosz) off the chrono-row landing skin — no card grid, no detail page" in {
    wasosz should not be empty
    wasosz.map(_.cinema).toSet shouldBe Set(KinoZaciszeWasosz)
    val film = wasosz.find(_.movie.title == "Tedi i magiczna lampa").value
    film.showtimes.map(_.dateTime) should contain allOf(
      LocalDateTime.of(2026, 9, 25, 13, 0), LocalDateTime.of(2026, 9, 25, 15, 0))
    film.showtimes.flatMap(_.bookingUrl).head should include("bilety-na-film")
    film.filmUrl shouldBe None
  }

  private val milejow =
    new EkobiletClient(new FakeHttpFetch("ekobilet-milejow"), "kino-milenium", KinoMilenium,
      today = LocalDate.of(2026, 9, 23)).fetch()

  it should "parse Kino Milenium (Milejów) off the chrono-row skin" in {
    milejow should not be empty
    milejow.map(_.cinema).toSet shouldBe Set(KinoMilenium)
    val film = milejow.find(_.movie.title.toLowerCase.contains("pucio kocha zwierzaki")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 26, 12, 0))
    film.filmUrl shouldBe None
  }

  private val opoleLubelskie =
    new EkobiletClient(new FakeHttpFetch("ekobilet-opole-lubelskie"), "ock-opolelubelskie", KinoOpolanka,
      today = LocalDate.of(2026, 9, 23)).fetch()

  it should "parse Kino Opolanka (Opole Lubelskie) off the chrono-row skin" in {
    opoleLubelskie should not be empty
    opoleLubelskie.map(_.cinema).toSet shouldBe Set(KinoOpolanka)
    val film = opoleLubelskie.find(_.movie.title.toLowerCase.contains("niebo nad normandią")).value
    film.showtimes.map(_.dateTime) should contain allOf(
      LocalDateTime.of(2026, 9, 25, 20, 0), LocalDateTime.of(2026, 9, 26, 20, 0), LocalDateTime.of(2026, 9, 27, 20, 0))
    film.filmUrl shouldBe None
  }

  // Venues that moved off Filmweb on 2026-09-27 (fixtures captured that day).
  // Kino Starówka and Wielicka Mediateka tag every title with pipe segments —
  // age, "PREMIERA!!!", version, "PL" — which went to TMDB whole and never
  // resolved ("Lalka | 13+ | PREMIERA!!!"). The film is the part before them;
  // the age becomes the rating and the version the showtimes' format.
  private val switchDay = LocalDate.of(2026, 9, 27)
  private val starowka =
    new EkobiletClient(new FakeHttpFetch("filmweb-only-switch"), "kinostarowka", KinoStarowka, today = switchDay).fetch()

  it should "peel Kino Starówka's age, premiere and version pipe tags off the title" in {
    starowka.map(_.movie.title) should contain allOf (
      "Lalka", "Vincent. Legenda oceanu", "Wtorek z klasyką: Asterix i Obelix: Misja Kleopatra", "Tony", "Obcy")
    all (starowka.map(_.movie.title)) should not include "|"
    val lalka = starowka.find(_.movie.title == "Lalka").value
    lalka.ageRating.value shouldBe "13+"
    lalka.movie.rawTitle.value shouldBe "Lalka | 13+ | PREMIERA!!!"
    starowka.find(_.movie.title.startsWith("Wtorek z klasyką: Asterix")).value
      .showtimes.map(_.format).distinct shouldBe Seq(List("DUB"))
    starowka.find(_.movie.title == "Tony").value.showtimes.map(_.format).distinct shouldBe Seq(List("NAP"))
  }

  it should "peel Wielicka Mediateka's version pipe tags off the chrono rows" in {
    val wieliczka =
      new EkobiletClient(new FakeHttpFetch("filmweb-only-switch"), "kino-wielicka-mediateka", KinoWielickaMediateka,
        today = switchDay).fetch()
    wieliczka.map(_.movie.title) should contain allOf ("Lalka", "Z klasą do kina: Lalka")
    all (wieliczka.map(_.movie.title)) should not include "|"
    wieliczka.find(_.movie.title == "Z klasą do kina: Lalka").value.showtimes.map(_.format).distinct shouldBe Seq(List("2D"))
  }

  // Kino Radość (DK Wolbrom) sells its concerts and plays on the same chrono
  // landing, and some carry no event word in the title ("Grzegorz Turnau").
  // ekobilet's booking link names the kind of ticket — "…-bilety-na-film" for
  // every film, "…-bilety-na-koncert" / "…-bilety-na-spektakl" otherwise — so
  // that, not the title, decides. ("Genialny pomysł" is Sébastien Castro's stage
  // comedy, ticketed as a spektakl.)
  it should "drop concerts and plays by the ticket kind their booking link names" in {
    val radosc =
      new EkobiletClient(new FakeHttpFetch("filmweb-only-switch"), "dk-wolbrom", KinoRadosc, today = switchDay).fetch()
    val titles = radosc.map(_.movie.title)
    titles should contain allOf ("Lalka", "Folwark zwierzęcy", "Tedi i magiczna lampa")
    titles should contain noneOf ("Grzegorz Turnau", "Zespół Pieśni i Tańca Śląsk / Podróże ze Śląskiem", "Genialny pomysł")
    titles.exists(_.startsWith("André Rieu")) shouldBe true
  }

  it should "keep a screened concert broadcast even when it is ticketed as a concert" in {
    val rieu = wasosz.find(_.movie.title.toLowerCase.contains("rieu")).value
    rieu.showtimes.flatMap(_.bookingUrl).head should endWith("bilety-na-koncert")
  }
}
