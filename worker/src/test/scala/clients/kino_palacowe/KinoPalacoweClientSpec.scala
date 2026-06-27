package clients.kino_palacowe

import clients.tools.FakeHttpFetch
import models.{KinoPalacowe, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, KinoPalacoweClient}
import tools.RoutingHttpFetch

import java.time.LocalDateTime

class KinoPalacoweClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title")))
      .getOrElse(fail(s"no detail for $title"))

  // ── Totals ────────────────────────────────────────────────────────────────

  "KinoPalacoweClient.fetch" should "return exactly 5 movies from fixture" in {
    results.size shouldBe 5
  }

  it should "return 6 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 6
  }

  it should "assign KinoPalacowe cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(KinoPalacowe)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Poranek dla dzieci: Chłopiec na krańcach świata",
      "Giulietta i duchy",
      "Głos z księżyca",
      "Osiem i pół",
      "Słodkie życie",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> detailFor(m.movie.title).runtimeMinutes).toMap
    runtimes("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe Some(88)
    runtimes("Giulietta i duchy")           shouldBe Some(139)
    runtimes("Głos z księżyca")             shouldBe Some(121)
    runtimes("Osiem i pół")                 shouldBe Some(138)
    runtimes("Słodkie życie")               shouldBe Some(176)
  }

  // The bare fetch() must already carry a runtime so a deferred-detail cinema's
  // listing isn't runtime-less: take the JSON `duration` when present, else parse
  // the `lead` prose ("Czas trwania: …") — accessibility screenings omit duration.
  it should "carry runtime on the bare fetch(), from JSON duration or the lead prose" in {
    val json =
      """{"next":false,"results":[{"subsections":[{"entries":[
        |  {"url":"/filmy/1-z-czasem","ticket_type":2,"title":"Z czasem",
        |   "start_date":"2026-06-16","start_time":"18:00","duration":120,"lead":"Krótki opis."},
        |  {"url":"/filmy/2-bez-duration","ticket_type":2,"title":"Bez duration",
        |   "start_date":"2026-06-16","start_time":"11:00",
        |   "lead":"Film. Czas trwania: 1 godzina i 56 minut. Kraj produkcji: Polska."}
        |]}]}]}""".stripMargin
    val movies = new KinoPalacoweClient(new RoutingHttpFetch(Map("calendar" -> json))).fetch()
    movies.size                               shouldBe 2
    movies.flatMap(_.movie.runtimeMinutes).toSet shouldBe Set(120, 116)  // 120 from `duration`; 116 = 1h56 from prose
  }

  // ── Director / country / year (parsed from "reż. … COUNTRY YEAR, NN'" meta line) ──

  it should "extract director from each film page's meta line" in {
    val directors = results.map(m => m.movie.title -> detailFor(m.movie.title).director).toMap
    // Multiple co-directors before the country are captured together.
    directors("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe Seq("Grzegorz Wacławek", "Marta Szymańska")
    directors("Giulietta i duchy")           shouldBe Seq("Federico Fellini")
    directors("Głos z księżyca")             shouldBe Seq("Federico Fellini")
    directors("Osiem i pół")                 shouldBe Seq("Federico Fellini")
    directors("Słodkie życie")               shouldBe Seq("Federico Fellini")
  }

  it should "extract countries (one or many) from each film page" in {
    val countries = results.map(m => m.movie.title -> detailFor(m.movie.title).countries).toMap
    countries("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe Seq("Polska")
    countries("Giulietta i duchy")           shouldBe Seq("Włochy", "Francja")
    countries("Głos z księżyca")             shouldBe Seq("Włochy")
    countries("Osiem i pół")                 shouldBe Seq("Włochy", "Francja")
    countries("Słodkie życie")               shouldBe Seq("Włochy", "Francja")
  }

  it should "extract release year from each film page" in {
    val years = results.map(m => m.movie.title -> detailFor(m.movie.title).releaseYear).toMap
    years("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe Some(2025)
    years("Giulietta i duchy")           shouldBe Some(1965)
    years("Głos z księżyca")             shouldBe Some(1990)
    years("Osiem i pół")                 shouldBe Some(1963)
    years("Słodkie życie")               shouldBe Some(1960)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/Chopiec_na_krancach_swiata_PLAKAT_teaser.jpg")
    posters("Giulietta i duchy")           shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/Giulietta_degli_2.jpg")
    posters("Głos z księżyca")             shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/La_voce_della_lunba_3.jpg")
    posters("Osiem i pół")                 shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/still2_zqnlD61.jpg")
    posters("Słodkie życie")               shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/still_4_PbgzW23.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe Some("http://kinopalacowe.pl/filmy/14435-poranek-dla-dzieci-chopiec-na-krancach-swiata/")
    filmUrls("Giulietta i duchy")           shouldBe Some("http://kinopalacowe.pl/filmy/14403-giulietta-i-duchy-federico-fellini-ciao-a-tutti/")
    filmUrls("Głos z księżyca")             shouldBe Some("http://kinopalacowe.pl/filmy/14404-gos-z-ksiezyca-federico-fellini-ciao-a-tutti/")
    filmUrls("Osiem i pół")                 shouldBe Some("http://kinopalacowe.pl/filmy/14402-osiem-i-po-federico-fellini-ciao-a-tutti/")
    filmUrls("Słodkie życie")               shouldBe Some("http://kinopalacowe.pl/filmy/14401-sodkie-zycie-federico-fellini-ciao-a-tutti/")
  }

  // ── Synopses ──────────────────────────────────────────────────────────────

  it should "extract a non-empty synopsis for every movie" in {
    results.foreach { cm =>
      withClue(s"${cm.movie.title}: ") {
        // KinoPałacowe exposes the lead text in the listing JSON (synopsis field
        // on the bare CinemaMovie), so synopsis is available without a detail fetch.
        cm.synopsis should not be empty
      }
    }
  }

  it should "extract correct synopsis for Chłopiec na krańcach świata" in {
    byTitle("Poranek dla dzieci: Chłopiec na krańcach świata").synopsis shouldBe Some(
      "Czy odważyłbyś się wyruszyć tam, gdzie kończy się świat, by uratować kogoś bliskiego? Omul rozumie mowę zwierząt, ale..."
    )
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Poranek dla dzieci: Chłopiec na krańcach świata") shouldBe 2
    counts("Giulietta i duchy")           shouldBe 1
    counts("Głos z księżyca")             shouldBe 1
    counts("Osiem i pół")                 shouldBe 1
    counts("Słodkie życie")               shouldBe 1
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for Chłopiec na krańcach świata" in {
    val st = byTitle("Poranek dla dzieci: Chłopiec na krańcach świata").showtimes
    st.size shouldBe 2
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 6, 27, 11, 0), Some("https://ckzamek.bilety24.pl/kup-bilety/?id=925494"), Some("Sala 1 - Kinowa"), Nil),
      Showtime(LocalDateTime.of(2026, 6, 28, 11, 0), Some("https://ckzamek.bilety24.pl/kup-bilety/?id=925496"), Some("Sala 1 - Kinowa"), Nil),
    )
  }

  it should "return exact showtime for Głos z księżyca" in {
    val st = byTitle("Głos z księżyca").showtimes
    st.size shouldBe 1
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 7, 17, 18, 0), Some("https://ckzamek.bilety24.pl/kup-bilety/?id=924779"), Some("Sala 1 - Kinowa"), Nil),
    )
  }

  // ── Trailers ──────────────────────────────────────────────────────────────
  //
  // Pałacowe embeds the YouTube watch URL in a `<a class="gallery__movie …">`
  // anchor on the film page. Among the 5 fixture films, only Chłopiec na
  // krańcach świata's page carries that anchor; the four Fellini-cycle pages
  // don't.

  it should "extract a canonical youtube watch URL from the film page" in {
    detailFor("Poranek dla dzieci: Chłopiec na krańcach świata").trailerUrl shouldBe
      Some("https://www.youtube.com/watch?v=MNwsZzIFF3s")
  }

  it should "leave trailerUrl None when the film page has no trailer anchor" in {
    detailFor("Słodkie życie").trailerUrl   shouldBe None
    detailFor("Osiem i pół").trailerUrl     shouldBe None
    detailFor("Giulietta i duchy").trailerUrl shouldBe None
    detailFor("Głos z księżyca").trailerUrl shouldBe None
  }

  // ── cleanTitle: strip cycle decoration ────────────────────────────────────
  //
  // Each strip lets a decorated screening merge onto — and enrich off — the
  // same canonical row as the regular run. Drop any one strip and the matching
  // assertion fails, exactly as it would have before the strip was added.

  // The "Poranek dla dzieci: " and "DKF Zamek: " banners are no longer stripped at
  // the client level — they're covered GLOBALLY now (the seed ProgrammePrefixPattern
  // 'Poranek dla dzieci' + ExtraTitleRules xtra-pp-dkf-named), query-only, so the
  // decorated screening keeps its own row. cleanTitle leaves them intact.
  "KinoPalacoweClient.cleanTitle" should "leave the now-global Poranek / DKF Zamek banners intact" in {
    KinoPalacoweClient.cleanTitle("Poranek dla dzieci: Pucio") shouldBe "Poranek dla dzieci: Pucio"
    KinoPalacoweClient.cleanTitle("DKF Zamek: Belle") shouldBe "DKF Zamek: Belle"
  }

  it should "strip the 'WAJDA: re-wizje. ' retrospective prefix" in {
    KinoPalacoweClient.cleanTitle("WAJDA: re-wizje. Człowiek z marmuru") shouldBe "Człowiek z marmuru"
  }

  it should "leave an undecorated title untouched" in {
    KinoPalacoweClient.cleanTitle("Osiem i pół") shouldBe "Osiem i pół"
  }
}
