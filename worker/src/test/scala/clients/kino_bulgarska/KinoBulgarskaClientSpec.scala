package clients.kino_bulgarska

import clients.tools.FakeHttpFetch
import models.{KinoBulgarska, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoBulgarskaClient
import services.movies.TitleNormalizer

import java.time.LocalDateTime

class KinoBulgarskaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoBulgarskaClient(new FakeHttpFetch("kino-bulgarska"))
  // casing is applied centrally now (TitleNormalizer.recase); apply it here so assertions read display titles
  private val results = client.fetch()
    .map(cm => cm.copy(movie = cm.movie.copy(title = TitleNormalizer.recase(cm.movie.title))))
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "KinoBulgarskaClient.fetch" should "return exactly 9 movies from fixture" in {
    results.size shouldBe 9
  }

  it should "return 11 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 11
  }

  it should "assign KinoBulgarska cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(KinoBulgarska)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Chronologia wody",
      "Father mother sister brother",
      "La grazia",
      "Maryja. Matka papieża",
      "Miłość w czasach apokalipsy",
      "Na rauszu",
      "Ostatnia sesja w paryżu",
      "Sirât",
      "Wpatrując się w słońce",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("Chronologia wody")             shouldBe Some(128)
    runtimes("Father mother sister brother") shouldBe Some(110)
    runtimes("La grazia")                    shouldBe Some(131)
    runtimes("Maryja. Matka papieża")        shouldBe Some(80)
    runtimes("Miłość w czasach apokalipsy")  shouldBe Some(100)
    runtimes("Na rauszu")                    shouldBe Some(115)
    runtimes("Ostatnia sesja w paryżu")      shouldBe Some(105)
    runtimes("Sirât")                        shouldBe Some(120)
    runtimes("Wpatrując się w słońce")       shouldBe Some(149)
  }

  // ── Release years ─────────────────────────────────────────────────────────

  it should "return correct release year for every movie" in {
    byTitle("Chronologia wody").movie.releaseYear             shouldBe Some(2025)
    byTitle("Father mother sister brother").movie.releaseYear shouldBe Some(2025)
    byTitle("La grazia").movie.releaseYear                    shouldBe Some(2025)
    byTitle("Maryja. Matka papieża").movie.releaseYear        shouldBe Some(2026)
    byTitle("Miłość w czasach apokalipsy").movie.releaseYear  shouldBe Some(2025)
    byTitle("Na rauszu").movie.releaseYear                    shouldBe Some(2020)
    byTitle("Ostatnia sesja w paryżu").movie.releaseYear      shouldBe Some(2025)
    byTitle("Sirât").movie.releaseYear                        shouldBe Some(2025)
    byTitle("Wpatrując się w słońce").movie.releaseYear       shouldBe Some(2025)
  }

  // ── Production country ───────────────────────────────────────────────────

  it should "return correct production country for every movie" in {
    byTitle("Chronologia wody").movie.countries             shouldBe Seq("USA")
    byTitle("Father mother sister brother").movie.countries shouldBe Seq("USA")
    byTitle("La grazia").movie.countries                    shouldBe Seq("Włochy")
    byTitle("Maryja. Matka papieża").movie.countries        shouldBe Seq("Polska")
    byTitle("Miłość w czasach apokalipsy").movie.countries  shouldBe Seq("Kanada")
    byTitle("Na rauszu").movie.countries                    shouldBe Seq("Dania")
    byTitle("Ostatnia sesja w paryżu").movie.countries      shouldBe Seq("Francja")
    byTitle("Sirât").movie.countries                        shouldBe Seq("Hiszpania", "Francja")
    byTitle("Wpatrując się w słońce").movie.countries       shouldBe Seq("Niemcy")
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Chronologia wody")             shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/04/CHRONOLOGIA-WODY-PLAKAT-PL-clean.jpg")
    posters("Father mother sister brother") shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2025/12/FMSB-plakat-PL_HQ.jpg")
    posters("La grazia")                    shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/01/la-grazia-plakat-b1-lq_1.jpg")
    posters("Maryja. Matka papieża")        shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/03/maryja-matka-papieza-scaled.jpg")
    posters("Miłość w czasach apokalipsy")  shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/04/milosc-w-czasach-apo_plakat.jpg")
    posters("Na rauszu")                    shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2021/02/NaRauszu_plakat_OSCAR_final.jpg")
    posters("Ostatnia sesja w paryżu")      shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/03/ostatnia-sesja-w-paryzu-plakat-z-data-11.jpg")
    posters("Sirât")                        shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2025/09/sirat-polski-plakat-1-scaled.jpg")
    posters("Wpatrując się w słońce")       shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2025/12/1766741798010.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Chronologia wody")             shouldBe Some("http://kinobulgarska19.pl/filmy/chronologia-wody")
    filmUrls("Father mother sister brother") shouldBe Some("http://kinobulgarska19.pl/filmy/father-mother-sister-brother")
    filmUrls("La grazia")                    shouldBe Some("http://kinobulgarska19.pl/filmy/la-grazia")
    filmUrls("Maryja. Matka papieża")        shouldBe Some("http://kinobulgarska19.pl/filmy/maryja-matka-papieza")
    filmUrls("Miłość w czasach apokalipsy")  shouldBe Some("http://kinobulgarska19.pl/filmy/milosc-w-czasach-apokalipsy")
    filmUrls("Na rauszu")                    shouldBe Some("http://kinobulgarska19.pl/filmy/na-rauszu")
    filmUrls("Ostatnia sesja w paryżu")      shouldBe Some("http://kinobulgarska19.pl/filmy/ostatnia-sesja-w-paryzu")
    filmUrls("Sirât")                        shouldBe Some("http://kinobulgarska19.pl/filmy/sirat")
    filmUrls("Wpatrując się w słońce")       shouldBe Some("http://kinobulgarska19.pl/filmy/wpatrujac-sie-w-slonce")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for every movie" in {
    byTitle("Chronologia wody").director             shouldBe Seq("Kristen Stewart")
    byTitle("Father mother sister brother").director shouldBe Seq("Jim Jarmusch")
    byTitle("La grazia").director                    shouldBe Seq("Paolo Sorrentino")
    byTitle("Maryja. Matka papieża").director        shouldBe Seq("Jan Sobierajski")
    byTitle("Miłość w czasach apokalipsy").director  shouldBe Seq("Anne Émond")
    byTitle("Na rauszu").director                    shouldBe Seq("Thomas Vinterberg")
    byTitle("Ostatnia sesja w paryżu").director      shouldBe Seq("Rebecca Zlotowski")
    byTitle("Sirât").director                        shouldBe Seq("Oliver Laxe")
    byTitle("Wpatrując się w słońce").director       shouldBe Seq("Mascha Schilinski")
  }

  // ── Synopses ──────────────────────────────────────────────────────────────

  it should "extract a non-empty synopsis for every movie" in {
    results.foreach { cm =>
      cm.synopsis should not be empty
      cm.synopsis.get.length should be > 50
    }
  }

  it should "extract the exact synopsis for Maryja. matka papieża" in {
    byTitle("Maryja. Matka papieża").synopsis shouldBe Some(
      "Pierwszy film ukazujący mistyczną relację Karola Wojtyły z Matką Bożą. To Ona prowadziła Jana Pawła II przez wszystkie dni jego pontyfikatu. W filmie ukazane są uznane objawienia Maryjne, w których ważnym elementem jest aktualne zagrożenie ze strony Rosji. Od wypełnienia orędzi Matki Bożej…"
    )
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Chronologia wody")             shouldBe 2
    counts("Father mother sister brother") shouldBe 1
    counts("La grazia")                    shouldBe 1
    counts("Maryja. Matka papieża")        shouldBe 1
    counts("Miłość w czasach apokalipsy")  shouldBe 1
    counts("Na rauszu")                    shouldBe 1
    counts("Ostatnia sesja w paryżu")      shouldBe 2
    counts("Sirât")                        shouldBe 1
    counts("Wpatrując się w słońce")       shouldBe 1
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for Chronologia wody" in {
    val st = byTitle("Chronologia wody").showtimes
    st.size shouldBe 2
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 16, 30), None, Some("Sala Dzienniki Motocyklowe"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 14, 17, 50), None, Some("Sala Dzienniki Motocyklowe"), Nil),
    )
  }

  it should "return exact showtimes for Ostatnia sesja w paryżu" in {
    val st = byTitle("Ostatnia sesja w paryżu").showtimes
    st.size shouldBe 2
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 18, 40), None, Some("Sala Dzienniki Motocyklowe"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 14, 16, 0), None, Some("Sala Dzienniki Motocyklowe"), Nil),
    )
  }

  // ── Suffix stripping (normalizeTitle) ─────────────────────────────────────
  //
  // The page tacks decoration suffixes onto raw titles ("DRZEWO MAGII –
  // Pokazy przedpremierowe – Kino dzieci"). All must be stripped before the
  // title reaches the cache so the row merges with the same film reported
  // un-decorated by other cinemas.

  // casing is applied centrally now (TitleNormalizer.recase); wrap so the assertions read display titles
  "KinoBulgarskaClient.normalizeTitle" should "strip a single '– pokazy przedpremierowe' suffix" in {
    TitleNormalizer.recase(KinoBulgarskaClient.normalizeTitle("POSŁANI – Pokazy przedpremierowe")) shouldBe "Posłani"
  }

  it should "strip a single '– pokaz przedpremierowy' suffix" in {
    TitleNormalizer.recase(KinoBulgarskaClient.normalizeTitle("FILM – Pokaz przedpremierowy")) shouldBe "Film"
  }

  it should "strip a single '– kino dzieci' suffix" in {
    TitleNormalizer.recase(KinoBulgarskaClient.normalizeTitle("PUCIO – Kino dzieci")) shouldBe "Pucio"
  }

  it should "strip chained '– pokazy przedpremierowe – kino dzieci' suffixes" in {
    TitleNormalizer.recase(KinoBulgarskaClient.normalizeTitle("DRZEWO MAGII – Pokazy przedpremierowe – Kino dzieci")) shouldBe "Drzewo magii"
  }

  it should "still strip the legacy '– poznańska premiera' suffix" in {
    TitleNormalizer.recase(KinoBulgarskaClient.normalizeTitle("ROMERÍA – Poznańska premiera")) shouldBe "Romería"
  }

  it should "leave plain titles untouched apart from sentence-casing" in {
    TitleNormalizer.recase(KinoBulgarskaClient.normalizeTitle("CHRONOLOGIA WODY")) shouldBe "Chronologia wody"
  }

  // ── Trailers ──────────────────────────────────────────────────────────────
  //
  // Bulgarska's per-film page embeds the YouTube trailer in an oEmbed
  // `<iframe class="..." src="https://www.youtube.com/embed/<id>?…">`. The
  // client canonicalises to a `watch?v=<id>` URL and the view reshapes back
  // to /embed/ at render time.
  //
  // Detail-page fixtures recorded for 4 of the 9 films in the repertuar
  // fixture. The other 5 detail pages aren't recorded, so their trailerUrl
  // stays None (the parallel fetch swallows the FileNotFoundException).

  it should "extract YouTube trailers from per-film detail pages" in {
    client.fetchFilmDetail(byTitle("Chronologia wody").filmUrl.getOrElse(fail("no filmUrl for Chronologia wody")))
      .getOrElse(fail("no detail for Chronologia wody")).trailerUrl shouldBe Some("https://www.youtube.com/watch?v=H-ok8moY5QA")
    client.fetchFilmDetail(byTitle("Miłość w czasach apokalipsy").filmUrl.getOrElse(fail("no filmUrl for Miłość w czasach apokalipsy")))
      .getOrElse(fail("no detail for Miłość w czasach apokalipsy")).trailerUrl shouldBe Some("https://www.youtube.com/watch?v=QCxhy4UO5pc")
  }

  it should "leave trailerUrl None when the detail page is unavailable" in {
    // Maryja. matka papieża has no detail-page fixture → fetchFilmDetail returns None.
    client.fetchFilmDetail(byTitle("Maryja. Matka papieża").filmUrl.getOrElse(fail("no filmUrl for Maryja. matka papieża"))) shouldBe None
  }
}
