package clients.kino_bulgarska

import clients.KinoBulgarskaClient
import clients.tools.FakeHttpFetch
import models.{KinoBulgarska, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class KinoBulgarskaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoBulgarskaClient(new FakeHttpFetch("kino-bulgarska"))
  private val results = client.fetch()
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
      "Maryja. matka papieża",
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
    runtimes("Maryja. matka papieża")        shouldBe Some(80)
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
    byTitle("Maryja. matka papieża").movie.releaseYear        shouldBe Some(2026)
    byTitle("Miłość w czasach apokalipsy").movie.releaseYear  shouldBe Some(2025)
    byTitle("Na rauszu").movie.releaseYear                    shouldBe Some(2020)
    byTitle("Ostatnia sesja w paryżu").movie.releaseYear      shouldBe Some(2025)
    byTitle("Sirât").movie.releaseYear                        shouldBe Some(2025)
    byTitle("Wpatrując się w słońce").movie.releaseYear       shouldBe Some(2025)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Chronologia wody")             shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/04/CHRONOLOGIA-WODY-PLAKAT-PL-clean.jpg")
    posters("Father mother sister brother") shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2025/12/FMSB-plakat-PL_HQ.jpg")
    posters("La grazia")                    shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/01/la-grazia-plakat-b1-lq_1.jpg")
    posters("Maryja. matka papieża")        shouldBe Some("http://kinobulgarska19.pl/wp-content/uploads/2026/03/maryja-matka-papieza-scaled.jpg")
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
    filmUrls("Maryja. matka papieża")        shouldBe Some("http://kinobulgarska19.pl/filmy/maryja-matka-papieza")
    filmUrls("Miłość w czasach apokalipsy")  shouldBe Some("http://kinobulgarska19.pl/filmy/milosc-w-czasach-apokalipsy")
    filmUrls("Na rauszu")                    shouldBe Some("http://kinobulgarska19.pl/filmy/na-rauszu")
    filmUrls("Ostatnia sesja w paryżu")      shouldBe Some("http://kinobulgarska19.pl/filmy/ostatnia-sesja-w-paryzu")
    filmUrls("Sirât")                        shouldBe Some("http://kinobulgarska19.pl/filmy/sirat")
    filmUrls("Wpatrując się w słońce")       shouldBe Some("http://kinobulgarska19.pl/filmy/wpatrujac-sie-w-slonce")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for every movie" in {
    byTitle("Chronologia wody").director             shouldBe Some("Kristen Stewart")
    byTitle("Father mother sister brother").director shouldBe Some("Jim Jarmusch")
    byTitle("La grazia").director                    shouldBe Some("Paolo Sorrentino")
    byTitle("Maryja. matka papieża").director        shouldBe Some("Jan Sobierajski")
    byTitle("Miłość w czasach apokalipsy").director  shouldBe Some("Anne Émond")
    byTitle("Na rauszu").director                    shouldBe Some("Thomas Vinterberg")
    byTitle("Ostatnia sesja w paryżu").director      shouldBe Some("Rebecca Zlotowski")
    byTitle("Sirât").director                        shouldBe Some("Oliver Laxe, Hiszpania")
    byTitle("Wpatrując się w słońce").director       shouldBe Some("Mascha Schilinski")
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Chronologia wody")             shouldBe 2
    counts("Father mother sister brother") shouldBe 1
    counts("La grazia")                    shouldBe 1
    counts("Maryja. matka papieża")        shouldBe 1
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
      Showtime(LocalDateTime.of(2026, 5, 13, 16, 30), None, Some("Sala Dzienniki Motocyklowe"), None),
      Showtime(LocalDateTime.of(2026, 5, 14, 17, 50), None, Some("Sala Dzienniki Motocyklowe"), None),
    )
  }

  it should "return exact showtimes for Ostatnia sesja w paryżu" in {
    val st = byTitle("Ostatnia sesja w paryżu").showtimes
    st.size shouldBe 2
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 18, 40), None, Some("Sala Dzienniki Motocyklowe"), None),
      Showtime(LocalDateTime.of(2026, 5, 14, 16, 0), None, Some("Sala Dzienniki Motocyklowe"), None),
    )
  }
}
