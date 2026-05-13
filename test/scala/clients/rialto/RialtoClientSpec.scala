package clients.rialto

import clients.RialtoClient
import clients.tools.FakeHttpFetch
import models.{Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class RialtoClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new RialtoClient(new FakeHttpFetch("rialto"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "RialtoClient.fetch" should "return exactly 14 movies from fixture" in {
    results.size shouldBe 14
  }

  it should "return 231 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 231
  }

  it should "assign Rialto cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(Rialto)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Ale mam | ну мам | wersja ukraińska z ang. napisami",
      "Człowiek z marmuru",
      "Diabeł ubiera się u prady 2",
      "Filmowe spotkania z psychoanalizą: dobry chłopiec",
      "Mavka. prawdziwy mit",
      "Modigliani: portret odarty z legendy",
      "Munch: miłość, duchy i wampirzyce",
      "Młode matki",
      "Niesamowite przygody skarpetek 3. ale kosmos!",
      "Szepty lasu",
      "Sprawiedliwość owiec",
      "Top gun | 40 rocznica",
      "Van gogh. pola zbóż i zachmurzone niebiosa",
      "Znaki pana śliwki",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("Ale mam | ну мам | wersja ukraińska z ang. napisami") shouldBe Some(90)
    runtimes("Człowiek z marmuru")                                   shouldBe Some(156)
    runtimes("Diabeł ubiera się u prady 2")                         shouldBe Some(120)
    runtimes("Filmowe spotkania z psychoanalizą: dobry chłopiec")    shouldBe Some(110)
    runtimes("Mavka. prawdziwy mit")                                 shouldBe Some(100)
    runtimes("Modigliani: portret odarty z legendy")                 shouldBe Some(90)
    runtimes("Munch: miłość, duchy i wampirzyce")                   shouldBe Some(90)
    runtimes("Młode matki")                                          shouldBe Some(104)
    runtimes("Niesamowite przygody skarpetek 3. ale kosmos!")        shouldBe Some(55)
    runtimes("Szepty lasu")                                          shouldBe Some(84)
    runtimes("Sprawiedliwość owiec")                                 shouldBe Some(105)
    runtimes("Top gun | 40 rocznica")                                shouldBe Some(110)
    runtimes("Van gogh. pola zbóż i zachmurzone niebiosa")          shouldBe Some(85)
    runtimes("Znaki pana śliwki")                                    shouldBe Some(72)
  }

  // ── Release years ─────────────────────────────────────────────────────────

  it should "return correct release year for every movie" in {
    byTitle("Ale mam | ну мам | wersja ukraińska z ang. napisami").movie.releaseYear shouldBe Some(2026)
    byTitle("Człowiek z marmuru").movie.releaseYear                                   shouldBe Some(1976)
    byTitle("Diabeł ubiera się u prady 2").movie.releaseYear                         shouldBe Some(2026)
    byTitle("Filmowe spotkania z psychoanalizą: dobry chłopiec").movie.releaseYear    shouldBe Some(2025)
    byTitle("Mavka. prawdziwy mit").movie.releaseYear                                 shouldBe Some(2026)
    byTitle("Modigliani: portret odarty z legendy").movie.releaseYear                 shouldBe Some(2020)
    byTitle("Munch: miłość, duchy i wampirzyce").movie.releaseYear                   shouldBe Some(2022)
    byTitle("Młode matki").movie.releaseYear                                          shouldBe Some(2025)
    byTitle("Niesamowite przygody skarpetek 3. ale kosmos!").movie.releaseYear        shouldBe Some(2026)
    byTitle("Szepty lasu").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Sprawiedliwość owiec").movie.releaseYear                                 shouldBe Some(2026)
    byTitle("Top gun | 40 rocznica").movie.releaseYear                                shouldBe Some(1986)
    byTitle("Van gogh. pola zbóż i zachmurzone niebiosa").movie.releaseYear          shouldBe Some(2018)
    byTitle("Znaki pana śliwki").movie.releaseYear                                    shouldBe Some(2025)
  }

  // ── Production country ────────────────────────────────────────────────────

  it should "return correct production country for every movie" in {
    byTitle("Ale mam | ну мам | wersja ukraińska z ang. napisami").movie.country shouldBe Some("Ukraina")
    byTitle("Człowiek z marmuru").movie.country shouldBe Some("Polska")
    byTitle("Diabeł ubiera się u prady 2").movie.country shouldBe Some("USA")
    byTitle("Filmowe spotkania z psychoanalizą: dobry chłopiec").movie.country shouldBe Some("Polska, Wielka Brytania")
    byTitle("Mavka. prawdziwy mit").movie.country shouldBe Some("Ukraina")
    byTitle("Modigliani: portret odarty z legendy").movie.country shouldBe Some("Włochy")
    byTitle("Munch: miłość, duchy i wampirzyce").movie.country shouldBe Some("Wielka Brytania, Włochy")
    byTitle("Młode matki").movie.country shouldBe Some("Belgia, Francja")
    byTitle("Niesamowite przygody skarpetek 3. ale kosmos!").movie.country shouldBe Some("Polska, Portugalia")
    byTitle("Sprawiedliwość owiec").movie.country shouldBe Some("Irlandia, Niemcy, USA, Wielka Brytania")
    byTitle("Szepty lasu").movie.country shouldBe Some("Polska")
    byTitle("Top gun | 40 rocznica").movie.country shouldBe Some("USA")
    byTitle("Van gogh. pola zbóż i zachmurzone niebiosa").movie.country shouldBe Some("Włochy")
    byTitle("Znaki pana śliwki").movie.country shouldBe Some("Polska")
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Ale mam | ну мам | wersja ukraińska z ang. napisami") shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/a1a0-v3-1x-062logo.jpg")
    posters("Człowiek z marmuru")                                   shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/czlowiek-z-marmuru-plakat1.jpg")
    posters("Diabeł ubiera się u prady 2")                         shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/diabel-ubiera-sie-u-prady-2-plakat1.jpg")
    posters("Filmowe spotkania z psychoanalizą: dobry chłopiec")    shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/dobry-chlopiec-plakat.jpg")
    posters("Mavka. prawdziwy mit")                                 shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/untitled-1-copy.jpg")
    posters("Modigliani: portret odarty z legendy")                 shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/modigliani-plakat-pion-scaled.jpg")
    posters("Munch: miłość, duchy i wampirzyce")                   shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/munch-plakat-b1.jpg")
    posters("Młode matki")                                          shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/plakat-mlode-matki-700x1050.jpg")
    posters("Niesamowite przygody skarpetek 3. ale kosmos!")        shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/niesamowite-przygody-skarpetek-3-ale-kosmos-plakat-scaled.jpg")
    posters("Szepty lasu")                                          shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/szepty-lasu-plakat.jpg")
    posters("Sprawiedliwość owiec")                                 shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/plakat-sprawiedliwosc-owiec-internetjpg.jpg")
    posters("Top gun | 40 rocznica")                                shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/top-gun-rocznica-tg40th.jpg")
    posters("Van gogh. pola zbóż i zachmurzone niebiosa")          shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/vangogh-plakat.jpg")
    posters("Znaki pana śliwki")                                    shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/znaki-pana-sliwki-plakat.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Ale mam | ну мам | wersja ukraińska z ang. napisami") shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155841")
    filmUrls("Człowiek z marmuru")                                   shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155445")
    filmUrls("Diabeł ubiera się u prady 2")                         shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=154210")
    filmUrls("Filmowe spotkania z psychoanalizą: dobry chłopiec")    shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155850")
    filmUrls("Mavka. prawdziwy mit")                                 shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155441")
    filmUrls("Modigliani: portret odarty z legendy")                 shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=87240")
    filmUrls("Munch: miłość, duchy i wampirzyce")                   shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=95047")
    filmUrls("Młode matki")                                          shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155895")
    filmUrls("Niesamowite przygody skarpetek 3. ale kosmos!")        shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155443")
    filmUrls("Szepty lasu")                                          shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155439")
    filmUrls("Sprawiedliwość owiec")                                 shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=154188")
    filmUrls("Top gun | 40 rocznica")                                shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=154207")
    filmUrls("Van gogh. pola zbóż i zachmurzone niebiosa")          shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=48676")
    filmUrls("Znaki pana śliwki")                                    shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155877")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for films that have one" in {
    byTitle("Człowiek z marmuru").director                                shouldBe Some("Andrzej Wajda")
    byTitle("Diabeł ubiera się u prady 2").director                      shouldBe Some("David Frankel")
    byTitle("Filmowe spotkania z psychoanalizą: dobry chłopiec").director shouldBe Some("Jan Komasa")
    byTitle("Mavka. prawdziwy mit").director                              shouldBe Some("Katya Tsarik")
    byTitle("Modigliani: portret odarty z legendy").director              shouldBe Some("Valeria Parisi")
    byTitle("Munch: miłość, duchy i wampirzyce").director                shouldBe Some("Michele Mally")
    byTitle("Młode matki").director                                       shouldBe Some("Jean-Pierre Dardenne, Luc Dardenne")
    byTitle("Niesamowite przygody skarpetek 3. ale kosmos!").director     shouldBe Some("Paweł Wendorff, Elżbieta Wąsik; Jarosław Szyszko, Barbara Koniecka, Mateusz Kmieć, Natalia Bartska-Kmieć")
    byTitle("Szepty lasu").director                                       shouldBe Some("Krzysztof Sarapata, Tomasz Kotaś")
    byTitle("Sprawiedliwość owiec").director                              shouldBe Some("Kyle Balda")
    byTitle("Top gun | 40 rocznica").director                             shouldBe Some("Tony Scott")
    byTitle("Van gogh. pola zbóż i zachmurzone niebiosa").director       shouldBe Some("Giovanni Piscaglia")
    byTitle("Znaki pana śliwki").director                                 shouldBe Some("Urszula Morga, Bartosz Mikołajczyk")
  }

  // ── Synopses ──────────────────────────────────────────────────────────────

  it should "extract a non-empty synopsis for every movie" in {
    results.foreach { cm =>
      withClue(s"${cm.movie.title}: ") {
        cm.synopsis should not be empty
        cm.synopsis.get.length should be > 50
      }
    }
  }

  it should "extract correct synopsis for Człowiek z marmuru" in {
    byTitle("Człowiek z marmuru").synopsis.getOrElse("") should startWith(
      "Film jest syntezą najnowszej historii powojennej Polski."
    )
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Ale mam | ну мам | wersja ukraińska z ang. napisami") shouldBe 11
    counts("Człowiek z marmuru")                                   shouldBe 11
    counts("Diabeł ubiera się u prady 2")                         shouldBe 33
    counts("Filmowe spotkania z psychoanalizą: dobry chłopiec")    shouldBe 11
    counts("Mavka. prawdziwy mit")                                 shouldBe 11
    counts("Modigliani: portret odarty z legendy")                 shouldBe 11
    counts("Munch: miłość, duchy i wampirzyce")                   shouldBe 11
    counts("Młode matki")                                          shouldBe 33
    counts("Niesamowite przygody skarpetek 3. ale kosmos!")        shouldBe 22
    counts("Szepty lasu")                                          shouldBe 11
    counts("Sprawiedliwość owiec")                                 shouldBe 33
    counts("Top gun | 40 rocznica")                                shouldBe 11
    counts("Van gogh. pola zbóż i zachmurzone niebiosa")          shouldBe 11
    counts("Znaki pana śliwki")                                    shouldBe 11
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for Mavka. prawdziwy mit" in {
    val st = byTitle("Mavka. prawdziwy mit").showtimes
    st.size shouldBe 11
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 16, 17, 0),  Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917511"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 16, 21, 10), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917514"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 17, 18, 0),  Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917520"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 17, 20, 15), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917521"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 18, 16, 30), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927665"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 18, 20, 45), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927667"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 20, 21, 15), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927670"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 21, 17, 30), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927680"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 24, 14, 45), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927726"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 25, 20, 20), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927728"), None, Nil),
      Showtime(LocalDateTime.of(2026, 5, 26, 20, 15), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927731"), None, Nil),
    )
  }
}
