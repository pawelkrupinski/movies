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
      "Ale mam | Ну мам | wersja ukraińska z ang. napisami",
      "CZŁOWIEK Z MARMURU",
      "DIABEŁ UBIERA SIĘ U PRADY 2",
      "Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC",
      "MAVKA. PRAWDZIWY MIT",
      "MODIGLIANI: PORTRET ODARTY Z LEGENDY",
      "MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE",
      "MŁODE MATKI",
      "NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!",
      "SZEPTY LASU",
      "Sprawiedliwość owiec",
      "TOP GUN | 40 rocznica",
      "VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA",
      "ZNAKI PANA ŚLIWKI",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("Ale mam | Ну мам | wersja ukraińska z ang. napisami") shouldBe Some(90)
    runtimes("CZŁOWIEK Z MARMURU")                                   shouldBe Some(156)
    runtimes("DIABEŁ UBIERA SIĘ U PRADY 2")                         shouldBe Some(120)
    runtimes("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC")    shouldBe Some(110)
    runtimes("MAVKA. PRAWDZIWY MIT")                                 shouldBe Some(100)
    runtimes("MODIGLIANI: PORTRET ODARTY Z LEGENDY")                 shouldBe Some(90)
    runtimes("MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE")                   shouldBe Some(90)
    runtimes("MŁODE MATKI")                                          shouldBe Some(104)
    runtimes("NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!")        shouldBe Some(55)
    runtimes("SZEPTY LASU")                                          shouldBe Some(84)
    runtimes("Sprawiedliwość owiec")                                 shouldBe Some(105)
    runtimes("TOP GUN | 40 rocznica")                                shouldBe Some(110)
    runtimes("VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA")          shouldBe Some(85)
    runtimes("ZNAKI PANA ŚLIWKI")                                    shouldBe Some(72)
  }

  // ── Release years ─────────────────────────────────────────────────────────

  it should "return correct release year for every movie" in {
    byTitle("Ale mam | Ну мам | wersja ukraińska z ang. napisami").movie.releaseYear shouldBe Some(2026)
    byTitle("CZŁOWIEK Z MARMURU").movie.releaseYear                                   shouldBe Some(1976)
    byTitle("DIABEŁ UBIERA SIĘ U PRADY 2").movie.releaseYear                         shouldBe Some(2026)
    byTitle("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC").movie.releaseYear    shouldBe Some(2025)
    byTitle("MAVKA. PRAWDZIWY MIT").movie.releaseYear                                 shouldBe Some(2026)
    byTitle("MODIGLIANI: PORTRET ODARTY Z LEGENDY").movie.releaseYear                 shouldBe Some(2020)
    byTitle("MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE").movie.releaseYear                   shouldBe Some(2022)
    byTitle("MŁODE MATKI").movie.releaseYear                                          shouldBe Some(2025)
    byTitle("NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!").movie.releaseYear        shouldBe Some(2026)
    byTitle("SZEPTY LASU").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Sprawiedliwość owiec").movie.releaseYear                                 shouldBe Some(2026)
    byTitle("TOP GUN | 40 rocznica").movie.releaseYear                                shouldBe Some(1986)
    byTitle("VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA").movie.releaseYear          shouldBe Some(2018)
    byTitle("ZNAKI PANA ŚLIWKI").movie.releaseYear                                    shouldBe Some(2025)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Ale mam | Ну мам | wersja ukraińska z ang. napisami") shouldBe Some("/wp-content/plugins/b24-api/css/images/FB.png")
    posters("CZŁOWIEK Z MARMURU")                                   shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/czlowiek-z-marmuru-plakat1.jpg")
    posters("DIABEŁ UBIERA SIĘ U PRADY 2")                         shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/diabel-ubiera-sie-u-prady-2-plakat1.jpg")
    posters("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC")    shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/dobry-chlopiec-plakat.jpg")
    posters("MAVKA. PRAWDZIWY MIT")                                 shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/untitled-1-copy.jpg")
    posters("MODIGLIANI: PORTRET ODARTY Z LEGENDY")                 shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/modigliani-plakat-pion-scaled.jpg")
    posters("MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE")                   shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/munch-plakat-b1.jpg")
    posters("MŁODE MATKI")                                          shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/plakat-mlode-matki-700x1050.jpg")
    posters("NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!")        shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/niesamowite-przygody-skarpetek-3-ale-kosmos-plakat-scaled.jpg")
    posters("SZEPTY LASU")                                          shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/szepty-lasu-plakat.jpg")
    posters("Sprawiedliwość owiec")                                 shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/plakat-sprawiedliwosc-owiec-internetjpg.jpg")
    posters("TOP GUN | 40 rocznica")                                shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/top-gun-rocznica-tg40th.jpg")
    posters("VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA")          shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/vangogh-plakat.jpg")
    posters("ZNAKI PANA ŚLIWKI")                                    shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/znaki-pana-sliwki-plakat.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Ale mam | Ну мам | wersja ukraińska z ang. napisami") shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155841")
    filmUrls("CZŁOWIEK Z MARMURU")                                   shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155445")
    filmUrls("DIABEŁ UBIERA SIĘ U PRADY 2")                         shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=154210")
    filmUrls("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC")    shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155850")
    filmUrls("MAVKA. PRAWDZIWY MIT")                                 shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155441")
    filmUrls("MODIGLIANI: PORTRET ODARTY Z LEGENDY")                 shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=87240")
    filmUrls("MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE")                   shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=95047")
    filmUrls("MŁODE MATKI")                                          shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155895")
    filmUrls("NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!")        shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155443")
    filmUrls("SZEPTY LASU")                                          shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155439")
    filmUrls("Sprawiedliwość owiec")                                 shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=154188")
    filmUrls("TOP GUN | 40 rocznica")                                shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=154207")
    filmUrls("VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA")          shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=48676")
    filmUrls("ZNAKI PANA ŚLIWKI")                                    shouldBe Some("https://www.kinorialto.poznan.pl/wydarzenie/?id=155877")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for films that have one" in {
    byTitle("CZŁOWIEK Z MARMURU").director                                shouldBe Some("Andrzej Wajda")
    byTitle("DIABEŁ UBIERA SIĘ U PRADY 2").director                      shouldBe Some("David Frankel")
    byTitle("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC").director shouldBe Some("Jan Komasa")
    byTitle("MAVKA. PRAWDZIWY MIT").director                              shouldBe Some("Katya Tsarik")
    byTitle("MODIGLIANI: PORTRET ODARTY Z LEGENDY").director              shouldBe Some("Valeria Parisi")
    byTitle("MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE").director                shouldBe Some("Michele Mally")
    byTitle("MŁODE MATKI").director                                       shouldBe Some("Jean-Pierre Dardenne, Luc Dardenne")
    byTitle("NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!").director     shouldBe Some("Paweł Wendorff, Elżbieta Wąsik; Jarosław Szyszko, Barbara Koniecka, Mateusz Kmieć, Natalia Bartska-Kmieć")
    byTitle("SZEPTY LASU").director                                       shouldBe Some("Krzysztof Sarapata, Tomasz Kotaś")
    byTitle("Sprawiedliwość owiec").director                              shouldBe Some("Kyle Balda")
    byTitle("TOP GUN | 40 rocznica").director                             shouldBe Some("Tony Scott")
    byTitle("VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA").director       shouldBe Some("Giovanni Piscaglia")
    byTitle("ZNAKI PANA ŚLIWKI").director                                 shouldBe Some("Urszula Morga, Bartosz Mikołajczyk")
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Ale mam | Ну мам | wersja ukraińska z ang. napisami") shouldBe 11
    counts("CZŁOWIEK Z MARMURU")                                   shouldBe 11
    counts("DIABEŁ UBIERA SIĘ U PRADY 2")                         shouldBe 33
    counts("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC")    shouldBe 11
    counts("MAVKA. PRAWDZIWY MIT")                                 shouldBe 11
    counts("MODIGLIANI: PORTRET ODARTY Z LEGENDY")                 shouldBe 11
    counts("MUNCH: MIŁOŚĆ, DUCHY I WAMPIRZYCE")                   shouldBe 11
    counts("MŁODE MATKI")                                          shouldBe 33
    counts("NIESAMOWITE PRZYGODY SKARPETEK 3. ALE KOSMOS!")        shouldBe 22
    counts("SZEPTY LASU")                                          shouldBe 11
    counts("Sprawiedliwość owiec")                                 shouldBe 33
    counts("TOP GUN | 40 rocznica")                                shouldBe 11
    counts("VAN GOGH. POLA ZBÓŻ I ZACHMURZONE NIEBIOSA")          shouldBe 11
    counts("ZNAKI PANA ŚLIWKI")                                    shouldBe 11
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for MAVKA. PRAWDZIWY MIT" in {
    val st = byTitle("MAVKA. PRAWDZIWY MIT").showtimes
    st.size shouldBe 11
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 16, 17, 0),  Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917511"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 16, 21, 10), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917514"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 17, 18, 0),  Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917520"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 17, 20, 15), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=917521"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 18, 16, 30), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927665"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 18, 20, 45), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927667"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 20, 21, 15), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927670"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 21, 17, 30), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927680"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 24, 14, 45), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927726"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 25, 20, 20), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927728"), None, None),
      Showtime(LocalDateTime.of(2026, 5, 26, 20, 15), Some("https://www.kinorialto.poznan.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=927731"), None, None),
    )
  }
}
