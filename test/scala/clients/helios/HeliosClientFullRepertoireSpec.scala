package clients.helios

import clients.tools.FakeHttpFetch
import models.{Helios, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosClient

import java.time.LocalDateTime

class HeliosClientFullRepertoireSpec extends AnyFlatSpec with Matchers {

  private val client  = new HeliosClient(new FakeHttpFetch("helios/rest-enrichment"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "HeliosClient.fetch" should "return exactly 48 movies from the full fixture" in {
    results.size shouldBe 48
  }

  it should "return 421 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 421
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Billie Eilish - Hit Me Hard and Soft: The Tour",
      "Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D",
      "Billie Eilish - Hit Me Hard and Soft: The Tour Live",
      "Cirque du Soleil: Kooza",
      "Diabeł ubiera się u Prady 2",
      "Drama",
      "Drugie życie - Kino Kobiet",
      "Drzewo magii",
      "Dyyavol Nosyt' Prada 2 - UA ",
      "Hopnięci",
      "Iron Maiden: Burning Ambition",
      "Kosmiczny mecz. 30. Rocznica",
      "Kurozając i Świątynia Świstaka",
      "Liga Mistrzów UEFA - Finał",
      "Mandalorian & Grogu",
      "Michael",
      "Mortal Kombat II",
      "Mortal Kombat II - UA",
      "Mumia: Film Lee Cronina",
      "NT Live: Playboy zachodniego świata",
      "NT Live: Wszyscy moi synowie",
      "Nawet myszy idą do nieba",
      "Niesamowite przygody skarpetek 3. Ale kosmos!",
      "O dziewczynie skaczącej przez czas",
      "O psie, który jeździł koleją ",
      "Obsesja",
      "Odrodzony jako galareta. Film: Łzy Morza Lazurowego",
      "Podziemny krąg",
      "Powrót do przyszłości",
      "Powrót do przyszłości II",
      "Powrót do przyszłości III",
      "Projekt Hail Mary",
      "Pucio",
      "Sprawiedliwość owiec",
      "Straszny film ",
      "Super Mario Galaxy Film",
      "The Amazing Digital Circus: The Last Act - dubbing - Event projekt",
      "The Amazing Digital Circus: The Last Act - napisy - Event projekt",
      "Tom i Jerry: Przygoda w muzeum",
      "Top Gun 40. Rocznica",
      "Top Gun: Maverick",
      "Wartość sentymentalna",
      "Werdykt - Kino Konesera",
      "Władcy wszechświata",
      "Yu-Gi-Oh! The Dark Side of Dimensions",
      "Za duży na bajki 3",
      "ДИЯВОЛ НОСИТЬ ПРАДА 2",
      "МОРТАЛ КОМБАТ ІІ"
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(cm => cm.movie.title -> cm.movie.runtimeMinutes).toMap
    runtimes("Billie Eilish - Hit Me Hard and Soft: The Tour")              shouldBe Some(114)
    runtimes("Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D") shouldBe Some(114)
    runtimes("Billie Eilish - Hit Me Hard and Soft: The Tour Live")   shouldBe Some(114)
    runtimes("Cirque du Soleil: Kooza")                                     shouldBe Some(90)
    runtimes("Diabeł ubiera się u Prady 2")                                 shouldBe Some(120)
    runtimes("Drama")                                                       shouldBe Some(106)
    runtimes("Drugie życie - Kino Kobiet")                                  shouldBe Some(116)
    runtimes("Drzewo magii")                                                shouldBe Some(110)
    runtimes("Dyyavol Nosyt' Prada 2 - UA ")                               shouldBe Some(125)
    runtimes("Hopnięci")                                                    shouldBe Some(106)
    runtimes("Iron Maiden: Burning Ambition")                               shouldBe Some(106)
    runtimes("Kosmiczny mecz. 30. Rocznica")                                shouldBe Some(88)
    runtimes("Kurozając i Świątynia Świstaka")                              shouldBe Some(88)
    runtimes("Liga Mistrzów UEFA - Finał")                                  shouldBe Some(180)
    runtimes("Mandalorian & Grogu")                                         shouldBe Some(132)
    runtimes("Michael")                                                     shouldBe Some(128)
    runtimes("Mortal Kombat II")                                            shouldBe Some(116)
    runtimes("Mortal Kombat II - UA")                                       shouldBe Some(116)
    runtimes("Mumia: Film Lee Cronina")                                     shouldBe Some(135)
    runtimes("NT Live: Playboy zachodniego świata")                         shouldBe Some(150)
    runtimes("NT Live: Wszyscy moi synowie")                                shouldBe Some(151)
    runtimes("Nawet myszy idą do nieba")                                    shouldBe Some(87)
    runtimes("Niesamowite przygody skarpetek 3. Ale kosmos!") shouldBe Some(55)
    runtimes("O dziewczynie skaczącej przez czas")                          shouldBe Some(98)
    runtimes("O psie, który jeździł koleją ")                               shouldBe Some(99)
    runtimes("Obsesja")                                                     shouldBe Some(109)
    runtimes("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")         shouldBe Some(105)
    runtimes("Podziemny krąg")                                              shouldBe Some(140)
    runtimes("Powrót do przyszłości")                                       shouldBe Some(116)
    runtimes("Powrót do przyszłości II")                                    shouldBe Some(108)
    runtimes("Powrót do przyszłości III")                                   shouldBe Some(118)
    runtimes("Projekt Hail Mary")                                           shouldBe Some(157)
    runtimes("Pucio")                                                       shouldBe Some(45)
    runtimes("Sprawiedliwość owiec")                                        shouldBe Some(109)
    runtimes("Straszny film ")                                              shouldBe Some(94)
    runtimes("Super Mario Galaxy Film")                                     shouldBe Some(100)
    runtimes("The Amazing Digital Circus: The Last Act - dubbing - Event projekt") shouldBe Some(93)
    runtimes("The Amazing Digital Circus: The Last Act - napisy - Event projekt")  shouldBe Some(93)
    runtimes("Tom i Jerry: Przygoda w muzeum")                              shouldBe Some(140)
    runtimes("Top Gun 40. Rocznica")                                        shouldBe Some(110)
    runtimes("Top Gun: Maverick")                                           shouldBe Some(130)
    runtimes("Wartość sentymentalna")                                       shouldBe Some(135)
    runtimes("Werdykt - Kino Konesera")                                     shouldBe Some(89)
    runtimes("Władcy wszechświata")                                         shouldBe Some(141)
    runtimes("Yu-Gi-Oh! The Dark Side of Dimensions")                       shouldBe Some(130)
    runtimes("Za duży na bajki 3")                                          shouldBe Some(90)
    runtimes("ДИЯВОЛ НОСИТЬ ПРАДА 2")                                       shouldBe Some(125)
    runtimes("МОРТАЛ КОМБАТ ІІ")                                            shouldBe Some(116)
  }

  // ── REST-enriched metadata ────────────────────────────────────────────────

  it should "return correct release year for REST-enriched movies" in {
    byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour").movie.releaseYear shouldBe Some(2026)
    byTitle("Diabeł ubiera się u Prady 2").movie.releaseYear                    shouldBe Some(2026)
    byTitle("Drama").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Drzewo magii").movie.releaseYear                                   shouldBe Some(2026)
    byTitle("Dyyavol Nosyt' Prada 2 - UA ").movie.releaseYear                  shouldBe Some(2026)
    byTitle("Kurozając i Świątynia Świstaka").movie.releaseYear                 shouldBe Some(2025)
    byTitle("Michael").movie.releaseYear                                        shouldBe Some(2026)
    byTitle("Mortal Kombat II").movie.releaseYear                               shouldBe Some(2025)
    byTitle("Mortal Kombat II - UA").movie.releaseYear                          shouldBe Some(2025)
    byTitle("Mumia: Film Lee Cronina").movie.releaseYear                        shouldBe Some(2026)
    byTitle("Nawet myszy idą do nieba").movie.releaseYear                       shouldBe Some(2021)
    byTitle("O psie, który jeździł koleją ").movie.releaseYear                  shouldBe Some(2023)
    byTitle("Obsesja").movie.releaseYear                                        shouldBe Some(2026)
    byTitle("Projekt Hail Mary").movie.releaseYear                              shouldBe Some(2026)
    byTitle("Pucio").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Sprawiedliwość owiec").movie.releaseYear                           shouldBe Some(2026)
    byTitle("Super Mario Galaxy Film").movie.releaseYear                        shouldBe Some(2026)
    byTitle("Za duży na bajki 3").movie.releaseYear                             shouldBe Some(2026)
  }

  it should "return correct Polish premiere dates for REST-enriched movies" in {
    import java.time.LocalDate
    byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour").movie.premierePl  shouldBe Some(LocalDate.of(2026, 5, 8))
    byTitle("Diabeł ubiera się u Prady 2").movie.premierePl                     shouldBe Some(LocalDate.of(2026, 5, 1))
    byTitle("Dyyavol Nosyt' Prada 2 - UA ").movie.premierePl                   shouldBe Some(LocalDate.of(2026, 5, 1))
    byTitle("Kurozając i Świątynia Świstaka").movie.premierePl                  shouldBe Some(LocalDate.of(2026, 5, 15))
    byTitle("Mortal Kombat II").movie.premierePl                                shouldBe Some(LocalDate.of(2026, 5, 8))
    byTitle("Mortal Kombat II - UA").movie.premierePl                           shouldBe Some(LocalDate.of(2026, 5, 8))
    byTitle("Mumia: Film Lee Cronina").movie.premierePl                         shouldBe Some(LocalDate.of(2026, 4, 17))
    byTitle("Nawet myszy idą do nieba").movie.premierePl                        shouldBe Some(LocalDate.of(2022, 4, 29))
    byTitle("O psie, który jeździł koleją ").movie.premierePl                   shouldBe Some(LocalDate.of(2023, 8, 25))
    byTitle("Obsesja").movie.premierePl                                         shouldBe Some(LocalDate.of(2026, 5, 15))
    byTitle("Projekt Hail Mary").movie.premierePl                               shouldBe Some(LocalDate.of(2026, 3, 20))
    byTitle("Pucio").movie.premierePl                                           shouldBe Some(LocalDate.of(2026, 4, 17))
    byTitle("Sprawiedliwość owiec").movie.premierePl                            shouldBe Some(LocalDate.of(2026, 5, 8))
    byTitle("Super Mario Galaxy Film").movie.premierePl                         shouldBe Some(LocalDate.of(2026, 4, 10))
    byTitle("Za duży na bajki 3").movie.premierePl                              shouldBe Some(LocalDate.of(2026, 3, 6))
    byTitle("Drama").movie.premierePl                                           shouldBe Some(LocalDate.of(2026, 4, 10))
    byTitle("Drzewo magii").movie.premierePl                                    shouldBe Some(LocalDate.of(2026, 5, 29))
    byTitle("Michael").movie.premierePl                                         shouldBe Some(LocalDate.of(2026, 4, 22))
  }

  it should "return correct production country for REST-enriched movies" in {
    byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour").movie.country shouldBe Some("USA")
    byTitle("Diabeł ubiera się u Prady 2").movie.country                    shouldBe Some("USA")
    byTitle("Drama").movie.country                                          shouldBe Some("USA")
    byTitle("Drzewo magii").movie.country                                   shouldBe Some("Wielka Brytania")
    byTitle("Dyyavol Nosyt' Prada 2 - UA ").movie.country                  shouldBe Some("USA")
    byTitle("Kurozając i Świątynia Świstaka").movie.country                 shouldBe Some("Francja")
    byTitle("Michael").movie.country                                        shouldBe Some("Wielka Brytania, USA")
    byTitle("Mortal Kombat II").movie.country                               shouldBe Some("USA")
    byTitle("Mortal Kombat II - UA").movie.country                          shouldBe Some("USA")
    byTitle("Mumia: Film Lee Cronina").movie.country                        shouldBe Some("Irlandia, USA")
    byTitle("Nawet myszy idą do nieba").movie.country                       shouldBe Some("Republika Czeska, Francja, Inne, Polska")
    byTitle("O psie, który jeździł koleją ").movie.country                  shouldBe Some("Polska")
    byTitle("Obsesja").movie.country                                        shouldBe Some("USA")
    byTitle("Projekt Hail Mary").movie.country                              shouldBe Some("USA")
    byTitle("Pucio").movie.country                                          shouldBe Some("Polska")
    byTitle("Sprawiedliwość owiec").movie.country                           shouldBe Some("Niemcy, Wielka Brytania, Irlandia, USA")
    byTitle("Super Mario Galaxy Film").movie.country                        shouldBe Some("USA, Japonia")
    byTitle("Za duży na bajki 3").movie.country                             shouldBe Some("Polska")
    // Pure-NUXT entries (event variants without a matching REST movie body) have no REST country.
    byTitle("Cirque du Soleil: Kooza").movie.country                        shouldBe None
    byTitle("Mandalorian & Grogu").movie.country                            shouldBe None
  }

  it should "return correct directors for REST-enriched movies" in {
    byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour").director shouldBe Some("James Cameron, Billie Eilish")
    byTitle("Diabeł ubiera się u Prady 2").director                    shouldBe Some("David Frankel")
    byTitle("Drama").director                                          shouldBe Some("Kristoffer Borgli")
    byTitle("Drzewo magii").director                                   shouldBe Some("Ben Gregor")
    byTitle("Dyyavol Nosyt' Prada 2 - UA ").director                  shouldBe Some("Девід Франкель")
    byTitle("Kurozając i Świątynia Świstaka").director                 shouldBe Some("Benjamin Mousquet")
    byTitle("Michael").director                                        shouldBe Some("Antoine Fuqua")
    byTitle("Mortal Kombat II").director                               shouldBe Some("Simon McQuoid")
    byTitle("Mortal Kombat II - UA").director                         shouldBe Some("Саймон Макквойд")
    byTitle("Mumia: Film Lee Cronina").director                        shouldBe Some("Lee Cronin")
    byTitle("Nawet myszy idą do nieba").director                       shouldBe Some("Jan Bubenicek, Denisa Grimmova")
    byTitle("O psie, który jeździł koleją ").director                  shouldBe Some("Magdalena Nieć")
    byTitle("Obsesja").director                                        shouldBe Some("Curry Barker")
    byTitle("Projekt Hail Mary").director                              shouldBe Some("Christopher Miller, Phil Lord")
    byTitle("Pucio").director                                          shouldBe Some("Marta Stróżycka")
    byTitle("Sprawiedliwość owiec").director                           shouldBe Some("Kyle Balda")
    byTitle("Super Mario Galaxy Film").director                        shouldBe Some("Aaron Horvath, Michael Jelenic")
    byTitle("Za duży na bajki 3").director                             shouldBe Some("Kristoffer Rus")
  }

  it should "return REST-enriched synopsis and cast for a covered movie" in {
    // Mortal Kombat II is fully REST-enriched. Lock in that synopsis and cast both flow through.
    val mk = byTitle("Mortal Kombat II")
    mk.cast     shouldBe Some("Hiroyuki Sanada, Karl Urban, Tadanobu Asano")
    mk.synopsis.map(_.take(80)) shouldBe Some("Oczekiwanie prawie dobiega końca! Mortal Kombat II w kinach od 8 maja. New Line ")
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URLs for all movies" in {
    val posters = results.map(cm => cm.movie.title -> cm.posterUrl).toMap
    // REST-sourced posters (movies.helios.pl)
    posters("Billie Eilish - Hit Me Hard and Soft: The Tour") shouldBe
      Some("https://img.helios.pl/pliki/film/billie-eilish-hit-me-hard-and-soft-the-tour/billie-eilish-hit-me-hard-and-soft-the-tour-plakat-28207.png")
    posters("Diabeł ubiera się u Prady 2")    shouldBe Some("https://movies.helios.pl/images/prada2plakat.jpg")
    posters("Drama")                          shouldBe Some("https://movies.helios.pl/images/DRAMAplakat2kpg.jpg")
    posters("Dyyavol Nosyt' Prada 2 - UA ")  shouldBe Some("https://movies.helios.pl/images/Diabelubierasieuprady2UA.jpg")
    posters("Michael")                        shouldBe Some("https://movies.helios.pl/images/Michaelnowyplakat.jpg")
    posters("Mortal Kombat II")               shouldBe Some("https://movies.helios.pl/images/MORTALKOMBAT2plakat.jpg")
    posters("Mortal Kombat II - UA")          shouldBe Some("https://movies.helios.pl/images/MortalKombatIIUA.jpg")
    posters("Mumia: Film Lee Cronina")        shouldBe Some("https://movies.helios.pl/images/MUMIAplakat.jpg")
    posters("Nawet myszy idą do nieba")       shouldBe Some("https://movies.helios.pl/plakaty/nawetmyszyidadonieba/nawetmyszyidadonieba.jpg")
    posters("O psie, który jeździł koleją ") shouldBe Some("https://movies.helios.pl/images/opsieplakat.jpg")
    posters("Obsesja")                        shouldBe Some("https://movies.helios.pl/images/Obsesjaplakat.jpg")
    posters("Projekt Hail Mary")              shouldBe Some("https://movies.helios.pl/images/ProjektHailMaryplakat.jpg")
    posters("Pucio")                          shouldBe Some("https://movies.helios.pl/images/pucioplakat.jpg")
    posters("Sprawiedliwość owiec")           shouldBe Some("https://movies.helios.pl/images/Sprawiedliwoscowiecplakat.jpg")
    posters("Super Mario Galaxy Film")        shouldBe Some("https://movies.helios.pl/images/SuperMarioplakat.jpg")
    posters("Za duży na bajki 3")             shouldBe Some("https://movies.helios.pl/images/zaduzynabajki3plakat.jpg")
    // NUXT-sourced posters (img.helios.pl)
    posters("Cirque du Soleil: Kooza")        shouldBe Some("https://img.helios.pl/pliki/film/cirque-du-soleil-kooza/cirque-du-soleil-kooza-plakat-57003.jpg")
    posters("Drzewo magii")                   shouldBe Some("https://movies.helios.pl/images/DRZEWOMAGIIPLAKAT.jpg")
    posters("Hopnięci")                       shouldBe Some("https://img.helios.pl/pliki/film/hopnieci/hopnieci-plakat-424.jpg")
    posters("Iron Maiden: Burning Ambition")  shouldBe Some("https://img.helios.pl/pliki/film/iron-maiden-burning-ambition/iron-maiden-burning-ambition-plakat-67489.jpg")
    posters("Kosmiczny mecz. 30. Rocznica")   shouldBe Some("https://img.helios.pl/pliki/film/kosmiczny-mecz-30-rocznica/kosmiczny-mecz-30-rocznica-plakat-58235.jpg")
    posters("Kurozając i Świątynia Świstaka") shouldBe Some("https://movies.helios.pl/images/Kurozajacplakat.jpg")
    posters("Mandalorian & Grogu")            shouldBe Some("https://img.helios.pl/pliki/film/mandalorian-grogu/mandalorian-grogu-plakat-224.jpg")
    posters("NT Live: Playboy zachodniego świata") shouldBe
      Some("https://img.helios.pl/pliki/film/nt-live-playboy-zachodniego-swiata/nt-live-playboy-zachodniego-swiata-plakat-21911.jpg")
    posters("NT Live: Wszyscy moi synowie")   shouldBe
      Some("https://img.helios.pl/pliki/film/nt-live-wszyscy-moi-synowie/nt-live-wszyscy-moi-synowie-plakat-77160.jpg")
    posters("Niesamowite przygody skarpetek 3. Ale kosmos!") shouldBe
      Some("https://img.helios.pl/pliki/film/niesamowite-przygody-skarpetek-3/niesamowite-przygody-skarpetek-3-plakat-34127.png")
    posters("O dziewczynie skaczącej przez czas") shouldBe
      Some("https://img.helios.pl/pliki/film/o-dziewczynie-skaczacej-przez-czas/o-dziewczynie-skaczacej-przez-czas-plakat-7957.jpg")
    posters("Odrodzony jako galareta. Film: Łzy Morza Lazurowego") shouldBe
      Some("https://img.helios.pl/pliki/film/odrodzony-jako-galareta-film-lzy-morza-lazurowego/odrodzony-jako-galareta-film-lzy-morza-lazurowego-plakat-38586.jpg")
    posters("Podziemny krąg")                shouldBe Some("https://img.helios.pl/pliki/film/podziemny-krag/podziemny-krag-plakat-52238.jpg")
    posters("Powrót do przyszłości")          shouldBe Some("https://img.helios.pl/pliki/film/powrot-do-przyszlosci/powrot-do-przyszlosci-plakat-32240.jpg")
    posters("Powrót do przyszłości II")       shouldBe Some("https://img.helios.pl/pliki/film/powrot-do-przyszlosci-ii/powrot-do-przyszlosci-ii-plakat-70759.jpg")
    posters("Powrót do przyszłości III")      shouldBe Some("https://img.helios.pl/pliki/film/powrot-do-przyszlosci-iii/powrot-do-przyszlosci-iii-plakat-29754.jpg")
    posters("Straszny film ")                 shouldBe Some("https://img.helios.pl/pliki/film/straszny-film-6/straszny-film-6-plakat-66159.jpg")
    posters("The Amazing Digital Circus: The Last Act - dubbing - Event projekt") shouldBe
      Some("https://img.helios.pl/pliki/film/the-amazing-digital-circus-the-last-act/the-amazing-digital-circus-the-last-act-plakat-70943.jpg")
    posters("Tom i Jerry: Przygoda w muzeum") shouldBe
      Some("https://img.helios.pl/pliki/film/tom-i-jerry-przygoda-w-muzeum/tom-i-jerry-przygoda-w-muzeum-plakat-650.jpg")
    posters("Top Gun 40. Rocznica")           shouldBe Some("https://img.helios.pl/pliki/film/top-gun-40-rocznica/top-gun-40-rocznica-plakat-48731.jpg")
    posters("Top Gun: Maverick")              shouldBe Some("https://img.helios.pl/pliki/film/top-gun-maverick/top-gun-maverick-plakat.jpg")
    posters("Wartość sentymentalna")          shouldBe Some("https://img.helios.pl/pliki/film/wartosc-sentymentalna/wartosc-sentymentalna-plakat-66574.jpeg")
    posters("Werdykt - Kino Konesera")        shouldBe Some("https://img.helios.pl/pliki/film/werdykt/werdykt-plakat-27931.jpg")
    posters("Władcy wszechświata")            shouldBe Some("https://img.helios.pl/pliki/film/wladcy-wszechswiata/wladcy-wszechswiata-plakat-895.jpg")
    posters("Yu-Gi-Oh! The Dark Side of Dimensions") shouldBe
      Some("https://img.helios.pl/pliki/film/yu-gi-oh-the-dark-side-of-dimensions/yu-gi-oh-the-dark-side-of-dimensions-plakat-81113.jpg")
    posters("ДИЯВОЛ НОСИТЬ ПРАДА 2")          shouldBe Some("https://img.helios.pl/pliki/film/dyyavol-nosyt-prada-2-ua/dyyavol-nosyt-prada-2-ua-plakat-161.jpg")
    posters("МОРТАЛ КОМБАТ ІІ")               shouldBe Some("https://img.helios.pl/pliki/film/mortal-kombat-ii-ua/mortal-kombat-ii-ua-plakat-996.jpg")
    // Event variants legitimately share the parent film's poster
    posters("Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D") shouldBe
      Some("https://img.helios.pl/pliki/film/billie-eilish-hit-me-hard-and-soft-the-tour/billie-eilish-hit-me-hard-and-soft-the-tour-plakat-28207.png")
    posters("Billie Eilish - Hit Me Hard and Soft: The Tour Live") shouldBe
      Some("https://img.helios.pl/pliki/film/billie-eilish-hit-me-hard-and-soft-the-tour/billie-eilish-hit-me-hard-and-soft-the-tour-plakat-28207.png")
    posters("Drugie życie - Kino Kobiet")     shouldBe Some("https://img.helios.pl/pliki/film/drugie-zycie/drugie-zycie-plakat-293.jpg")
    posters("The Amazing Digital Circus: The Last Act - napisy - Event projekt") shouldBe
      Some("https://img.helios.pl/pliki/film/the-amazing-digital-circus-the-last-act/the-amazing-digital-circus-the-last-act-plakat-70943.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film/event URLs" in {
    val urls = results.map(cm => cm.movie.title -> cm.filmUrl).toMap
    urls("Billie Eilish - Hit Me Hard and Soft: The Tour")              shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/billie-eilish-hit-me-hard-and-soft-the-tour-4346")
    urls("Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D") shouldBe Some("https://helios.pl/poznan/kino-helios/wydarzenie/billie-eilish-hit-me-hard-and-soft-the-tour-live-in-3d-w-hns-2550")
    urls("Billie Eilish - Hit Me Hard and Soft: The Tour Live")   shouldBe Some("https://helios.pl/poznan/kino-helios/wydarzenie/billie-eilish-hit-me-hard-and-soft-the-tour-live-w-hns-2548")
    urls("Diabeł ubiera się u Prady 2")                                 shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/diabel-ubiera-sie-u-prady-2-4401")
    urls("Drama")                                                       shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/drama-4408")
    urls("Michael")                                                     shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/michael-4308")
    urls("Mortal Kombat II")                                            shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/mortal-kombat-2-4118")
    urls("Mumia: Film Lee Cronina")                                     shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/mumia-film-lee-cronina-4442")
    urls("Nawet myszy idą do nieba")                                    shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/nawet-myszy-ida-do-nieba-344")
    urls("O psie, który jeździł koleją ")                               shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/o-psie-ktory-jezdzil-koleja-715")
    urls("Obsesja")                                                     shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/obsesja-4463")
    urls("Projekt Hail Mary")                                           shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/projekt-hail-mary-4316")
    urls("Pucio")                                                       shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/pucio-2026-4292")
    urls("Sprawiedliwość owiec")                                        shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/sprawiedliwosc-owiec-4466")
    urls("Super Mario Galaxy Film")                                     shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/super-mario-galaxy-film-4310")
    urls("Za duży na bajki 3")                                          shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/za-duzy-na-bajki-3-4291")
    // no REST film URL for UA screenings
    urls("Dyyavol Nosyt' Prada 2 - UA ")                               shouldBe None
    urls("Mortal Kombat II - UA")                                       shouldBe None
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime counts per movie" in {
    val counts = results.map(cm => cm.movie.title -> cm.showtimes.size).toMap
    counts("Billie Eilish - Hit Me Hard and Soft: The Tour")              shouldBe 14
    counts("Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D") shouldBe 4
    counts("Billie Eilish - Hit Me Hard and Soft: The Tour Live")   shouldBe 2
    counts("Cirque du Soleil: Kooza")                                     shouldBe 1
    counts("Diabeł ubiera się u Prady 2")                                 shouldBe 56
    counts("Drama")                                                       shouldBe 2
    counts("Drugie życie - Kino Kobiet")                                  shouldBe 1
    counts("Drzewo magii")                                                shouldBe 10
    counts("Dyyavol Nosyt' Prada 2 - UA ")                               shouldBe 1
    counts("Hopnięci")                                                    shouldBe 1
    counts("Iron Maiden: Burning Ambition")                               shouldBe 10
    counts("Kosmiczny mecz. 30. Rocznica")                                shouldBe 2
    counts("Kurozając i Świątynia Świstaka")                              shouldBe 23
    counts("Liga Mistrzów UEFA - Finał")                                  shouldBe 1
    counts("Mandalorian & Grogu")                                         shouldBe 25
    counts("Michael")                                                     shouldBe 25
    counts("Mortal Kombat II")                                            shouldBe 48
    counts("Mortal Kombat II - UA")                                       shouldBe 1
    counts("Mumia: Film Lee Cronina")                                     shouldBe 2
    counts("NT Live: Playboy zachodniego świata")                         shouldBe 1
    counts("NT Live: Wszyscy moi synowie")                                shouldBe 1
    counts("Nawet myszy idą do nieba")                                    shouldBe 1
    counts("Niesamowite przygody skarpetek 3. Ale kosmos!") shouldBe 7
    counts("O dziewczynie skaczącej przez czas")                          shouldBe 1
    counts("O psie, który jeździł koleją ")                               shouldBe 1
    counts("Obsesja")                                                     shouldBe 28
    counts("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")         shouldBe 2
    counts("Podziemny krąg")                                              shouldBe 4
    counts("Powrót do przyszłości")                                       shouldBe 2
    counts("Powrót do przyszłości II")                                    shouldBe 2
    counts("Powrót do przyszłości III")                                   shouldBe 2
    counts("Projekt Hail Mary")                                           shouldBe 12
    counts("Pucio")                                                       shouldBe 24
    counts("Sprawiedliwość owiec")                                        shouldBe 20
    counts("Straszny film ")                                              shouldBe 11
    counts("Super Mario Galaxy Film")                                     shouldBe 27
    counts("The Amazing Digital Circus: The Last Act - dubbing - Event projekt") shouldBe 4
    counts("The Amazing Digital Circus: The Last Act - napisy - Event projekt")  shouldBe 4
    counts("Tom i Jerry: Przygoda w muzeum")                              shouldBe 9
    counts("Top Gun 40. Rocznica")                                        shouldBe 7
    counts("Top Gun: Maverick")                                           shouldBe 4
    counts("Wartość sentymentalna")                                       shouldBe 1
    counts("Werdykt - Kino Konesera")                                     shouldBe 1
    counts("Władcy wszechświata")                                         shouldBe 9
    counts("Yu-Gi-Oh! The Dark Side of Dimensions")                       shouldBe 2
    counts("Za duży na bajki 3")                                          shouldBe 3
    counts("ДИЯВОЛ НОСИТЬ ПРАДА 2")                                       shouldBe 1
    counts("МОРТАЛ КОМБАТ ІІ")                                            shouldBe 1
  }

  // ── Full showtime detail for key REST-enriched movies ─────────────────────
  // These movies have room + format from the REST API — most valuable to lock in.

  it should "return exact showtimes for Mortal Kombat II" in {
    val mk = byTitle("Mortal Kombat II")
    val expected = Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 13, 30), Some("https://bilety.helios.pl/screen/78b1d860-fadf-4b3f-97f8-cef721748d19?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 8"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 13, 14, 40), Some("https://bilety.helios.pl/screen/926f7767-6e3c-4d34-ad25-ef3cc085de1b?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "NAP", "ATMOS")),
      Showtime(LocalDateTime.of(2026, 5, 13, 16, 10), Some("https://bilety.helios.pl/screen/b8ed5ab0-c699-4c07-9a7e-5e18867143ed?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 8"), List("2D", "DUB")),
      Showtime(LocalDateTime.of(2026, 5, 13, 17, 15), Some("https://bilety.helios.pl/screen/12991f8e-031d-48b8-8d68-9b266f821e77?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "DUB")),
      Showtime(LocalDateTime.of(2026, 5, 13, 18, 45), Some("https://bilety.helios.pl/screen/56ba0377-a410-42a1-b079-3a650889e8e8?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 8"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 13, 20,  0), Some("https://bilety.helios.pl/screen/cd5d9255-5f2f-402f-b9aa-d30aac0ef387?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "NAP", "ATMOS")),
      Showtime(LocalDateTime.of(2026, 5, 14, 13, 30), Some("https://bilety.helios.pl/screen/e6e7ab02-ca3b-4d64-977f-f4236613ba5b?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 4"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 14, 14, 40), Some("https://bilety.helios.pl/screen/66023493-5c12-4579-9f86-031c603e5c17?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "NAP", "ATMOS")),
      Showtime(LocalDateTime.of(2026, 5, 14, 16, 10), Some("https://bilety.helios.pl/screen/51d9d631-0d32-4a9f-baa7-dfe482f3b44f?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 4"), List("2D", "DUB")),
      Showtime(LocalDateTime.of(2026, 5, 14, 17, 15), Some("https://bilety.helios.pl/screen/e79c4edc-af31-4db9-8789-58d158db1468?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "DUB")),
      Showtime(LocalDateTime.of(2026, 5, 14, 18, 45), Some("https://bilety.helios.pl/screen/c849db6e-02b6-45bc-bccd-fd761d86f0dc?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 4"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 14, 20,  0), Some("https://bilety.helios.pl/screen/8a1e1833-0bd8-4111-8f63-627926ee5b0d?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "NAP", "ATMOS")),
      Showtime(LocalDateTime.of(2026, 5, 15, 13, 30), Some("https://bilety.helios.pl/screen/5721ea93-8287-4f3d-bf61-372e4d9d304b?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 2"), List("2D", "DUB")),
      Showtime(LocalDateTime.of(2026, 5, 15, 14,  0), Some("https://bilety.helios.pl/screen/6409151c-c1d7-470a-ae45-2d1a051e340b?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 5 - Dream"), List("2D", "NAP", "ATMOS")),
      Showtime(LocalDateTime.of(2026, 5, 15, 14, 40), Some("https://bilety.helios.pl/screen/a4e2c651-800e-4520-aae1-02913998c781?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 7 - Dream"), List("2D", "DUB")),
      Showtime(LocalDateTime.of(2026, 5, 15, 16, 10), Some("https://bilety.helios.pl/screen/e1a4ebad-42ab-4dd5-bf76-1b928938c803?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 2"), List("2D", "NAP"))
    )
    mk.showtimes.take(16) shouldBe expected
    mk.showtimes.size     shouldBe 48
  }

  it should "return exact showtimes for Projekt Hail Mary (fully REST-enriched)" in {
    val phm = byTitle("Projekt Hail Mary")
    phm.showtimes.filter(_.room.isDefined) shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 14, 20), Some("https://bilety.helios.pl/screen/80cdabad-6d7a-433c-9b53-ab6d6108e695?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 3"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 14, 14, 10), Some("https://bilety.helios.pl/screen/a0136039-230d-4881-b21c-0cf79c383fa7?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 3"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 15, 14, 30), Some("https://bilety.helios.pl/screen/26076126-cbe2-4798-8146-9def2a97e021?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 4"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 16, 13, 20), Some("https://bilety.helios.pl/screen/1341a816-35fe-48bb-98d4-315bcbff220b?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 4"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 16, 16, 45), Some("https://bilety.helios.pl/screen/aefe9805-d2dc-4979-a114-2b690b0e486d?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 4"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 17, 13, 20), Some("https://bilety.helios.pl/screen/947a2ea8-a1bc-477f-b0de-7af45fe593b8?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 6"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 17, 19, 30), Some("https://bilety.helios.pl/screen/7e0ff70e-693a-4f84-a5e1-adf7a8dc64c2?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 6"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 18, 13, 30), Some("https://bilety.helios.pl/screen/65873bc9-eca7-4bd8-876a-88d053a4c607?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 6"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 19, 13, 30), Some("https://bilety.helios.pl/screen/60fe50a6-cee8-49c4-81d3-d20553606c39?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2"), Some("Sala 6"), List("2D", "NAP"))
    )
  }

  it should "return exact showtimes for Pucio (REST-enriched, most days)" in {
    val pucio  = byTitle("Pucio")
    val restSt = pucio.showtimes.filter(_.room.isDefined)
    restSt.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 5, 13, 11, 15),
      LocalDateTime.of(2026, 5, 13, 12, 10),
      LocalDateTime.of(2026, 5, 13, 16, 35),
      LocalDateTime.of(2026, 5, 14, 12,  0),
      LocalDateTime.of(2026, 5, 14, 14, 50),
      LocalDateTime.of(2026, 5, 14, 16, 20),
      LocalDateTime.of(2026, 5, 15, 11, 15),
      LocalDateTime.of(2026, 5, 15, 15, 10),
      LocalDateTime.of(2026, 5, 16, 10, 30),
      LocalDateTime.of(2026, 5, 16, 12, 20),
      LocalDateTime.of(2026, 5, 16, 13, 50),
      LocalDateTime.of(2026, 5, 17, 11,  0),
      LocalDateTime.of(2026, 5, 17, 12, 40),
      LocalDateTime.of(2026, 5, 17, 14, 10),
      LocalDateTime.of(2026, 5, 18, 12,  0),
      LocalDateTime.of(2026, 5, 18, 15,  0),
      LocalDateTime.of(2026, 5, 19, 12,  0),
      LocalDateTime.of(2026, 5, 19, 15,  0)
    )
    restSt.map(_.format).distinct shouldBe Seq(List("2D", "DUB"))
    pucio.showtimes.size shouldBe 24
  }

  it should "return exact showtimes for Sprawiedliwość owiec" in {
    val owiec = byTitle("Sprawiedliwość owiec")
    owiec.showtimes.filter(_.room.isDefined).map(st => st.dateTime -> (st.room.get, st.format)) shouldBe Seq(
      LocalDateTime.of(2026, 5, 13, 11, 30) -> ("Sala 7 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 13, 16, 45) -> ("Sala 4",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 13, 19, 15) -> ("Sala 4",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 14, 10,  0) -> ("Sala 4",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 14, 11, 20) -> ("Sala 7 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 14, 16, 40) -> ("Sala 8",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 14, 19, 15) -> ("Sala 8",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 15, 12,  0) -> ("Sala 4",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 15, 16, 45) -> ("Sala 5 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 16, 10, 40) -> ("Sala 4",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 16, 14, 40) -> ("Sala 7 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 17, 10, 40) -> ("Sala 6",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 17, 14, 40) -> ("Sala 7 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 18, 14, 40) -> ("Sala 7 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 18, 16, 50) -> ("Sala 6",         List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 19, 14, 40) -> ("Sala 7 - Dream", List("2D", "DUB")),
      LocalDateTime.of(2026, 5, 19, 16, 50) -> ("Sala 6",         List("2D", "DUB"))
    )
    owiec.showtimes.size shouldBe 20
  }

  it should "return exact showtimes for Michael" in {
    val michael = byTitle("Michael")
    michael.showtimes.filter(_.room.isDefined).map(st => st.dateTime -> (st.room.get, st.format)) shouldBe Seq(
      LocalDateTime.of(2026, 5, 13, 14,  0) -> ("Sala 7 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 13, 17, 45) -> ("Sala 3",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 13, 20, 45) -> ("Sala 3",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 14, 14,  0) -> ("Sala 7 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 14, 17, 45) -> ("Sala 3",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 14, 20, 45) -> ("Sala 3",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 15, 11, 45) -> ("Sala 7 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 15, 19, 10) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 15, 20, 20) -> ("Sala 8",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 16, 11, 30) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 16, 17,  0) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 16, 20, 15) -> ("Sala 6",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 17, 11, 30) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 17, 17,  0) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 17, 21, 15) -> ("Sala 4",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 18, 11, 30) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 18, 17,  0) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 18, 20, 15) -> ("Sala 4",          List("2D", "NAP")),
      LocalDateTime.of(2026, 5, 19, 11, 30) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 19, 17,  0) -> ("Sala 5 - Dream",  List("2D", "NAP", "ATMOS")),
      LocalDateTime.of(2026, 5, 19, 20, 15) -> ("Sala 4",          List("2D", "NAP"))
    )
    michael.showtimes.size shouldBe 25
  }

  it should "return exact showtimes for Billie Eilish – The Tour (REST + NUXT events combined)" in {
    val billie = byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour")
    // REST-enriched showtimes (with room/format): May 15–19
    billie.showtimes.filter(_.room.isDefined).map(_.dateTime).toSet shouldBe Set(
      LocalDateTime.of(2026, 5, 15, 17, 45),
      LocalDateTime.of(2026, 5, 15, 21,  0),
      LocalDateTime.of(2026, 5, 16, 17, 40),
      LocalDateTime.of(2026, 5, 16, 21, 15),
      LocalDateTime.of(2026, 5, 17, 18,  0),
      LocalDateTime.of(2026, 5, 17, 21,  0),
      LocalDateTime.of(2026, 5, 18, 19, 20),
      LocalDateTime.of(2026, 5, 18, 21, 15),
      LocalDateTime.of(2026, 5, 19, 19, 30),
      LocalDateTime.of(2026, 5, 19, 21, 15)
    )
    billie.showtimes.filter(_.room.isDefined).map(_.format).toSet shouldBe Set(List("2D", "NAP"), List("3D", "NAP"))
    billie.showtimes.size shouldBe 14
  }

  // ── NUXT-only event showtimes ─────────────────────────────────────────────

  it should "return correct showtimes for both Billie Eilish events (NUXT-only)" in {
    val in3d = byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D")
    val wHnS = byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour Live")

    in3d.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 5, 13, 19, 40),
      LocalDateTime.of(2026, 5, 13, 21, 10),
      LocalDateTime.of(2026, 5, 14, 20, 15),
      LocalDateTime.of(2026, 5, 14, 21, 10)
    )
    wHnS.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 5, 13, 15, 10),
      LocalDateTime.of(2026, 5, 14, 15, 40)
    )
    // NUXT events have no room (Helios doesn't expose hall info in NUXT) but format
    // is extracted from each entry's printRelease.
    in3d.showtimes.forall(_.room.isEmpty)   shouldBe true
    wHnS.showtimes.forall(_.room.isEmpty)   shouldBe true
    in3d.showtimes.forall(_.format.nonEmpty) shouldBe true
    wHnS.showtimes.forall(_.format.nonEmpty) shouldBe true
  }

  it should "return booking URLs for NUXT events" in {
    val in3d = byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D")
    in3d.showtimes.flatMap(_.bookingUrl).size shouldBe 4
    in3d.showtimes.head.bookingUrl shouldBe
      Some("https://bilety.helios.pl/screen/512ebbda-4422-4219-aceb-4002b5588a84?cinemaId=815face9-2a1d-4c62-9b2f-a361574b79a2")
  }

  // ── NUXT-only movie properties ────────────────────────────────────────────

  it should "return correct data for NUXT-only movies (not in REST API)" in {
    // Mandalorian & Grogu is a regular NUXT-only film (no REST screening entry)
    val mando = byTitle("Mandalorian & Grogu")
    mando.posterUrl shouldBe Some("https://img.helios.pl/pliki/film/mandalorian-grogu/mandalorian-grogu-plakat-224.jpg")
    mando.filmUrl   shouldBe Some("https://helios.pl/poznan/kino-helios/filmy/mandalorian-grogu-4347")
    mando.synopsis  shouldBe None
    mando.cast      shouldBe None
    mando.director  shouldBe None
    mando.movie.releaseYear shouldBe None

    // Iron Maiden is a NUXT-only event
    val ironMaiden = byTitle("Iron Maiden: Burning Ambition")
    ironMaiden.posterUrl shouldBe Some("https://img.helios.pl/pliki/film/iron-maiden-burning-ambition/iron-maiden-burning-ambition-plakat-67489.jpg")
    ironMaiden.filmUrl   shouldBe Some("https://helios.pl/poznan/kino-helios/wydarzenie/iron-maiden-burning-ambition-w-helios-na-scenie-2503")
    ironMaiden.synopsis  shouldBe None
    ironMaiden.cast      shouldBe None
    ironMaiden.director  shouldBe None
  }

  // ── REST vs NUXT showtime enrichment ──────────────────────────────────────

  it should "enrich only the REST-covered showtimes with room (but format comes from both sources)" in {
    // Mortal Kombat II: 38 REST screenings (room+format from REST), 10 NUXT-only screenings
    // (no room — Helios doesn't expose hall info in NUXT — but format is parsed from NUXT printRelease).
    val mk = byTitle("Mortal Kombat II")
    mk.showtimes.count(_.room.isDefined)   shouldBe 38
    mk.showtimes.count(_.room.isEmpty)     shouldBe 10
    mk.showtimes.count(_.format.nonEmpty)  shouldBe 48
  }

  // ── O psie — the single-screening regression ──────────────────────────────

  it should "assign exactly one showtime to O psie który jeździł koleją" in {
    val oPsie = byTitle("O psie, który jeździł koleją ")
    oPsie.showtimes.size shouldBe 1
    oPsie.showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 5, 14, 10, 0)
    oPsie.showtimes.head.room     shouldBe Some("Sala 2")
    oPsie.showtimes.head.format   shouldBe List("2D", "DUB")
    oPsie.posterUrl               shouldBe Some("https://movies.helios.pl/images/opsieplakat.jpg")
  }

  // ── NUXT-sourced format regression ────────────────────────────────────────

  it should "extract format from the NUXT printRelease field for NUXT-only films" in {
    // Mandalorian & Grogu has no REST entry; every showtime is NUXT-sourced.
    // Each NUXT screening carries `moviePrint:{...,printRelease:"3D/DUB",...}` which
    // must surface on the Showtime so the listing/film pages can show 2D vs 3D.
    val mando = byTitle("Mandalorian & Grogu")
    val withFormat = mando.showtimes.filter(_.format.nonEmpty)
    withFormat                              should not be empty
    withFormat.size                         shouldBe mando.showtimes.size  // every screening has a printRelease
    mando.showtimes.map(_.format).toSet     should contain allOf (List("3D", "DUB"), List("2D", "DUB"))
  }

  // ── removeLessSpecificOverlaps regression ─────────────────────────────────

  it should "strip the ' - seanse z konkursami HDD' suffix and merge with the plain title" in {
    val titles = results.map(_.movie.title).toSet
    // Suffix must never appear in any title.
    titles.foreach(t => t should not include " - seanse z konkursami HDD")
    // Drzewo magii has two underlying NUXT entries (plain + HDD) with disjoint times — merged.
    titles                                                             should contain ("Drzewo magii")
    byTitle("Drzewo magii").showtimes.size                             shouldBe 10
    // Mandalorian & Grogu likewise has plain + HDD entries that combine.
    byTitle("Mandalorian & Grogu").showtimes.size                      shouldBe 25
    // Skarpetek had only a "- HDD" NUXT entry; after stripping it surfaces under the plain title.
    titles                                                             should contain ("Niesamowite przygody skarpetek 3. Ale kosmos!")
    // Tom i Jerry had a plain entry and an HDD entry sharing screenings — distinct'd to 9 slots.
    byTitle("Tom i Jerry: Przygoda w muzeum").showtimes.size           shouldBe 9
  }

  // ── REST/NUXT split for Latin-vs-Cyrillic UA titles ───────────────────────

  it should "keep Latin and Cyrillic UA titles as separate entries with separate metadata" in {
    // Latin "Dyyavol Nosyt' Prada 2 - UA " comes from REST only; the Cyrillic title is in NUXT.
    // They share underlying screening UUIDs but appear in their own sources under distinct titles.
    val latin    = byTitle("Dyyavol Nosyt' Prada 2 - UA ")
    val cyrillic = byTitle("ДИЯВОЛ НОСИТЬ ПРАДА 2")
    latin.director    shouldBe Some("Девід Франкель")
    cyrillic.director shouldBe None
    latin.posterUrl    shouldBe Some("https://movies.helios.pl/images/Diabelubierasieuprady2UA.jpg")
    cyrillic.posterUrl shouldBe Some("https://img.helios.pl/pliki/film/dyyavol-nosyt-prada-2-ua/dyyavol-nosyt-prada-2-ua-plakat-161.jpg")
  }

  // ── Output shape invariants ───────────────────────────────────────────────

  it should "label every returned movie with cinema = Helios" in {
    results.map(_.cinema).toSet shouldBe Set(Helios)
  }

  it should "return showtimes sorted by dateTime and unique by booking URL within each movie" in {
    results.foreach { cm =>
      cm.showtimes.map(_.dateTime) shouldBe cm.showtimes.map(_.dateTime).sorted
      val bookings = cm.showtimes.flatMap(_.bookingUrl)
      withClue(s"duplicate bookingUrl in '${cm.movie.title}': ") {
        bookings.distinct.size shouldBe bookings.size
      }
    }
  }

  it should "return results sorted alphabetically by title" in {
    results.map(_.movie.title) shouldBe results.map(_.movie.title).sorted
  }

  // ── REST timestamp time-zone conversion ───────────────────────────────────

  it should "convert REST screening timestamps to Europe/Warsaw local time" in {
    // The REST screening fixture records the first Mortal Kombat II screening as
    // "2026-05-13T13:30:00+02:00". After conversion to Europe/Warsaw (+02:00 DST),
    // that is 13:30 local — not 11:30 UTC and not 15:30 CET shifted twice.
    val first = byTitle("Mortal Kombat II").showtimes.head
    first.dateTime shouldBe LocalDateTime.of(2026, 5, 13, 13, 30)
  }
}
