package clients.kino_muza

import clients.KinoMuzaClient
import clients.tools.FakeHttpFetch
import models.{KinoMuza, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class KinoMuzaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoMuzaClient(new FakeHttpFetch("kino-muza"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "KinoMuzaClient.fetch" should "return exactly 80 movies from fixture" in {
    results.size shouldBe 80
  }

  it should "return 120 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 120
  }

  it should "assign KinoMuza cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(KinoMuza)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Barbara Forever",
      "Bez końca",
      "Bez znieczulenia",
      "Bibliotekarki",
      "Broken English",
      "Brzezina",
      "Był sobie śnieg",
      "Chronologia wody",
      "Co ukrywa Elon Musk?",
      "Człowiek z marmuru",
      "Drama",
      "Dziecko z pyłu",
      "Dzikie szaleństwo",
      "Efekt architektury",
      "FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar",
      "Fałszerz stulecia",
      "Freak Show",
      "Gala Wręczenia Nagrody Wolności | Pieśni lasu",
      "Giulietta i duchy",
      "Guru",
      "Głos z księżyca",
      "Habibi Hussein",
      "Hamnet",
      "Idź z duszą na dłoni",
      "Istoty czujące",
      "Jak głęboka jest twoja miłość?",
      "Jak zrobić film o betonie",
      "Jane Elliot kontra reszta świata",
      "Kandydaci Śmierci",
      "Krajobraz po bitwie",
      "Kronika wypadków miłosnych",
      "Kłopotliwy niedźwiedź",
      "Layla",
      "Mam rzekę we krwi",
      "Mariinka",
      "Mi Amor",
      "Milcząca przyjaciółka",
      "Miłość, która zostaje",
      "Moi Themersonowie",
      "Młodsza siostra",
      "Najstarsza osoba na świecie",
      "Nie oglądaj się teraz",
      "Niedźwiedzica",
      "Noce Cabirii",
      "Nomadland",
      "Nowa fala",
      "Nurt",
      "O czasie i wodzie",
      "Obcy",
      "Orwell: 2 + 2 = 5",
      "Osiem i pół",
      "Pan Nikt kontra Putin",
      "Pieniądze to wszystko",
      "Pieśni lasu",
      "Poczta głosowa",
      "Pomiędzy woskiem i złotem",
      "Proszę nie siadać",
      "Przeżyj to sam",
      "Romería",
      "Runa Simi",
      "Sny o słoniach",
      "Sygnalista",
      "Słodkie życie",
      "Top Gun",
      "Uciszone",
      "Wałkonie",
      "Wielki Łuk",
      "Windą na szafot",
      "Wolność po włosku",
      "Wpatrując się w słońce",
      "Wspinaczka",
      "Wszyscy na Kenmure Street",
      "Wszystko na sprzedaż",
      "Wypadek fortepianowy",
      "Yanuni",
      "Zemsta Embriona | najlepsze z najgorszych",
      "Ziemia obiecana",
      "Znaki Pana Śliwki",
      "Ślady",
      "Żądło",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("Barbara Forever")                                    shouldBe Some(102)
    runtimes("Bez końca")                                          shouldBe Some(108)
    runtimes("Bez znieczulenia")                                   shouldBe Some(125)
    runtimes("Bibliotekarki")                                      shouldBe Some(92)
    runtimes("Broken English")                                     shouldBe Some(96)
    runtimes("Brzezina")                                           shouldBe Some(99)
    runtimes("Był sobie śnieg")                                    shouldBe Some(128)
    runtimes("Chronologia wody")                                   shouldBe Some(128)
    runtimes("Co ukrywa Elon Musk?")                               shouldBe Some(90)
    runtimes("Człowiek z marmuru")                                 shouldBe Some(153)
    runtimes("Drama")                                              shouldBe Some(105)
    runtimes("Dziecko z pyłu")                                     shouldBe Some(93)
    runtimes("Dzikie szaleństwo")                                  shouldBe Some(84)
    runtimes("Efekt architektury")                                 shouldBe Some(106)
    runtimes("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar") shouldBe Some(98)
    runtimes("Fałszerz stulecia")                                  shouldBe Some(128)
    runtimes("Freak Show")                                         shouldBe Some(75)
    runtimes("Gala Wręczenia Nagrody Wolności | Pieśni lasu")      shouldBe Some(94)
    runtimes("Giulietta i duchy")                                  shouldBe Some(139)
    runtimes("Guru")                                               shouldBe Some(126)
    runtimes("Głos z księżyca")                                    shouldBe Some(121)
    runtimes("Habibi Hussein")                                     shouldBe Some(96)
    runtimes("Hamnet")                                             shouldBe Some(125)
    runtimes("Idź z duszą na dłoni")                               shouldBe Some(113)
    runtimes("Istoty czujące")                                     shouldBe Some(106)
    runtimes("Jak głęboka jest twoja miłość?")                     shouldBe Some(100)
    runtimes("Jak zrobić film o betonie")                          shouldBe Some(101)
    runtimes("Jane Elliot kontra reszta świata")                   shouldBe Some(99)
    runtimes("Kandydaci Śmierci")                                  shouldBe Some(95)
    runtimes("Krajobraz po bitwie")                                shouldBe Some(101)
    runtimes("Kronika wypadków miłosnych")                         shouldBe Some(114)
    runtimes("Kłopotliwy niedźwiedź")                              shouldBe Some(90)
    runtimes("Layla")                                              shouldBe Some(100)
    runtimes("Mam rzekę we krwi")                                  shouldBe Some(98)
    runtimes("Mariinka")                                           shouldBe Some(94)
    runtimes("Mi Amor")                                            shouldBe Some(113)
    runtimes("Milcząca przyjaciółka")                              shouldBe Some(147)
    runtimes("Miłość, która zostaje")                              shouldBe Some(109)
    runtimes("Moi Themersonowie")                                  shouldBe Some(80)
    runtimes("Młodsza siostra")                                    shouldBe Some(106)
    runtimes("Najstarsza osoba na świecie")                        shouldBe Some(87)
    runtimes("Nie oglądaj się teraz")                              shouldBe Some(110)
    runtimes("Niedźwiedzica")                                      shouldBe Some(96)
    runtimes("Noce Cabirii")                                       shouldBe Some(110)
    runtimes("Nomadland")                                          shouldBe Some(108)
    runtimes("Nowa fala")                                          shouldBe Some(105)
    runtimes("Nurt")                                               shouldBe Some(83)
    runtimes("O czasie i wodzie")                                  shouldBe Some(87)
    runtimes("Obcy")                                               shouldBe Some(122)
    runtimes("Orwell: 2 + 2 = 5")                                 shouldBe Some(119)
    runtimes("Osiem i pół")                                        shouldBe Some(138)
    runtimes("Pan Nikt kontra Putin")                              shouldBe Some(90)
    runtimes("Pieniądze to wszystko")                              shouldBe Some(96)
    runtimes("Pieśni lasu")                                        shouldBe Some(94)
    runtimes("Poczta głosowa")                                     shouldBe Some(102)
    runtimes("Pomiędzy woskiem i złotem")                          shouldBe Some(97)
    runtimes("Proszę nie siadać")                                  shouldBe Some(60)
    runtimes("Przeżyj to sam")                                     shouldBe Some(116)
    runtimes("Romería")                                            shouldBe Some(111)
    runtimes("Runa Simi")                                          shouldBe Some(86)
    runtimes("Sny o słoniach")                                     shouldBe Some(99)
    runtimes("Sygnalista")                                         shouldBe Some(91)
    runtimes("Słodkie życie")                                      shouldBe Some(176)
    runtimes("Top Gun")                                            shouldBe Some(110)
    runtimes("Uciszone")                                           shouldBe Some(92)
    runtimes("Wałkonie")                                           shouldBe Some(103)
    runtimes("Wielki Łuk")                                         shouldBe Some(106)
    runtimes("Windą na szafot")                                    shouldBe Some(90)
    runtimes("Wolność po włosku")                                  shouldBe Some(117)
    runtimes("Wpatrując się w słońce")                             shouldBe Some(155)
    runtimes("Wspinaczka")                                         shouldBe Some(83)
    runtimes("Wszyscy na Kenmure Street")                          shouldBe Some(98)
    runtimes("Wszystko na sprzedaż")                               shouldBe Some(94)
    runtimes("Wypadek fortepianowy")                               shouldBe Some(88)
    runtimes("Yanuni")                                             shouldBe Some(112)
    runtimes("Zemsta Embriona | najlepsze z najgorszych")          shouldBe Some(89)
    runtimes("Ziemia obiecana")                                    shouldBe Some(179)
    runtimes("Znaki Pana Śliwki")                                  shouldBe Some(72)
    runtimes("Ślady")                                              shouldBe Some(85)
    runtimes("Żądło")                                              shouldBe Some(129)
  }

  // ── Release years ─────────────────────────────────────────────────────────

  it should "return correct release year for every movie" in {
    byTitle("Barbara Forever").movie.releaseYear                                    shouldBe Some(2026)
    byTitle("Bez końca").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Bez znieczulenia").movie.releaseYear                                   shouldBe Some(1978)
    byTitle("Bibliotekarki").movie.releaseYear                                      shouldBe Some(2025)
    byTitle("Broken English").movie.releaseYear                                     shouldBe Some(2025)
    byTitle("Brzezina").movie.releaseYear                                           shouldBe Some(1970)
    byTitle("Był sobie śnieg").movie.releaseYear                                    shouldBe Some(2025)
    byTitle("Chronologia wody").movie.releaseYear                                   shouldBe Some(2025)
    byTitle("Co ukrywa Elon Musk?").movie.releaseYear                               shouldBe Some(2025)
    byTitle("Człowiek z marmuru").movie.releaseYear                                 shouldBe Some(1977)
    byTitle("Drama").movie.releaseYear                                              shouldBe Some(2026)
    byTitle("Dziecko z pyłu").movie.releaseYear                                     shouldBe Some(2025)
    byTitle("Dzikie szaleństwo").movie.releaseYear                                  shouldBe Some(2025)
    byTitle("Efekt architektury").movie.releaseYear                                 shouldBe Some(2026)
    byTitle("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar").movie.releaseYear shouldBe Some(2025)
    byTitle("Fałszerz stulecia").movie.releaseYear                                  shouldBe Some(2025)
    byTitle("Freak Show").movie.releaseYear                                         shouldBe Some(2026)
    byTitle("Gala Wręczenia Nagrody Wolności | Pieśni lasu").movie.releaseYear      shouldBe Some(2025)
    byTitle("Giulietta i duchy").movie.releaseYear                                  shouldBe Some(1965)
    byTitle("Guru").movie.releaseYear                                               shouldBe Some(2025)
    byTitle("Głos z księżyca").movie.releaseYear                                    shouldBe Some(1990)
    byTitle("Habibi Hussein").movie.releaseYear                                     shouldBe Some(2026)
    byTitle("Hamnet").movie.releaseYear                                             shouldBe Some(2025)
    byTitle("Idź z duszą na dłoni").movie.releaseYear                               shouldBe Some(2025)
    byTitle("Istoty czujące").movie.releaseYear                                     shouldBe Some(2026)
    byTitle("Jak głęboka jest twoja miłość?").movie.releaseYear                     shouldBe Some(2026)
    byTitle("Jak zrobić film o betonie").movie.releaseYear                          shouldBe Some(2026)
    byTitle("Jane Elliot kontra reszta świata").movie.releaseYear                   shouldBe Some(2026)
    byTitle("Kandydaci Śmierci").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Krajobraz po bitwie").movie.releaseYear                                shouldBe Some(1970)
    byTitle("Kronika wypadków miłosnych").movie.releaseYear                         shouldBe Some(1985)
    byTitle("Kłopotliwy niedźwiedź").movie.releaseYear                              shouldBe Some(2026)
    byTitle("Layla").movie.releaseYear                                              shouldBe Some(2024)
    byTitle("Mam rzekę we krwi").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Mariinka").movie.releaseYear                                           shouldBe Some(2026)
    byTitle("Mi Amor").movie.releaseYear                                            shouldBe Some(2025)
    byTitle("Milcząca przyjaciółka").movie.releaseYear                              shouldBe Some(2025)
    byTitle("Miłość, która zostaje").movie.releaseYear                              shouldBe Some(2025)
    byTitle("Moi Themersonowie").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Młodsza siostra").movie.releaseYear                                    shouldBe Some(2025)
    byTitle("Najstarsza osoba na świecie").movie.releaseYear                        shouldBe Some(2025)
    byTitle("Nie oglądaj się teraz").movie.releaseYear                              shouldBe Some(1973)
    byTitle("Niedźwiedzica").movie.releaseYear                                      shouldBe Some(2026)
    byTitle("Noce Cabirii").movie.releaseYear                                       shouldBe Some(1957)
    byTitle("Nomadland").movie.releaseYear                                          shouldBe Some(2020)
    byTitle("Nowa fala").movie.releaseYear                                          shouldBe Some(2025)
    byTitle("Nurt").movie.releaseYear                                               shouldBe Some(2026)
    byTitle("O czasie i wodzie").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Obcy").movie.releaseYear                                               shouldBe Some(2025)
    byTitle("Orwell: 2 + 2 = 5").movie.releaseYear                                 shouldBe Some(2025)
    byTitle("Osiem i pół").movie.releaseYear                                        shouldBe Some(1963)
    byTitle("Pan Nikt kontra Putin").movie.releaseYear                              shouldBe Some(2025)
    byTitle("Pieniądze to wszystko").movie.releaseYear                              shouldBe Some(2026)
    byTitle("Pieśni lasu").movie.releaseYear                                        shouldBe Some(2025)
    byTitle("Poczta głosowa").movie.releaseYear                                     shouldBe Some(2024)
    byTitle("Pomiędzy woskiem i złotem").movie.releaseYear                          shouldBe Some(2025)
    byTitle("Proszę nie siadać").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Przeżyj to sam").movie.releaseYear                                     shouldBe Some(2025)
    byTitle("Romería").movie.releaseYear                                            shouldBe Some(2025)
    byTitle("Runa Simi").movie.releaseYear                                          shouldBe Some(2025)
    byTitle("Sny o słoniach").movie.releaseYear                                     shouldBe Some(2025)
    byTitle("Sygnalista").movie.releaseYear                                         shouldBe Some(2026)
    byTitle("Słodkie życie").movie.releaseYear                                      shouldBe Some(1960)
    byTitle("Top Gun").movie.releaseYear                                            shouldBe Some(1986)
    byTitle("Uciszone").movie.releaseYear                                           shouldBe Some(2026)
    byTitle("Wałkonie").movie.releaseYear                                           shouldBe Some(1953)
    byTitle("Wielki Łuk").movie.releaseYear                                         shouldBe Some(2025)
    byTitle("Windą na szafot").movie.releaseYear                                    shouldBe Some(1958)
    byTitle("Wolność po włosku").movie.releaseYear                                  shouldBe Some(2025)
    byTitle("Wpatrując się w słońce").movie.releaseYear                             shouldBe Some(2025)
    byTitle("Wspinaczka").movie.releaseYear                                         shouldBe Some(2025)
    byTitle("Wszyscy na Kenmure Street").movie.releaseYear                          shouldBe Some(2026)
    byTitle("Wszystko na sprzedaż").movie.releaseYear                               shouldBe Some(1968)
    byTitle("Wypadek fortepianowy").movie.releaseYear                               shouldBe Some(2025)
    byTitle("Yanuni").movie.releaseYear                                             shouldBe Some(2025)
    byTitle("Zemsta Embriona | najlepsze z najgorszych").movie.releaseYear          shouldBe Some(1990)
    byTitle("Ziemia obiecana").movie.releaseYear                                    shouldBe Some(1974)
    byTitle("Znaki Pana Śliwki").movie.releaseYear                                  shouldBe Some(2025)
    byTitle("Ślady").movie.releaseYear                                              shouldBe Some(2026)
    byTitle("Żądło").movie.releaseYear                                              shouldBe Some(1973)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Barbara Forever")                                    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/barbaraforever_still_01-800x450.jpg")
    posters("Bez końca")                                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/closure-stills-0-2875099-closure-stills-dir-michal-marczak-01-main-800x450.jpeg")
    posters("Bez znieczulenia")                                   shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/bez-zniecz-2-800x450.jpg")
    posters("Bibliotekarki")                                      shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/the-librarians-stills-0-2715069-800x450.jpeg")
    posters("Broken English")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/broken-800x450.jpg")
    posters("Brzezina")                                           shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/brzezina-1-800x450.jpg")
    posters("Był sobie śnieg")                                    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/byl-sobie-snieg-2-800x450.jpg")
    posters("Chronologia wody")                                   shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/04/chronologia-wody-4-800x450.jpg")
    posters("Co ukrywa Elon Musk?")                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/elon-3-800x450.jpg")
    posters("Człowiek z marmuru")                                 shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/czlowiek-1-800x450.jpg")
    posters("Drama")                                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/drama-1-800x450.jpg")
    posters("Dziecko z pyłu")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/COD_STILL5-scaled-1920x1080-c-default-800x450.jpg")
    posters("Dzikie szaleństwo")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/better-go-mad-in-the-wild-stills-0-2751886-800x450.jpeg")
    posters("Efekt architektury")                                 shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/building-visions-2-stills-0-2871912-visionenbauen2-neworleans-6-c-ngf-800x450.jpeg")
    posters("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar") shouldBe Some("https://www.kinomuza.pl/content/uploads/2025/12/baby-2-800x450.png")
    posters("Fałszerz stulecia")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/falszerz-2-800x450.jpg")
    posters("Freak Show")                                         shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/1-3-800x450.jpg")
    posters("Gala Wręczenia Nagrody Wolności | Pieśni lasu")      shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/piesn-lasu-3-800x450.jpg")
    posters("Giulietta i duchy")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/3-4-800x450.jpg")
    posters("Guru")                                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/guru-2-800x450.jpg")
    posters("Głos z księżyca")                                    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/2-5-800x450.jpg")
    posters("Habibi Hussein")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/habibi-hussein-stills-0-2872523-1e3e5412-b9da-4718-8514-615d01bb39b6-800x450.jpeg")
    posters("Hamnet")                                             shouldBe Some("https://www.kinomuza.pl/content/uploads/2025/10/kadry-1-1-800x450.jpg")
    posters("Idź z duszą na dłoni")                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/idz-z-d-2-800x450.jpg")
    posters("Istoty czujące")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/istoty-cz-2-800x450.jpg")
    posters("Jak głęboka jest twoja miłość?")                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/jak-gleboka-3-800x450.jpg")
    posters("Jak zrobić film o betonie")                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/the-history-of-concrete-stills-0-2875208-the-history-of-concrete-promo-still-03-v2-1-1-800x450.jpeg")
    posters("Jane Elliot kontra reszta świata")                   shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/jane-elliott-against-the-world-stills-0-2869379-800x450.jpeg")
    posters("Kandydaci Śmierci")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/kandydat-3-800x450.jpg")
    posters("Krajobraz po bitwie")                                shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/krajobraz-3-800x450.jpg")
    posters("Kronika wypadków miłosnych")                         shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/kroniki-3-800x450.jpg")
    posters("Kłopotliwy niedźwiedź")                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/klopotliwy-niedzwiedz-800x450.jpg")
    posters("Layla")                                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2025/10/lalyla-2-800x450.jpg")
    posters("Mam rzekę we krwi")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/i-follow-rivers-stills-0-2810346-800x450.jpeg")
    posters("Mariinka")                                           shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/Mariinka-2-800x450.jpg")
    posters("Mi Amor")                                            shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/mi-amor-1-800x450.jpg")
    posters("Milcząca przyjaciółka")                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/04/milczaca-przyj-2-800x450.jpg")
    posters("Miłość, która zostaje")                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/milosc-800x450.jpg")
    posters("Moi Themersonowie")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/my-themersons-stills-0-2876697-moi-themersonowie-rez-c-marcin-borchardt-franciszka-i-stefan-1-800x450.jpeg")
    posters("Młodsza siostra")                                    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/mlodsza-siostra-1-800x450.jpg")
    posters("Najstarsza osoba na świecie")                        shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/Najstarsza-osoba-2-800x450.jpg")
    posters("Nie oglądaj się teraz")                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/Nie-oglądaj-się-teraz1-800x450.jpg")
    posters("Niedźwiedzica")                                      shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/niedzwiedzica-2-800x450.jpg")
    posters("Noce Cabirii")                                       shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/2-2-800x450.jpg")
    posters("Nomadland")                                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2021/02/1-12-800x450.jpg")
    posters("Nowa fala")                                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2025/11/3-7-800x450.jpg")
    posters("Nurt")                                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/nurt-2-800x450.jpg")
    posters("O czasie i wodzie")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/czas-i-woda-1-800x450.jpg")
    posters("Obcy")                                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/obcy-1-800x450.jpg")
    posters("Orwell: 2 + 2 = 5")                                 shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/orwel-800x450.jpg")
    posters("Osiem i pół")                                        shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/3-800x450.jpg")
    posters("Pan Nikt kontra Putin")                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2025/03/PavelTalankin_credit_František-Svatoš-_HIGH-800x450.jpg")
    posters("Pieniądze to wszystko")                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/Pieniądze-to-wszystko-2-800x450.jpg")
    posters("Pieśni lasu")                                        shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/piesn-lasu-3-800x450.jpg")
    posters("Poczta głosowa")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/poczta-glosowa-1-800x450.jpg")
    posters("Pomiędzy woskiem i złotem")                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/01_WAXGOLD_©_Ruth_Beckermann_Filmproduktion-800x450.jpg")
    posters("Proszę nie siadać")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/please-do-not-sit-stills-0-2821930-800x450.jpeg")
    posters("Przeżyj to sam")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/przezyj-to-sam-1-800x450.jpg")
    posters("Romería")                                            shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/rom-3-800x450.jpg")
    posters("Runa Simi")                                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/runa-simi-stills-0-2711098-800x450.jpeg")
    posters("Sny o słoniach")                                     shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/sny-o-sloniach-1-800x450.jpg")
    posters("Sygnalista")                                         shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/sygnalista-3-800x450.jpg")
    posters("Słodkie życie")                                      shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/3-3-800x450.jpg")
    posters("Top Gun")                                            shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/1-6-800x450.jpg")
    posters("Uciszone")                                           shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/silenced-stills-0-2829639-800x450.png")
    posters("Wałkonie")                                           shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/1-1-800x450.jpg")
    posters("Wielki Łuk")                                         shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/wielki-luk-2-800x450.jpg")
    posters("Windą na szafot")                                    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/winda-1-800x450.jpg")
    posters("Wolność po włosku")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/04/wolnosc-po-w-3-800x450.jpg")
    posters("Wpatrując się w słońce")                             shouldBe Some("https://www.kinomuza.pl/content/uploads/2025/12/WSWS_2026_3-800x450.jpg")
    posters("Wspinaczka")                                         shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/wspinaczka-800x450.jpg")
    posters("Wszyscy na Kenmure Street")                          shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/everybody-to-kenmure-street-stills-0-2882713-everybodytokenmurestreet-still1-800x450.jpeg")
    posters("Wszystko na sprzedaż")                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/wszystko_na_sprzedaz_06_gallery-800x450.jpg")
    posters("Wypadek fortepianowy")                               shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/05/wypadek-fortepianowy-2-800x450.jpg")
    posters("Yanuni")                                             shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/yanuni-2-800x450.jpg")
    posters("Zemsta Embriona | najlepsze z najgorszych")          shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/04/Zemsta-embriona-9-800x450.png")
    posters("Ziemia obiecana")                                    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/02/ziemia-o-3-800x450.jpg")
    posters("Znaki Pana Śliwki")                                  shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/04/znaki-p-3-800x450.jpg")
    posters("Ślady")                                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/traces-4-2048x1152-1-800x450.jpg")
    posters("Żądło")                                              shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/04/zadlo-1-800x450.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Barbara Forever")                                    shouldBe Some("https://www.kinomuza.pl/movie/barbara-forever/")
    filmUrls("Bez końca")                                          shouldBe Some("https://www.kinomuza.pl/movie/bez-konca/")
    filmUrls("Bez znieczulenia")                                   shouldBe Some("https://www.kinomuza.pl/movie/bez-znieczulenia/")
    filmUrls("Bibliotekarki")                                      shouldBe Some("https://www.kinomuza.pl/movie/bibliotekarki/")
    filmUrls("Broken English")                                     shouldBe Some("https://www.kinomuza.pl/movie/broken-english/")
    filmUrls("Brzezina")                                           shouldBe Some("https://www.kinomuza.pl/movie/93409/")
    filmUrls("Był sobie śnieg")                                    shouldBe Some("https://www.kinomuza.pl/movie/byl-sobie-snieg/")
    filmUrls("Chronologia wody")                                   shouldBe Some("https://www.kinomuza.pl/movie/chronologia-wody/")
    filmUrls("Co ukrywa Elon Musk?")                               shouldBe Some("https://www.kinomuza.pl/movie/co-ukrywa-elon-musk/")
    filmUrls("Człowiek z marmuru")                                 shouldBe Some("https://www.kinomuza.pl/movie/czlowiek-z-marmuru/")
    filmUrls("Drama")                                              shouldBe Some("https://www.kinomuza.pl/movie/drama/")
    filmUrls("Dziecko z pyłu")                                     shouldBe Some("https://www.kinomuza.pl/movie/dziecko-z-pylu/")
    filmUrls("Dzikie szaleństwo")                                  shouldBe Some("https://www.kinomuza.pl/movie/dzikie-szalenstwo/")
    filmUrls("Efekt architektury")                                 shouldBe Some("https://www.kinomuza.pl/movie/efekt-architektury/")
    filmUrls("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar") shouldBe Some("https://www.kinomuza.pl/movie/babystar/")
    filmUrls("Fałszerz stulecia")                                  shouldBe Some("https://www.kinomuza.pl/movie/falszerz-stulecia/")
    filmUrls("Freak Show")                                         shouldBe Some("https://www.kinomuza.pl/movie/freak-show/")
    filmUrls("Gala Wręczenia Nagrody Wolności | Pieśni lasu")      shouldBe Some("https://www.kinomuza.pl/movie/gala-nagrody-wolnosci-piesni-lasu/")
    filmUrls("Giulietta i duchy")                                  shouldBe Some("https://www.kinomuza.pl/movie/giulietta-i-duchy/")
    filmUrls("Guru")                                               shouldBe Some("https://www.kinomuza.pl/movie/guru/")
    filmUrls("Głos z księżyca")                                    shouldBe Some("https://www.kinomuza.pl/movie/glos-z-ksiezyca/")
    filmUrls("Habibi Hussein")                                     shouldBe Some("https://www.kinomuza.pl/movie/habibi-hussein/")
    filmUrls("Hamnet")                                             shouldBe Some("https://www.kinomuza.pl/movie/hamnet/")
    filmUrls("Idź z duszą na dłoni")                               shouldBe Some("https://www.kinomuza.pl/movie/idz-z-dusza-na-dloni/")
    filmUrls("Istoty czujące")                                     shouldBe Some("https://www.kinomuza.pl/movie/istoty-czujace/")
    filmUrls("Jak głęboka jest twoja miłość?")                     shouldBe Some("https://www.kinomuza.pl/movie/jak-gleboka-jest-twoja-milosc/")
    filmUrls("Jak zrobić film o betonie")                          shouldBe Some("https://www.kinomuza.pl/movie/jak-zrobic-film-o-betonie/")
    filmUrls("Jane Elliot kontra reszta świata")                   shouldBe Some("https://www.kinomuza.pl/movie/jane-elliot-kontra-reszta-swiata/")
    filmUrls("Kandydaci Śmierci")                                  shouldBe Some("https://www.kinomuza.pl/movie/kandydaci-smierci/")
    filmUrls("Krajobraz po bitwie")                                shouldBe Some("https://www.kinomuza.pl/movie/krajobraz-po-bitwie/")
    filmUrls("Kronika wypadków miłosnych")                         shouldBe Some("https://www.kinomuza.pl/movie/kronika-wypadkow-milosnych/")
    filmUrls("Kłopotliwy niedźwiedź")                              shouldBe Some("https://www.kinomuza.pl/movie/klopotliwy-niedzwiedz/")
    filmUrls("Layla")                                              shouldBe Some("https://www.kinomuza.pl/movie/layla/")
    filmUrls("Mam rzekę we krwi")                                  shouldBe Some("https://www.kinomuza.pl/movie/mam-rzeke-we-krwi/")
    filmUrls("Mariinka")                                           shouldBe Some("https://www.kinomuza.pl/movie/mariinka/")
    filmUrls("Mi Amor")                                            shouldBe Some("https://www.kinomuza.pl/movie/mi-amor/")
    filmUrls("Milcząca przyjaciółka")                              shouldBe Some("https://www.kinomuza.pl/movie/milczaca-przyjaciolka/")
    filmUrls("Miłość, która zostaje")                              shouldBe Some("https://www.kinomuza.pl/movie/milosc-ktora-zostaje/")
    filmUrls("Moi Themersonowie")                                  shouldBe Some("https://www.kinomuza.pl/movie/moi-themersonowie/")
    filmUrls("Młodsza siostra")                                    shouldBe Some("https://www.kinomuza.pl/movie/mlodsza-siostra/")
    filmUrls("Najstarsza osoba na świecie")                        shouldBe Some("https://www.kinomuza.pl/movie/najstarsza-osoba-na-swiecie/")
    filmUrls("Nie oglądaj się teraz")                              shouldBe Some("https://www.kinomuza.pl/movie/nie-ogladaj-sie-teraz/")
    filmUrls("Niedźwiedzica")                                      shouldBe Some("https://www.kinomuza.pl/movie/niedzwiedzica/")
    filmUrls("Noce Cabirii")                                       shouldBe Some("https://www.kinomuza.pl/movie/noce-cabirii/")
    filmUrls("Nomadland")                                          shouldBe Some("https://www.kinomuza.pl/movie/nomadland/")
    filmUrls("Nowa fala")                                          shouldBe Some("https://www.kinomuza.pl/movie/nowa-fala/")
    filmUrls("Nurt")                                               shouldBe Some("https://www.kinomuza.pl/movie/nurt/")
    filmUrls("O czasie i wodzie")                                  shouldBe Some("https://www.kinomuza.pl/movie/o-czasie-i-wodzie/")
    filmUrls("Obcy")                                               shouldBe Some("https://www.kinomuza.pl/movie/letranger/")
    filmUrls("Orwell: 2 + 2 = 5")                                 shouldBe Some("https://www.kinomuza.pl/movie/orwell-2-2-5/")
    filmUrls("Osiem i pół")                                        shouldBe Some("https://www.kinomuza.pl/movie/osiem-i-pol/")
    filmUrls("Pan Nikt kontra Putin")                              shouldBe Some("https://www.kinomuza.pl/movie/pan-nikt-kontra-putin/")
    filmUrls("Pieniądze to wszystko")                              shouldBe Some("https://www.kinomuza.pl/movie/pieniadze-to-wszystko/")
    filmUrls("Pieśni lasu")                                        shouldBe Some("https://www.kinomuza.pl/movie/piesni-lasu/")
    filmUrls("Poczta głosowa")                                     shouldBe Some("https://www.kinomuza.pl/movie/le-repondeur/")
    filmUrls("Pomiędzy woskiem i złotem")                          shouldBe Some("https://www.kinomuza.pl/movie/pomiedzy-woskiem-i-zlotem/")
    filmUrls("Proszę nie siadać")                                  shouldBe Some("https://www.kinomuza.pl/movie/prosze-nie-siadac/")
    filmUrls("Przeżyj to sam")                                     shouldBe Some("https://www.kinomuza.pl/movie/juste-une-illusion/")
    filmUrls("Romería")                                            shouldBe Some("https://www.kinomuza.pl/movie/romeria/")
    filmUrls("Runa Simi")                                          shouldBe Some("https://www.kinomuza.pl/movie/runa-simi/")
    filmUrls("Sny o słoniach")                                     shouldBe Some("https://www.kinomuza.pl/movie/sny-o-sloniach/")
    filmUrls("Sygnalista")                                         shouldBe Some("https://www.kinomuza.pl/movie/sygnalista/")
    filmUrls("Słodkie życie")                                      shouldBe Some("https://www.kinomuza.pl/movie/slodkie-zycie/")
    filmUrls("Top Gun")                                            shouldBe Some("https://www.kinomuza.pl/movie/top-gun/")
    filmUrls("Uciszone")                                           shouldBe Some("https://www.kinomuza.pl/movie/uciszone/")
    filmUrls("Wałkonie")                                           shouldBe Some("https://www.kinomuza.pl/movie/walkonie/")
    filmUrls("Wielki Łuk")                                         shouldBe Some("https://www.kinomuza.pl/movie/wielki-luk/")
    filmUrls("Windą na szafot")                                    shouldBe Some("https://www.kinomuza.pl/movie/winda-na-szafot/")
    filmUrls("Wolność po włosku")                                  shouldBe Some("https://www.kinomuza.pl/movie/wolnosc-po-wlosku/")
    filmUrls("Wpatrując się w słońce")                             shouldBe Some("https://www.kinomuza.pl/movie/wpatrujac-sie-w-slonce/")
    filmUrls("Wspinaczka")                                         shouldBe Some("https://www.kinomuza.pl/movie/wspinaczka/")
    filmUrls("Wszyscy na Kenmure Street")                          shouldBe Some("https://www.kinomuza.pl/movie/wszyscy-na-kenmure-street/")
    filmUrls("Wszystko na sprzedaż")                               shouldBe Some("https://www.kinomuza.pl/movie/wszystko-na-sprzedaz/")
    filmUrls("Wypadek fortepianowy")                               shouldBe Some("https://www.kinomuza.pl/movie/wypadek-fortepianowy/")
    filmUrls("Yanuni")                                             shouldBe Some("https://www.kinomuza.pl/movie/yanuni/")
    filmUrls("Zemsta Embriona | najlepsze z najgorszych")          shouldBe Some("https://www.kinomuza.pl/movie/zemsta-embriona/")
    filmUrls("Ziemia obiecana")                                    shouldBe Some("https://www.kinomuza.pl/movie/ziemia-obiecana/")
    filmUrls("Znaki Pana Śliwki")                                  shouldBe Some("https://www.kinomuza.pl/movie/znaki-pana-sliwki/")
    filmUrls("Ślady")                                              shouldBe Some("https://www.kinomuza.pl/movie/slady/")
    filmUrls("Żądło")                                              shouldBe Some("https://www.kinomuza.pl/movie/zadlo/")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for every movie" in {
    byTitle("Barbara Forever").director                                    shouldBe Some("Brydie O’Connor USA 2026 102’")
    byTitle("Bez końca").director                                          shouldBe Some("Michał Marczak Polska")
    byTitle("Bez znieczulenia").director                                   shouldBe Some("Andrzej Wajda Polska 1978 125’")
    byTitle("Bibliotekarki").director                                      shouldBe Some("Kim A. Snyder USA 2025 92’")
    byTitle("Broken English").director                                     shouldBe Some("Jane Pollard")
    byTitle("Brzezina").director                                           shouldBe Some("Andrzej Wajda Polska 1970 99’")
    byTitle("Był sobie śnieg").director                                    shouldBe Some("Nikolaus Geyrhalter Austria 2025 128’")
    byTitle("Chronologia wody").director                                   shouldBe Some("Kristen Stewart Francja")
    byTitle("Co ukrywa Elon Musk?").director                               shouldBe Some("Andreas Pichler Niemcy 2025 90’")
    byTitle("Człowiek z marmuru").director                                 shouldBe Some("Andrzej Wajda Polska 1977 153’")
    byTitle("Drama").director                                              shouldBe Some("Kristoffer Borgli USA 2026 105’")
    byTitle("Dziecko z pyłu").director                                     shouldBe Some("Weronika Mliczewska Polska")
    byTitle("Dzikie szaleństwo").director                                  shouldBe Some("Miro Remo Słowacja")
    byTitle("Efekt architektury").director                                 shouldBe Some("Diego Breit Lira Austria 2026 106’")
    byTitle("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar").director shouldBe Some("Joscha Bongard Niemcy 2025 98’")
    byTitle("Fałszerz stulecia").director                                  shouldBe Some("Jean-Paul Salomé Francja 2025 128’")
    byTitle("Freak Show").director                                         shouldBe Some("Łukasz Ronduda")
    byTitle("Gala Wręczenia Nagrody Wolności | Pieśni lasu").director      shouldBe Some("Vincent Munier Francja 2025 94’")
    byTitle("Giulietta i duchy").director                                  shouldBe Some("Federico Fellini Włochy")
    byTitle("Guru").director                                               shouldBe Some("Yann Gozlan Francja 2025 126’")
    byTitle("Głos z księżyca").director                                    shouldBe Some("Federico Fellini Włochy 1990 121’")
    byTitle("Habibi Hussein").director                                     shouldBe Some("Alex Bakri Palestyna")
    byTitle("Hamnet").director                                             shouldBe Some("Chloé Zhao Wielka Brytania 2025 125’")
    byTitle("Idź z duszą na dłoni").director                               shouldBe Some("Sepideh Farsi Francja")
    byTitle("Istoty czujące").director                                     shouldBe Some("Tony Jones Australia 2026 106’")
    byTitle("Jak głęboka jest twoja miłość?").director                     shouldBe Some("Eleanor Mortimer USA 2026 100’")
    byTitle("Jak zrobić film o betonie").director                          shouldBe Some("John Wilson USA 2026 101’")
    byTitle("Jane Elliot kontra reszta świata").director                   shouldBe Some("Judd Ehrlich USA 2026 99’")
    byTitle("Kandydaci Śmierci").director                                  shouldBe Some("Maciej Cuske Polska 2026 95’")
    byTitle("Krajobraz po bitwie").director                                shouldBe Some("Andrzej Wajda Polska 1970 101’")
    byTitle("Kronika wypadków miłosnych").director                         shouldBe Some("Andrzej Wajda Polska 1985 114’")
    byTitle("Kłopotliwy niedźwiedź").director                              shouldBe Some("Gabriela Osio Vanden")
    byTitle("Layla").director                                              shouldBe Some("Amrou Al-Kadhi Wielka Brytania 2024 100’")
    byTitle("Mam rzekę we krwi").director                                  shouldBe Some("Barbora Hollan Norwegia 2026 98’")
    byTitle("Mariinka").director                                           shouldBe Some("Pieter-Jan de Pue Belgia")
    byTitle("Mi Amor").director                                            shouldBe Some("Guillaume Nicloux Francja 2025 113’")
    byTitle("Milcząca przyjaciółka").director                              shouldBe Some("Ildikó Enyedi Niemcy")
    byTitle("Miłość, która zostaje").director                              shouldBe Some("Hlynur Pálmason Islandia")
    byTitle("Moi Themersonowie").director                                  shouldBe Some("Marcin Borchardt Polska 2026 80’")
    byTitle("Młodsza siostra").director                                    shouldBe Some("Hafsia Herzi Francja")
    byTitle("Najstarsza osoba na świecie").director                        shouldBe Some("Sam Green USA 2025 87’")
    byTitle("Nie oglądaj się teraz").director                              shouldBe Some("Nicolas Roeg Wielka Brytania")
    byTitle("Niedźwiedzica").director                                      shouldBe Some("Asgeir Helgestad Norwegia")
    byTitle("Noce Cabirii").director                                       shouldBe Some("Federico Fellini Włochy")
    byTitle("Nomadland").director                                          shouldBe Some("Chloé Zhao USA 2020 108’")
    byTitle("Nowa fala").director                                          shouldBe Some("Richard Linklater Francja")
    byTitle("Nurt").director                                               shouldBe Some("Rafał Skalski Polska 2026 83’")
    byTitle("O czasie i wodzie").director                                  shouldBe Some("Sara Dosa Islandia")
    byTitle("Obcy").director                                               shouldBe Some("François Ozon francja 2025 122’")
    byTitle("Orwell: 2 + 2 = 5").director                                 shouldBe Some("Raoul Peck Francja")
    byTitle("Osiem i pół").director                                        shouldBe Some("Federico Fellini Włochy")
    byTitle("Pan Nikt kontra Putin").director                              shouldBe Some("David Borenstein")
    byTitle("Pieniądze to wszystko").director                              shouldBe Some("Sinéad O'Shea Irlandia")
    byTitle("Pieśni lasu").director                                        shouldBe Some("Vincent Munier Francja 2025 94’")
    byTitle("Poczta głosowa").director                                     shouldBe Some("Fabienne Godet Francja 2024 102’")
    byTitle("Pomiędzy woskiem i złotem").director                          shouldBe Some("Ruth Beckermann Austria")
    byTitle("Proszę nie siadać").director                                  shouldBe Some("Piotr Małecki Polska 2026 60’")
    byTitle("Przeżyj to sam").director                                     shouldBe Some("Olivier Nakache")
    byTitle("Romería").director                                            shouldBe Some("Carla Simón Hiszpania 2025 111’")
    byTitle("Runa Simi").director                                          shouldBe Some("Augusto Zegarra Peru 2025 86’")
    byTitle("Sny o słoniach").director                                     shouldBe Some("Werner Herzog USA 2025 99’")
    byTitle("Sygnalista").director                                         shouldBe Some("Marc Bauder Niemcy 2026 91’")
    byTitle("Słodkie życie").director                                      shouldBe Some("Federico Fellini Włochy")
    byTitle("Top Gun").director                                            shouldBe Some("Tony Scott USA 1986 110’")
    byTitle("Uciszone").director                                           shouldBe Some("Selina Miles Australia 2026 92’")
    byTitle("Wałkonie").director                                           shouldBe Some("Federico Fellini Włochy")
    byTitle("Wielki Łuk").director                                         shouldBe Some("Stéphane Demoustier Francja 2025 106’")
    byTitle("Windą na szafot").director                                    shouldBe Some("Louis Malle Francja 1958 90’")
    byTitle("Wolność po włosku").director                                  shouldBe Some("Mario Martone Francja")
    byTitle("Wpatrując się w słońce").director                             shouldBe Some("Mascha Schilinski Niemcy 2025 155’")
    byTitle("Wspinaczka").director                                         shouldBe Some("Jon Glassberg Austria")
    byTitle("Wszyscy na Kenmure Street").director                          shouldBe Some("Felipe Bustos Sierra Wielka Brytania")
    byTitle("Wszystko na sprzedaż").director                               shouldBe Some("Andrzej Wajda Polska 1968 94’")
    byTitle("Wypadek fortepianowy").director                               shouldBe Some("Quentin Dupieux Francja 2025 88’")
    byTitle("Yanuni").director                                             shouldBe Some("Richard Ladkani Austria")
    byTitle("Zemsta Embriona | najlepsze z najgorszych").director          shouldBe Some("Francis Teri USA 1990 89’")
    byTitle("Ziemia obiecana").director                                    shouldBe Some("Andrzej Wajda Polska 1974 179’")
    byTitle("Znaki Pana Śliwki").director                                  shouldBe Some("Urszula Morga")
    byTitle("Ślady").director                                              shouldBe Some("Alisa Kovalenko")
    byTitle("Żądło").director                                              shouldBe Some("George Roy Hill USA 1973 129’")
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Barbara Forever")                                    shouldBe 1
    counts("Bez końca")                                          shouldBe 2
    counts("Bez znieczulenia")                                   shouldBe 1
    counts("Bibliotekarki")                                      shouldBe 1
    counts("Broken English")                                     shouldBe 2
    counts("Brzezina")                                           shouldBe 1
    counts("Był sobie śnieg")                                    shouldBe 1
    counts("Chronologia wody")                                   shouldBe 4
    counts("Co ukrywa Elon Musk?")                               shouldBe 2
    counts("Człowiek z marmuru")                                 shouldBe 1
    counts("Drama")                                              shouldBe 5
    counts("Dziecko z pyłu")                                     shouldBe 3
    counts("Dzikie szaleństwo")                                  shouldBe 1
    counts("Efekt architektury")                                 shouldBe 1
    counts("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar") shouldBe 1
    counts("Fałszerz stulecia")                                  shouldBe 1
    counts("Freak Show")                                         shouldBe 1
    counts("Gala Wręczenia Nagrody Wolności | Pieśni lasu")      shouldBe 1
    counts("Giulietta i duchy")                                  shouldBe 1
    counts("Guru")                                               shouldBe 1
    counts("Głos z księżyca")                                    shouldBe 1
    counts("Habibi Hussein")                                     shouldBe 1
    counts("Hamnet")                                             shouldBe 4
    counts("Idź z duszą na dłoni")                               shouldBe 1
    counts("Istoty czujące")                                     shouldBe 1
    counts("Jak głęboka jest twoja miłość?")                     shouldBe 1
    counts("Jak zrobić film o betonie")                          shouldBe 1
    counts("Jane Elliot kontra reszta świata")                   shouldBe 2
    counts("Kandydaci Śmierci")                                  shouldBe 2
    counts("Krajobraz po bitwie")                                shouldBe 1
    counts("Kronika wypadków miłosnych")                         shouldBe 1
    counts("Kłopotliwy niedźwiedź")                              shouldBe 2
    counts("Layla")                                              shouldBe 4
    counts("Mam rzekę we krwi")                                  shouldBe 1
    counts("Mariinka")                                           shouldBe 1
    counts("Mi Amor")                                            shouldBe 1
    counts("Milcząca przyjaciółka")                              shouldBe 1
    counts("Miłość, która zostaje")                              shouldBe 2
    counts("Moi Themersonowie")                                  shouldBe 1
    counts("Młodsza siostra")                                    shouldBe 1
    counts("Najstarsza osoba na świecie")                        shouldBe 1
    counts("Nie oglądaj się teraz")                              shouldBe 1
    counts("Niedźwiedzica")                                      shouldBe 1
    counts("Noce Cabirii")                                       shouldBe 1
    counts("Nomadland")                                          shouldBe 1
    counts("Nowa fala")                                          shouldBe 1
    counts("Nurt")                                               shouldBe 1
    counts("O czasie i wodzie")                                  shouldBe 1
    counts("Obcy")                                               shouldBe 1
    counts("Orwell: 2 + 2 = 5")                                 shouldBe 2
    counts("Osiem i pół")                                        shouldBe 1
    counts("Pan Nikt kontra Putin")                              shouldBe 1
    counts("Pieniądze to wszystko")                              shouldBe 1
    counts("Pieśni lasu")                                        shouldBe 1
    counts("Poczta głosowa")                                     shouldBe 1
    counts("Pomiędzy woskiem i złotem")                          shouldBe 1
    counts("Proszę nie siadać")                                  shouldBe 1
    counts("Przeżyj to sam")                                     shouldBe 1
    counts("Romería")                                            shouldBe 5
    counts("Runa Simi")                                          shouldBe 1
    counts("Sny o słoniach")                                     shouldBe 1
    counts("Sygnalista")                                         shouldBe 1
    counts("Słodkie życie")                                      shouldBe 1
    counts("Top Gun")                                            shouldBe 1
    counts("Uciszone")                                           shouldBe 1
    counts("Wałkonie")                                           shouldBe 1
    counts("Wielki Łuk")                                         shouldBe 1
    counts("Windą na szafot")                                    shouldBe 1
    counts("Wolność po włosku")                                  shouldBe 5
    counts("Wpatrując się w słońce")                             shouldBe 3
    counts("Wspinaczka")                                         shouldBe 1
    counts("Wszyscy na Kenmure Street")                          shouldBe 1
    counts("Wszystko na sprzedaż")                               shouldBe 1
    counts("Wypadek fortepianowy")                               shouldBe 1
    counts("Yanuni")                                             shouldBe 2
    counts("Zemsta Embriona | najlepsze z najgorszych")          shouldBe 2
    counts("Ziemia obiecana")                                    shouldBe 1
    counts("Znaki Pana Śliwki")                                  shouldBe 6
    counts("Ślady")                                              shouldBe 1
    counts("Żądło")                                              shouldBe 1
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for Znaki Pana Śliwki" in {
    val st = byTitle("Znaki Pana Śliwki").showtimes
    st.size shouldBe 6
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 18, 19, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927847#total"), Some("Sala 3"), None),
      Showtime(LocalDateTime.of(2026, 5, 19, 14, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927860#total"), Some("Sala 1"), None),
      Showtime(LocalDateTime.of(2026, 5, 19, 18, 0),  Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927863#total"), Some("Sala 3"), None),
      Showtime(LocalDateTime.of(2026, 5, 20, 17, 0),  Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927876#total"), Some("Sala 3"), None),
      Showtime(LocalDateTime.of(2026, 5, 21, 14, 45), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927968#total"), Some("Sala 1"), None),
      Showtime(LocalDateTime.of(2026, 5, 21, 19, 45), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927979#total"), Some("Sala 2"), None),
    )
  }

  it should "return exact showtimes for Drama" in {
    val st = byTitle("Drama").showtimes
    st.size shouldBe 5
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 18, 14, 30), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927835#total"), Some("Sala 2"), None),
      Showtime(LocalDateTime.of(2026, 5, 18, 20, 30), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927834#total"), Some("Sala 1"), None),
      Showtime(LocalDateTime.of(2026, 5, 19, 20, 30), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927868#total"), Some("Sala 2"), None),
      Showtime(LocalDateTime.of(2026, 5, 20, 21, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927870#total"), Some("Sala 1"), None),
      Showtime(LocalDateTime.of(2026, 5, 21, 21, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927980#total"), Some("Sala 2"), None),
    )
  }
}
