package clients.kino_muza

import clients.tools.FakeHttpFetch
import models.{KinoMuza, Showtime}
import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoMuzaClient

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
      "Zemsta Embriona",
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
    runtimes("Zemsta Embriona")          shouldBe Some(89)
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
    byTitle("Zemsta Embriona").movie.releaseYear          shouldBe Some(1990)
    byTitle("Ziemia obiecana").movie.releaseYear                                    shouldBe Some(1974)
    byTitle("Znaki Pana Śliwki").movie.releaseYear                                  shouldBe Some(2025)
    byTitle("Ślady").movie.releaseYear                                              shouldBe Some(2026)
    byTitle("Żądło").movie.releaseYear                                              shouldBe Some(1973)
  }

  // ── Production country ────────────────────────────────────────────────────

  it should "return correct production country for every movie" in {
    byTitle("Barbara Forever").movie.countries shouldBe Seq("USA")
    byTitle("Bez końca").movie.countries shouldBe Seq("Polska", "Francja")
    byTitle("Bez znieczulenia").movie.countries shouldBe Seq("Polska")
    byTitle("Bibliotekarki").movie.countries shouldBe Seq("USA")
    byTitle("Broken English").movie.countries shouldBe Seq("Wielka Brytania")
    byTitle("Brzezina").movie.countries shouldBe Seq("Polska")
    byTitle("Był sobie śnieg").movie.countries shouldBe Seq("Austria")
    byTitle("Chronologia wody").movie.countries shouldBe Seq("Francja", "Łotwa", "USA")
    byTitle("Co ukrywa Elon Musk?").movie.countries shouldBe Seq("Niemcy")
    byTitle("Człowiek z marmuru").movie.countries shouldBe Seq("Polska")
    byTitle("Drama").movie.countries shouldBe Seq("USA")
    byTitle("Dziecko z pyłu").movie.countries shouldBe Seq("Polska", "Wietnam", "Szwecja", "Czechy", "Katar")
    byTitle("Dzikie szaleństwo").movie.countries shouldBe Seq("Słowacja", "Czechy")
    byTitle("Efekt architektury").movie.countries shouldBe Seq("Austria")
    byTitle("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar").movie.countries shouldBe Seq("Niemcy")
    byTitle("Fałszerz stulecia").movie.countries shouldBe Seq("Francja")
    byTitle("Freak Show").movie.countries shouldBe Seq("Polska")
    byTitle("Gala Wręczenia Nagrody Wolności | Pieśni lasu").movie.countries shouldBe Seq("Francja")
    byTitle("Giulietta i duchy").movie.countries shouldBe Seq("Włochy", "Francja")
    byTitle("Guru").movie.countries shouldBe Seq("Francja")
    byTitle("Głos z księżyca").movie.countries shouldBe Seq("Włochy")
    byTitle("Habibi Hussein").movie.countries shouldBe Seq("Palestyna", "Niemcy", "Arabia Saudyjska", "Szwecja")
    byTitle("Hamnet").movie.countries shouldBe Seq("Wielka Brytania")
    byTitle("Idź z duszą na dłoni").movie.countries shouldBe Seq("Francja", "Palestyna", "Iran")
    byTitle("Istoty czujące").movie.countries shouldBe Seq("Australia")
    byTitle("Jak głęboka jest twoja miłość?").movie.countries shouldBe Seq("USA")
    byTitle("Jak zrobić film o betonie").movie.countries shouldBe Seq("USA")
    byTitle("Jane Elliot kontra reszta świata").movie.countries shouldBe Seq("USA")
    byTitle("Kandydaci Śmierci").movie.countries shouldBe Seq("Polska")
    byTitle("Krajobraz po bitwie").movie.countries shouldBe Seq("Polska")
    byTitle("Kronika wypadków miłosnych").movie.countries shouldBe Seq("Polska")
    byTitle("Kłopotliwy niedźwiedź").movie.countries shouldBe Seq("USA", "Kanada", "Wielka Brytania")
    byTitle("Layla").movie.countries shouldBe Seq("Wielka Brytania")
    byTitle("Mam rzekę we krwi").movie.countries shouldBe Seq("Norwegia")
    byTitle("Mariinka").movie.countries shouldBe Seq("Belgia", "Niderlandy")
    byTitle("Mi Amor").movie.countries shouldBe Seq("Francja")
    byTitle("Milcząca przyjaciółka").movie.countries shouldBe Seq("Niemcy", "Francja", "Węgry")
    byTitle("Miłość, która zostaje").movie.countries shouldBe Seq("Islandia", "Dania", "Szwecja", "Francja")
    byTitle("Moi Themersonowie").movie.countries shouldBe Seq("Polska")
    byTitle("Młodsza siostra").movie.countries shouldBe Seq("Francja", "Niemcy")
    byTitle("Najstarsza osoba na świecie").movie.countries shouldBe Seq("USA")
    byTitle("Nie oglądaj się teraz").movie.countries shouldBe Seq("Wielka Brytania", "Włochy")
    byTitle("Niedźwiedzica").movie.countries shouldBe Seq("Norwegia", "Niemcy")
    byTitle("Noce Cabirii").movie.countries shouldBe Seq("Włochy", "Francja")
    byTitle("Nomadland").movie.countries shouldBe Seq("USA")
    byTitle("Nowa fala").movie.countries shouldBe Seq("Francja", "USA")
    byTitle("Nurt").movie.countries shouldBe Seq("Polska")
    byTitle("O czasie i wodzie").movie.countries shouldBe Seq("Islandia", "USA")
    byTitle("Obcy").movie.countries shouldBe Seq("francja")
    byTitle("Orwell: 2 + 2 = 5").movie.countries shouldBe Seq("Francja", "USA")
    byTitle("Osiem i pół").movie.countries shouldBe Seq("Włochy", "Francja")
    byTitle("Pan Nikt kontra Putin").movie.countries shouldBe Seq("Dania", "Czechy")
    byTitle("Pieniądze to wszystko").movie.countries shouldBe Seq("Irlandia", "Dania")
    byTitle("Pieśni lasu").movie.countries shouldBe Seq("Francja")
    byTitle("Poczta głosowa").movie.countries shouldBe Seq("Francja")
    byTitle("Pomiędzy woskiem i złotem").movie.countries shouldBe Seq("Austria", "Włochy")
    byTitle("Proszę nie siadać").movie.countries shouldBe Seq("Polska")
    byTitle("Przeżyj to sam").movie.countries shouldBe Seq("Francja")
    byTitle("Romería").movie.countries shouldBe Seq("Hiszpania")
    byTitle("Runa Simi").movie.countries shouldBe Seq("Peru")
    byTitle("Sny o słoniach").movie.countries shouldBe Seq("USA")
    byTitle("Sygnalista").movie.countries shouldBe Seq("Niemcy")
    byTitle("Słodkie życie").movie.countries shouldBe Seq("Włochy", "Francja")
    byTitle("Top Gun").movie.countries shouldBe Seq("USA")
    byTitle("Uciszone").movie.countries shouldBe Seq("Australia")
    byTitle("Wałkonie").movie.countries shouldBe Seq("Włochy", "Francja")
    byTitle("Wielki Łuk").movie.countries shouldBe Seq("Francja")
    byTitle("Windą na szafot").movie.countries shouldBe Seq("Francja")
    byTitle("Wolność po włosku").movie.countries shouldBe Seq("Francja", "Włochy")
    byTitle("Wpatrując się w słońce").movie.countries shouldBe Seq("Niemcy")
    byTitle("Wspinaczka").movie.countries shouldBe Seq("Austria", "USA")
    byTitle("Wszyscy na Kenmure Street").movie.countries shouldBe Seq("Wielka Brytania", "Katar")
    byTitle("Wszystko na sprzedaż").movie.countries shouldBe Seq("Polska")
    byTitle("Wypadek fortepianowy").movie.countries shouldBe Seq("Francja")
    byTitle("Yanuni").movie.countries shouldBe Seq("Austria", "Brazylia", "USA", "Kanada", "Niemcy")
    byTitle("Zemsta Embriona").movie.countries shouldBe Seq("USA")
    byTitle("Ziemia obiecana").movie.countries shouldBe Seq("Polska")
    byTitle("Znaki Pana Śliwki").movie.countries shouldBe Seq("Polska")
    byTitle("Ślady").movie.countries shouldBe Seq("Ukraina", "Polska")
    byTitle("Żądło").movie.countries shouldBe Seq("USA")
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────
  //
  // The listing page only carries a landscape-cropped thumbnail; the
  // higher-fidelity portrait poster lives on each film's detail page and is
  // pulled by the deferred `fetchFilmDetail`. Keeping the listing's posterUrl
  // intentionally None means `MovieCache.recordCinemaScrape`'s slot merge
  // can't undo the detail-page portrait poster on subsequent ticks.

  it should "return posterUrl = None for every film (detail-page poster filled by fetchFilmDetail)" in {
    results.flatMap(_.posterUrl) shouldBe empty
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
    filmUrls("Zemsta Embriona")          shouldBe Some("https://www.kinomuza.pl/movie/zemsta-embriona/")
    filmUrls("Ziemia obiecana")                                    shouldBe Some("https://www.kinomuza.pl/movie/ziemia-obiecana/")
    filmUrls("Znaki Pana Śliwki")                                  shouldBe Some("https://www.kinomuza.pl/movie/znaki-pana-sliwki/")
    filmUrls("Ślady")                                              shouldBe Some("https://www.kinomuza.pl/movie/slady/")
    filmUrls("Żądło")                                              shouldBe Some("https://www.kinomuza.pl/movie/zadlo/")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for every movie" in {
    byTitle("Barbara Forever").director shouldBe Seq("Brydie O’Connor")
    byTitle("Bez końca").director shouldBe Seq("Michał Marczak")
    byTitle("Bez znieczulenia").director shouldBe Seq("Andrzej Wajda")
    byTitle("Bibliotekarki").director shouldBe Seq("Kim A. Snyder")
    byTitle("Broken English").director shouldBe Seq("Jane Pollard", "Iain Forsyth")
    byTitle("Brzezina").director shouldBe Seq("Andrzej Wajda")
    byTitle("Był sobie śnieg").director shouldBe Seq("Nikolaus Geyrhalter")
    byTitle("Chronologia wody").director shouldBe Seq("Kristen Stewart")
    byTitle("Co ukrywa Elon Musk?").director shouldBe Seq("Andreas Pichler")
    byTitle("Człowiek z marmuru").director shouldBe Seq("Andrzej Wajda")
    byTitle("Drama").director shouldBe Seq("Kristoffer Borgli")
    byTitle("Dziecko z pyłu").director shouldBe Seq("Weronika Mliczewska")
    byTitle("Dzikie szaleństwo").director shouldBe Seq("Miro Remo")
    byTitle("Efekt architektury").director shouldBe Seq("Diego Breit Lira")
    byTitle("FILMOCZULE DLA EDUKACJI z ODN i WZiSS UMP – Babystar").director shouldBe Seq("Joscha Bongard")
    byTitle("Fałszerz stulecia").director shouldBe Seq("Jean-Paul Salomé")
    byTitle("Freak Show").director shouldBe Seq("Łukasz Ronduda", "Filip Pawlak")
    byTitle("Gala Wręczenia Nagrody Wolności | Pieśni lasu").director shouldBe Seq("Vincent Munier")
    byTitle("Giulietta i duchy").director shouldBe Seq("Federico Fellini")
    byTitle("Guru").director shouldBe Seq("Yann Gozlan")
    byTitle("Głos z księżyca").director shouldBe Seq("Federico Fellini")
    byTitle("Habibi Hussein").director shouldBe Seq("Alex Bakri")
    byTitle("Hamnet").director shouldBe Seq("Chloé Zhao")
    byTitle("Idź z duszą na dłoni").director shouldBe Seq("Sepideh Farsi")
    byTitle("Istoty czujące").director shouldBe Seq("Tony Jones")
    byTitle("Jak głęboka jest twoja miłość?").director shouldBe Seq("Eleanor Mortimer")
    byTitle("Jak zrobić film o betonie").director shouldBe Seq("John Wilson")
    byTitle("Jane Elliot kontra reszta świata").director shouldBe Seq("Judd Ehrlich")
    byTitle("Kandydaci Śmierci").director shouldBe Seq("Maciej Cuske")
    byTitle("Krajobraz po bitwie").director shouldBe Seq("Andrzej Wajda")
    byTitle("Kronika wypadków miłosnych").director shouldBe Seq("Andrzej Wajda")
    byTitle("Kłopotliwy niedźwiedź").director shouldBe Seq("Gabriela Osio Vanden", "Jack Weisman")
    byTitle("Layla").director shouldBe Seq("Amrou Al-Kadhi")
    byTitle("Mam rzekę we krwi").director shouldBe Seq("Barbora Hollan")
    byTitle("Mariinka").director shouldBe Seq("Pieter-Jan de Pue")
    byTitle("Mi Amor").director shouldBe Seq("Guillaume Nicloux")
    byTitle("Milcząca przyjaciółka").director shouldBe Seq("Ildikó Enyedi")
    byTitle("Miłość, która zostaje").director shouldBe Seq("Hlynur Pálmason")
    byTitle("Moi Themersonowie").director shouldBe Seq("Marcin Borchardt")
    byTitle("Młodsza siostra").director shouldBe Seq("Hafsia Herzi")
    byTitle("Najstarsza osoba na świecie").director shouldBe Seq("Sam Green")
    byTitle("Nie oglądaj się teraz").director shouldBe Seq("Nicolas Roeg")
    byTitle("Niedźwiedzica").director shouldBe Seq("Asgeir Helgestad")
    byTitle("Noce Cabirii").director shouldBe Seq("Federico Fellini")
    byTitle("Nomadland").director shouldBe Seq("Chloé Zhao")
    byTitle("Nowa fala").director shouldBe Seq("Richard Linklater")
    byTitle("Nurt").director shouldBe Seq("Rafał Skalski")
    byTitle("O czasie i wodzie").director shouldBe Seq("Sara Dosa")
    byTitle("Obcy").director shouldBe Seq("François Ozon")
    byTitle("Orwell: 2 + 2 = 5").director shouldBe Seq("Raoul Peck")
    byTitle("Osiem i pół").director shouldBe Seq("Federico Fellini")
    byTitle("Pan Nikt kontra Putin").director shouldBe Seq("David Borenstein", "Pasha Talankin")
    byTitle("Pieniądze to wszystko").director shouldBe Seq("Sinéad O'Shea")
    byTitle("Pieśni lasu").director shouldBe Seq("Vincent Munier")
    byTitle("Poczta głosowa").director shouldBe Seq("Fabienne Godet")
    byTitle("Pomiędzy woskiem i złotem").director shouldBe Seq("Ruth Beckermann")
    byTitle("Proszę nie siadać").director shouldBe Seq("Piotr Małecki")
    byTitle("Przeżyj to sam").director shouldBe Seq("Olivier Nakache", "Eric Toledano")
    byTitle("Romería").director shouldBe Seq("Carla Simón")
    byTitle("Runa Simi").director shouldBe Seq("Augusto Zegarra")
    byTitle("Sny o słoniach").director shouldBe Seq("Werner Herzog")
    byTitle("Sygnalista").director shouldBe Seq("Marc Bauder")
    byTitle("Słodkie życie").director shouldBe Seq("Federico Fellini")
    byTitle("Top Gun").director shouldBe Seq("Tony Scott")
    byTitle("Uciszone").director shouldBe Seq("Selina Miles")
    byTitle("Wałkonie").director shouldBe Seq("Federico Fellini")
    byTitle("Wielki Łuk").director shouldBe Seq("Stéphane Demoustier")
    byTitle("Windą na szafot").director shouldBe Seq("Louis Malle")
    byTitle("Wolność po włosku").director shouldBe Seq("Mario Martone")
    byTitle("Wpatrując się w słońce").director shouldBe Seq("Mascha Schilinski")
    byTitle("Wspinaczka").director shouldBe Seq("Jon Glassberg")
    byTitle("Wszyscy na Kenmure Street").director shouldBe Seq("Felipe Bustos Sierra")
    byTitle("Wszystko na sprzedaż").director shouldBe Seq("Andrzej Wajda")
    byTitle("Wypadek fortepianowy").director shouldBe Seq("Quentin Dupieux")
    byTitle("Yanuni").director shouldBe Seq("Richard Ladkani")
    byTitle("Zemsta Embriona").director shouldBe Seq("Francis Teri")
    byTitle("Ziemia obiecana").director shouldBe Seq("Andrzej Wajda")
    byTitle("Znaki Pana Śliwki").director shouldBe Seq("Urszula Morga", "Bartosz Mikołajczyk")
    byTitle("Ślady").director shouldBe Seq("Alisa Kovalenko", "Marysia Nikitiuk")
    byTitle("Żądło").director shouldBe Seq("George Roy Hill")
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
    counts("Zemsta Embriona")          shouldBe 2
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
      Showtime(LocalDateTime.of(2026, 5, 18, 19, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927847#total"), Some("Sala 3"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 19, 14, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927860#total"), Some("Sala 1"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 19, 18, 0),  Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927863#total"), Some("Sala 3"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 20, 17, 0),  Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927876#total"), Some("Sala 3"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 21, 14, 45), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927968#total"), Some("Sala 1"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 21, 19, 45), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927979#total"), Some("Sala 2"), Nil),
    )
  }

  // ── Synopsis ──────────────────────────────────────────────────────────────
  //
  // The listing scrape no longer fetches per-film detail pages — Muza's
  // burst limiter doesn't tolerate 80+ detail-page requests every tick.
  // Per-film synopsis/poster/trailer is DEFERRED to an `EnrichDetails` queue
  // task that calls `fetchFilmDetail` (see that section below). `fetch()`
  // returns CinemaMovies with `synopsis = None`; the listing data (title,
  // director, runtime, year, country, showtimes) is still rich enough to
  // render the home-page card and to resolve TMDB without the detail.

  it should "return synopsis = None for every film (detail fetch deferred to fetchFilmDetail)" in {
    results.flatMap(_.synopsis) shouldBe empty
  }

  it should "expose parseSynopsis for fetchFilmDetail to call against a recorded detail page" in {
    val html = scala.io.Source.fromFile("test/resources/fixtures/kino-muza/www.kinomuza.pl/movie/pieniadze-to-wszystko")(using scala.io.Codec.UTF8).mkString
    val s    = client.parseSynopsis(Jsoup.parse(html))
    s                            should not be empty
    s.get                        should startWith ("James Cox Chambers Jr.")
    s.get                        should include ("Fergie")
  }

  // Muza's detail page hosts a higher-fidelity portrait poster than the
  // listing-page thumbnail. `parsePoster` pulls it out so `fetchFilmDetail`
  // can upgrade the row's posterUrl when its EnrichDetails task runs.
  it should "extract the portrait poster URL from a detail page" in {
    val html = scala.io.Source.fromFile("test/resources/fixtures/kino-muza/www.kinomuza.pl/movie/pieniadze-to-wszystko")(using scala.io.Codec.UTF8).mkString
    client.parsePoster(Jsoup.parse(html)) shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/Pieniądze-to-wszystko-556x800.png")
  }

  it should "return None when the detail page has no poster image slot" in {
    client.parsePoster(Jsoup.parse("<html><body><p>no images here</p></body></html>")) shouldBe empty
  }

  it should "return exact showtimes for Drama" in {
    val st = byTitle("Drama").showtimes
    st.size shouldBe 5
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 18, 14, 30), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927835#total"), Some("Sala 2"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 18, 20, 30), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927834#total"), Some("Sala 1"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 19, 20, 30), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927868#total"), Some("Sala 2"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 20, 21, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927870#total"), Some("Sala 1"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 21, 21, 15), Some("https://estradapoznan.bilety24.pl/kup-bilety/?id=927980#total"), Some("Sala 2"), Nil),
    )
  }

  // ── Deferred per-film detail (DetailEnricher.fetchFilmDetail) ──────────────
  //
  // KinoMuza opts into the standard deferred-detail pipeline: synopsis / poster
  // / trailer are filled by a deduped `EnrichDetails` task calling
  // `fetchFilmDetail` (one detail-page request covers all three). Its listing
  // already carries the TMDB hints (director/year), so `defersTmdbResolution`
  // is false — resolution doesn't wait on this fetch.

  "KinoMuzaClient (DetailEnricher)" should "advertise the kino-muza detail group and not defer TMDB resolution" in {
    client.detailGroup          shouldBe "kino-muza"
    client.defersTmdbResolution shouldBe false
  }

  it should "fetchFilmDetail: pull synopsis + portrait poster from a detail page" in {
    val detail = client.fetchFilmDetail("https://www.kinomuza.pl/movie/pieniadze-to-wszystko/")
    detail                  should not be empty
    detail.get.synopsis.get should startWith ("James Cox Chambers Jr.")
    detail.get.posterUrl    shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/Pieniądze-to-wszystko-556x800.png")
    detail.get.trailerUrl   shouldBe None  // this page embeds no trailer iframe
  }

  it should "fetchFilmDetail: pull the trailer URL when the detail page embeds one" in {
    val detail = client.fetchFilmDetail("https://www.kinomuza.pl/movie/dziecko-z-pylu/")
    detail.get.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=h9r7lx9yDXk")
    detail.get.synopsis   should not be empty  // same fetch carries the synopsis
  }

  it should "fetchFilmDetail: return None on a fetch failure so the task stays stale and retries" in {
    val failing = new FakeHttpFetch("kino-muza") {
      override def get(url: String): String = throw new java.io.IOException("simulated fetch failure")
    }
    new KinoMuzaClient(failing).fetchFilmDetail("https://www.kinomuza.pl/movie/foo/") shouldBe None
  }

  it should "fetchFilmDetail: return Some with no synopsis when the page lacks a synopsis paragraph (marks fresh, no spin)" in {
    val noSynopsis = new FakeHttpFetch("kino-muza") {
      override def get(url: String): String =
        "<html><body><div class='col-11 paragraph'><p>not the synopsis div</p></div></body></html>"
    }
    val detail = new KinoMuzaClient(noSynopsis).fetchFilmDetail("https://www.kinomuza.pl/movie/foo/")
    detail              should not be empty  // page fetched OK → Some, so the handler marks it fresh
    detail.get.synopsis shouldBe None
  }

  // Regression: cycle/event pages mix organiser-link paragraphs (an <a> to a
  // festival, Instagram/Facebook, the ticket page) and plain-text URLs into the
  // synopsis column. Drop the link paragraphs and strip residual URLs.
  // (The exact prod page isn't in the corpus; the structure is reproduced here.)
  it should "parseSynopsis: drop link paragraphs and strip plain-text URLs" in {
    val html =
      """<div class="col-lg-7 paragraph">
        |<p>Prawdziwy opis filmu, dwa zdania o jego fabule i bohaterach.</p>
        |<p>Więcej informacji na www.kinomuza.pl o pokazie.</p>
        |<p><a href="https://instagram.com/x">Instagram organizatora</a></p>
        |</div>""".stripMargin
    val s = client.parseSynopsis(Jsoup.parse(html)).getOrElse(fail("no synopsis"))
    s should include ("Prawdziwy opis filmu")
    s should not include "www."
    s should not include "instagram"
    s should not include "Instagram organizatora" // link paragraph dropped whole
  }
}
