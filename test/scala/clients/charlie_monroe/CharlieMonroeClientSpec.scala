package clients.charlie_monroe

import clients.tools.FakeHttpFetch
import models.{CharlieMonroe, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CharlieMonroeClient

import java.time.LocalDateTime

class CharlieMonroeClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new CharlieMonroeClient(new FakeHttpFetch("charlie-monroe"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "CharlieMonroeClient.fetch" should "return exactly 37 movies from fixture" in {
    results.size shouldBe 37
  }

  it should "return 76 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 76
  }

  it should "assign CharlieMonroe cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(CharlieMonroe)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Bałtyk",
      "Bez wyjścia",
      "Chronologia wody",
      "Ciemna strona Mount Everest",
      "Człowiek z marmuru",
      "Drama",
      "Dźwięki miłości",
      "Father Mother Sister Brother",
      "Flow",
      "Glorious Summer",
      "Głos Hind Rajab",
      "Harry Angel",
      "Kicia Kocia ma braciszka",
      "Kopnęłabym cię, gdybym mogła",
      "Kurozając i świątynia Świstaka",
      "La Grazia",
      "Layla",
      "Left-Handed Girl. To była ręka... diabła!",
      "Maryja. Matka Papieża",
      "Matador",
      "Miłość w czasach apokalipsy",
      "Nie ma duchów w mieszkaniu na Dobrej",
      "Niewinni czarodzieje",
      "Opętanie",
      "Ostatnia sesja w Paryżu",
      "Pan Nikt kontra Putin",
      "Pod skórą",
      "Podziemny krąg",
      "Pucio",
      "Rocznica",
      "Romería",
      "Silver",
      "Wartość sentymentalna",
      "Wielki błękit",
      "Wpatrując się w słońce",
      "Znaki Pana Śliwki",
      "Ścieżki życia",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("Bałtyk")                                     shouldBe Some(66)
    runtimes("Bez wyjścia")                                shouldBe Some(139)
    runtimes("Chronologia wody")                           shouldBe Some(128)
    runtimes("Ciemna strona Mount Everest")                shouldBe Some(89)
    runtimes("Człowiek z marmuru")                         shouldBe Some(153)
    runtimes("Drama")                                      shouldBe Some(106)
    runtimes("Dźwięki miłości")                            shouldBe Some(99)
    runtimes("Father Mother Sister Brother")               shouldBe Some(111)
    runtimes("Flow")                                       shouldBe Some(85)
    runtimes("Glorious Summer")                            shouldBe Some(90)
    runtimes("Głos Hind Rajab")                            shouldBe Some(89)
    runtimes("Harry Angel")                                shouldBe Some(114)
    runtimes("Kicia Kocia ma braciszka")                   shouldBe Some(45)
    runtimes("Kopnęłabym cię, gdybym mogła")               shouldBe Some(113)
    runtimes("Kurozając i świątynia Świstaka")             shouldBe Some(88)
    runtimes("La Grazia")                                  shouldBe Some(133)
    runtimes("Layla")                                      shouldBe Some(100)
    runtimes("Left-Handed Girl. To była ręka... diabła!")  shouldBe Some(109)
    runtimes("Maryja. Matka Papieża")                      shouldBe Some(79)
    runtimes("Matador")                                    shouldBe Some(105)
    runtimes("Miłość w czasach apokalipsy")                shouldBe Some(97)
    runtimes("Nie ma duchów w mieszkaniu na Dobrej")       shouldBe Some(90)
    runtimes("Niewinni czarodzieje")                       shouldBe Some(84)
    runtimes("Opętanie")                                   shouldBe Some(124)
    runtimes("Ostatnia sesja w Paryżu")                    shouldBe Some(103)
    runtimes("Pan Nikt kontra Putin")                      shouldBe Some(90)
    runtimes("Pod skórą")                                  shouldBe Some(108)
    runtimes("Podziemny krąg")                             shouldBe Some(139)
    runtimes("Pucio")                                      shouldBe Some(45)
    runtimes("Rocznica")                                   shouldBe Some(112)
    runtimes("Romería")                                    shouldBe Some(114)
    runtimes("Silver")                                     shouldBe Some(78)
    runtimes("Wartość sentymentalna")                      shouldBe Some(133)
    runtimes("Wielki błękit")                              shouldBe Some(137)
    runtimes("Wpatrując się w słońce")                     shouldBe Some(155)
    runtimes("Znaki Pana Śliwki")                          shouldBe Some(72)
    runtimes("Ścieżki życia")                              shouldBe Some(115)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Bałtyk")                                     shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/08/BALTYK-PLAKAT-W-KINACH-22.08-jpg-po-zmniejszeniu-200x288.jpg")
    posters("Bez wyjścia")                                shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/03/Bez-wyjscia_plakat_B1_HQ-po-zmniejszeniu-200x288.jpg")
    posters("Chronologia wody")                           shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/CHRONOLOGIA-WODY-PLAKAT-PL-clean-po-zmniejszeniu-200x288.jpg")
    posters("Ciemna strona Mount Everest")                shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/12/Ciemna-strona-Mount-Everest_plakat-PL-po-zmniejszeniu-200x288.jpg")
    posters("Człowiek z marmuru")                         shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/03/Plakat-WAJDA-jpg-200x288.jpg")
    posters("Drama")                                      shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/robert-pattinson-i-zendaya-zapraszaja-na-slub-drama-w-kinach-w-kwietniu-2026-zobacz-plakat-i-zwiastun-unknown-1-200x288.webp")
    posters("Dźwięki miłości")                            shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/11/DzwiekiMilosci_B1_Net-200x288.jpg")
    posters("Father Mother Sister Brother")               shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/12/FMSB-plakat-PL_HQ-200x288.jpg")
    posters("Flow")                                       shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/01/flow-200x288.jpg")
    posters("Glorious Summer")                            shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/02/GLORIOUS-SUMMER_PLAKAT-200x288.jpg")
    posters("Głos Hind Rajab")                            shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/01/Glos_Hind_Rajab-polski-plakat1-po-zmniejszeniu-200x288.jpg")
    posters("Harry Angel")                                shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/10/plakat-Harry-200x288.webp")
    posters("Kicia Kocia ma braciszka")                   shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/11/Kicia-Kocia-ma-braciszka-PLAKAT-online-po-zmniejszeniu-200x288.jpg")
    posters("Kopnęłabym cię, gdybym mogła")               shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/12/KOPNELABYM-CIE_patroni-po-zmniejszeniu-200x288.jpg")
    posters("Kurozając i świątynia Świstaka")             shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/05/Kurozajac_B1_net-po-zmniejszeniu-200x288.jpg")
    posters("La Grazia")                                  shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/12/LaGrazia_plakat-B1_HQ-po-zmniejszeniu-200x288.jpg")
    posters("Layla")                                      shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/Layla_Plakat-po-zmniejszeniu-200x288.jpg")
    posters("Left-Handed Girl. To była ręka... diabła!")  shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/11/LEFT-HANDED-GIRL_PLAKAT-PL-jpg-200x288.jpg")
    posters("Maryja. Matka Papieża")                      shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/Maryja-Matka-Papieza-scaled-1-200x288.jpg")
    posters("Matador")                                    shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/05/jBBLDaF7j3EeTPkOwDPqdfSCWWk-200x288.jpg")
    posters("Miłość w czasach apokalipsy")                shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/milosc-w-czasach-apo_plakat-200x288.jpg")
    posters("Nie ma duchów w mieszkaniu na Dobrej")       shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/03/NIE-MA-DUCHOW_PLAKAT-po-zmniejszeniu-200x288.jpg")
    posters("Niewinni czarodzieje")                       shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/03/Plakat-WAJDA-jpg-200x288.jpg")
    posters("Opętanie")                                   shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/04/plw5syL13CNWIxGaqhuSOlLPUM2-200x288.jpg")
    posters("Ostatnia sesja w Paryżu")                    shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/03/OstatniaSesja_B1_patroni-po-zmniejszeniu-200x288.jpg")
    posters("Pan Nikt kontra Putin")                      shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/02/Pan-NIKT-kontra-PUTIN-plakat-net-po-zmniejszeniu-200x288.jpg")
    posters("Pod skórą")                                  shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2024/11/463315300_122193112556195240_4187623239888508464_n-200x288.jpg")
    posters("Podziemny krąg")                             shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/Podziemny-krag_online_cut-200x288.webp")
    posters("Pucio")                                      shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/Pucio-PLAKAT-online-po-zmniejszeniu-200x288.jpg")
    posters("Rocznica")                                   shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/11/rocznica-rocznica-plakat-finalny-scaled-1-200x288.webp")
    posters("Romería")                                    shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/05/Romeria-B1-po-zmniejszeniu-200x288.jpg")
    posters("Silver")                                     shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/11/plakat-Silver-200x288.jpg")
    posters("Wartość sentymentalna")                      shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/02/WartoscSentymentalna_plakat_NET-po-zmniejszeniu-200x288.jpg")
    posters("Wielki błękit")                              shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/01/WIELKI-BLEKIT_WEB-full-jpg-po-zmniejszeniu-200x288.jpg")
    posters("Wpatrując się w słońce")                     shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/04/WpatrujacSieWSlonce_B1_Net-1-200x288.jpg")
    posters("Znaki Pana Śliwki")                          shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2026/05/ZNAKI-PANA-SLIWKI-POSTER-jpg-po-zmniejszeniu-200x288.jpg")
    posters("Ścieżki życia")                              shouldBe Some("https://www.kinomalta.pl/wp-content/uploads/2025/11/Sciezki_zycia_plakat_PL_HQ-po-zmniejszeniu-200x288.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Bałtyk")                                     shouldBe Some("https://kinomalta.pl/movies/baltyk")
    filmUrls("Bez wyjścia")                                shouldBe Some("https://kinomalta.pl/movies/bez-wyjscia")
    filmUrls("Chronologia wody")                           shouldBe Some("https://kinomalta.pl/movies/chronologia-wody")
    filmUrls("Ciemna strona Mount Everest")                shouldBe Some("https://kinomalta.pl/movies/ciemna-strona-mount-everest")
    filmUrls("Człowiek z marmuru")                         shouldBe Some("https://kinomalta.pl/movies/czlowiek-z-marmuru")
    filmUrls("Drama")                                      shouldBe Some("https://kinomalta.pl/movies/drama")
    filmUrls("Dźwięki miłości")                            shouldBe Some("https://kinomalta.pl/movies/dzwieki-milosci")
    filmUrls("Father Mother Sister Brother")               shouldBe Some("https://kinomalta.pl/movies/father-mother-sister-brother")
    filmUrls("Flow")                                       shouldBe Some("https://kinomalta.pl/movies/flow")
    filmUrls("Glorious Summer")                            shouldBe Some("https://kinomalta.pl/movies/glorious-summer")
    filmUrls("Głos Hind Rajab")                            shouldBe Some("https://kinomalta.pl/movies/glos-hind-rajab")
    filmUrls("Harry Angel")                                shouldBe Some("https://kinomalta.pl/movies/harry-angel")
    filmUrls("Kicia Kocia ma braciszka")                   shouldBe Some("https://kinomalta.pl/movies/kicia-kocia-ma-braciszka")
    filmUrls("Kopnęłabym cię, gdybym mogła")               shouldBe Some("https://kinomalta.pl/movies/kopnelabym-cie-gdybym-mogla")
    filmUrls("Kurozając i świątynia Świstaka")             shouldBe Some("https://kinomalta.pl/movies/kurozajac-i-swiatynia-swistaka")
    filmUrls("La Grazia")                                  shouldBe Some("https://kinomalta.pl/movies/la-grazia")
    filmUrls("Layla")                                      shouldBe Some("https://kinomalta.pl/movies/layla")
    filmUrls("Left-Handed Girl. To była ręka... diabła!")  shouldBe Some("https://kinomalta.pl/movies/left-handed-girl-to-byla-reka-diabla")
    filmUrls("Maryja. Matka Papieża")                      shouldBe Some("https://kinomalta.pl/movies/maryja-matka-papieza")
    filmUrls("Matador")                                    shouldBe Some("https://kinomalta.pl/movies/matador")
    filmUrls("Miłość w czasach apokalipsy")                shouldBe Some("https://kinomalta.pl/movies/milosc-w-czasach-apokalipsy")
    filmUrls("Nie ma duchów w mieszkaniu na Dobrej")       shouldBe Some("https://kinomalta.pl/movies/nie-ma-duchow-w-mieszkaniu-na-dobrej")
    filmUrls("Niewinni czarodzieje")                       shouldBe Some("https://kinomalta.pl/movies/niewinni-czarodzieje")
    filmUrls("Opętanie")                                   shouldBe Some("https://kinomalta.pl/movies/opetanie")
    filmUrls("Ostatnia sesja w Paryżu")                    shouldBe Some("https://kinomalta.pl/movies/ostatnia-sesja-w-paryzu")
    filmUrls("Pan Nikt kontra Putin")                      shouldBe Some("https://kinomalta.pl/movies/pan-nikt-kontra-putin")
    filmUrls("Pod skórą")                                  shouldBe Some("https://kinomalta.pl/movies/pod-skora")
    filmUrls("Podziemny krąg")                             shouldBe Some("https://kinomalta.pl/movies/podziemny-krag")
    filmUrls("Pucio")                                      shouldBe Some("https://kinomalta.pl/movies/pucio")
    filmUrls("Rocznica")                                   shouldBe Some("https://kinomalta.pl/movies/rocznica")
    filmUrls("Romería")                                    shouldBe Some("https://kinomalta.pl/movies/romeria")
    filmUrls("Silver")                                     shouldBe Some("https://kinomalta.pl/movies/silver")
    filmUrls("Wartość sentymentalna")                      shouldBe Some("https://kinomalta.pl/movies/wartosc-sentymentalna")
    filmUrls("Wielki błękit")                              shouldBe Some("https://kinomalta.pl/movies/wielki-blekit")
    filmUrls("Wpatrując się w słońce")                     shouldBe Some("https://kinomalta.pl/movies/wpatrujac-sie-w-slonce")
    filmUrls("Znaki Pana Śliwki")                          shouldBe Some("https://kinomalta.pl/movies/znaki-pana-sliwki")
    filmUrls("Ścieżki życia")                              shouldBe Some("https://kinomalta.pl/movies/sciezki-zycia")
  }

  // ── Synopses ──────────────────────────────────────────────────────────────

  it should "extract a non-empty synopsis for every movie" in {
    results.foreach { cm =>
      withClue(s"${cm.movie.title}: ") {
        cm.synopsis should not be empty
        cm.synopsis.get.length should be > 30
      }
    }
  }

  it should "extract the exact synopsis for Bałtyk" in {
    byTitle("Bałtyk").synopsis.getOrElse("") should startWith(
      "W samym sercu Łeby, wśród wiatru, soli i zapachu dymu, Miecia od czterech dekad prowadzi kultową wędzarnię ryb."
    )
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Bałtyk")                                     shouldBe 1
    counts("Bez wyjścia")                                shouldBe 4
    counts("Chronologia wody")                           shouldBe 1
    counts("Ciemna strona Mount Everest")                shouldBe 1
    counts("Człowiek z marmuru")                         shouldBe 1
    counts("Drama")                                      shouldBe 5
    counts("Dźwięki miłości")                            shouldBe 1
    counts("Father Mother Sister Brother")               shouldBe 1
    counts("Flow")                                       shouldBe 1
    counts("Glorious Summer")                            shouldBe 1
    counts("Głos Hind Rajab")                            shouldBe 1
    counts("Harry Angel")                                shouldBe 1
    counts("Kicia Kocia ma braciszka")                   shouldBe 1
    counts("Kopnęłabym cię, gdybym mogła")               shouldBe 2
    counts("Kurozając i świątynia Świstaka")             shouldBe 3
    counts("La Grazia")                                  shouldBe 4
    counts("Layla")                                      shouldBe 2
    counts("Left-Handed Girl. To była ręka... diabła!")  shouldBe 1
    counts("Maryja. Matka Papieża")                      shouldBe 2
    counts("Matador")                                    shouldBe 3
    counts("Miłość w czasach apokalipsy")                shouldBe 3
    counts("Nie ma duchów w mieszkaniu na Dobrej")       shouldBe 3
    counts("Niewinni czarodzieje")                       shouldBe 1
    counts("Opętanie")                                   shouldBe 3
    counts("Ostatnia sesja w Paryżu")                    shouldBe 5
    counts("Pan Nikt kontra Putin")                      shouldBe 3
    counts("Pod skórą")                                  shouldBe 1
    counts("Podziemny krąg")                             shouldBe 3
    counts("Pucio")                                      shouldBe 1
    counts("Rocznica")                                   shouldBe 1
    counts("Romería")                                    shouldBe 3
    counts("Silver")                                     shouldBe 1
    counts("Wartość sentymentalna")                      shouldBe 4
    counts("Wielki błękit")                              shouldBe 1
    counts("Wpatrując się w słońce")                     shouldBe 1
    counts("Znaki Pana Śliwki")                          shouldBe 3
    counts("Ścieżki życia")                              shouldBe 2
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for Drama" in {
    val st = byTitle("Drama").showtimes
    st.size shouldBe 5
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 20, 30), Some("https://bilety.kinomalta.pl/system_portal.php/repertoire.html?id=41937"), Some("Sala Marilyn"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 14, 20, 30), Some("https://bilety.kinomalta.pl/system_portal.php/repertoire.html?id=41944"), Some("Sala Charlie"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 15, 20, 20), None, Some("Sala Marilyn"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 16, 20, 25), None, Some("Sala Marilyn"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 17, 20, 20), None, Some("Sala Marilyn"), Nil),
    )
  }

  it should "return exact showtimes for Ostatnia sesja w Paryżu" in {
    val st = byTitle("Ostatnia sesja w Paryżu").showtimes
    st.size shouldBe 5
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 18, 45), Some("https://bilety.kinomalta.pl/system_portal.php/repertoire.html?id=41940"), Some("Sala Audrey"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 14, 18, 45), Some("https://bilety.kinomalta.pl/system_portal.php/repertoire.html?id=41943"), Some("Sala Charlie"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 15, 18, 45), None, Some("Sala Charlie"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 16, 18, 45), None, Some("Sala Audrey"), Nil),
      Showtime(LocalDateTime.of(2026, 5, 17, 18, 45), None, Some("Sala Audrey"), Nil),
    )
  }
}
