package clients.multikino

import clients.MultikinoClient
import clients.tools.FakeHttpFetch
import models.{Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class MultikinoClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new MultikinoClient(new FakeHttpFetch("multikino"))
  private val results = client.fetch()
  // "Drugie życie" appears twice; toMap keeps the last entry for duplicate keys
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "MultikinoClient.fetch" should "return exactly 75 movies from fixture" in {
    results.size shouldBe 75
  }

  it should "return 667 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 667
  }

  it should "assign Multikino cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(Multikino)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "90. urodziny Pavarottiego",
      "Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza",
      "Art Beats: Muzeum Prado - kolekcja cudów",
      "Art Beats: Nenufary Moneta - cuda z wody i światła",
      "Art Beats: Rafael. Młody geniusz",
      "Billie Eilish - Hit Me Hard and Soft: The Tour",
      "Bluey w kinie: Kolekcja Zabawy z przyjaciółmi",
      "Caravaggio. Arcydzieła niepokornego geniusza",
      "Cirque du Soleil: Kooza",
      "Cirque du Soleil: Kurios - Gabinet osobliwości",
      "Diabeł ubiera się u Prady 2",
      "Drama",
      "Drugie życie",
      "Drzewo magii",
      "Erupcja",
      "FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ",
      "FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA",
      "Fantastyczne zwierzęta: Tajemnice Dumbledorea",
      "Harry Potter i Czara ognia",
      "Harry Potter i Insygnia Śmierci cz. 1",
      "Harry Potter i Insygnia Śmierci cz. 2",
      "Harry Potter i Kamień filozoficzny",
      "Harry Potter i Komnata Tajemnic",
      "Harry Potter i Książę Półkrwi",
      "Harry Potter i Więzień Azkabanu",
      "Harry Potter i Zakon Feniksa",
      "Hopnięci",
      "Iron Maiden: Burning Ambition",
      "John Williams - A Tribute",
      "Klątwa doliny węży -  z autorską narracją Łony",
      "Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas",
      "Kosmiczny mecz",
      "Kurozając i Świątynia Świstaka",
      "LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC",
      "La Traviata Verdiego z Arena di Verona",
      "Mandalorian i Grogu",
      "Maraton: Powrót do przyszłości",
      "Merrily We Roll Along",
      "Michael",
      "Milczenie owiec",
      "Milcząca przyjaciółka",
      "Mortal Kombat 2",
      "Moulin Rouge! – wersja oryginalna",
      "Mumia: Film Lee Cronina",
      "NT Live: Audiencja",
      "NT Live: Niebezpieczne związki",
      "NT Live: Playboy zachodniego świata",
      "NT Live: Wszyscy moi synowie",
      "Niesamowite przygody skarpetek 3. Ale kosmos!",
      "Obsesja",
      "Odlot",
      "Odrodzony jako galareta. Film: Łzy Morza Lazurowego",
      "Piep*zyć Mickiewicza 3",
      "Podziemny krąg",
      "Powrót do przyszłości",
      "Powrót do przyszłości II",
      "Powrót do przyszłości III",
      "Projekt Hail Mary",
      "Pucio",
      "Rambo: Pierwsza krew",
      "Romeo i Julia – wersja oryginalna",
      "Sprawiedliwość owiec",
      "Straszny film",
      "Super Mario Galaxy Film",
      "The Amazing Digital Circus: Ostatni Akt",
      "Top Gun",
      "Top Gun: Maverick",
      "Werdykt",
      "Wolność po włosku",
      "Za duży na bajki 3",
      "Zaplątani",
      "Zaproszenie",
      "Zawieście czerwone latarnie",
      "Żywot Briana Grupy Monty Pythona. Wersja zremasterowana",
    )
  }

  it should "have exactly 2 entries titled Drugie życie" in {
    results.count(_.movie.title == "Drugie życie") shouldBe 2
  }

  it should "have different film URLs for the two Drugie życie entries" in {
    results.filter(_.movie.title == "Drugie życie").flatMap(_.filmUrl).toSet shouldBe Set(
      "https://www.multikino.pl/filmy/kino-na-obcasach-drugie-zycie",
      "https://www.multikino.pl/filmy/drugie-zycie",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("90. urodziny Pavarottiego")                                          shouldBe Some(95)
    runtimes("Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza") shouldBe Some(80)
    runtimes("Art Beats: Muzeum Prado - kolekcja cudów")                           shouldBe Some(95)
    runtimes("Art Beats: Nenufary Moneta - cuda z wody i światła")                 shouldBe Some(95)
    runtimes("Art Beats: Rafael. Młody geniusz")                                   shouldBe Some(80)
    runtimes("Billie Eilish - Hit Me Hard and Soft: The Tour")                     shouldBe Some(105)
    runtimes("Bluey w kinie: Kolekcja Zabawy z przyjaciółmi")                      shouldBe Some(55)
    runtimes("Caravaggio. Arcydzieła niepokornego geniusza")                       shouldBe Some(100)
    runtimes("Cirque du Soleil: Kooza")                                            shouldBe Some(90)
    runtimes("Cirque du Soleil: Kurios - Gabinet osobliwości")                     shouldBe Some(89)
    runtimes("Diabeł ubiera się u Prady 2")                                        shouldBe Some(120)
    runtimes("Drama")                                                              shouldBe Some(106)
    runtimes("Drugie życie")                                                       shouldBe Some(0)
    runtimes("Drzewo magii")                                                       shouldBe Some(110)
    runtimes("Erupcja")                                                            shouldBe Some(71)
    runtimes("FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ")                            shouldBe Some(0)
    runtimes("FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA")                      shouldBe Some(0)
    runtimes("Fantastyczne zwierzęta: Tajemnice Dumbledorea")                      shouldBe Some(142)
    runtimes("Harry Potter i Czara ognia")                                         shouldBe Some(0)
    runtimes("Harry Potter i Insygnia Śmierci cz. 1")                              shouldBe Some(0)
    runtimes("Harry Potter i Insygnia Śmierci cz. 2")                              shouldBe Some(0)
    runtimes("Harry Potter i Kamień filozoficzny")                                 shouldBe Some(0)
    runtimes("Harry Potter i Komnata Tajemnic")                                    shouldBe Some(0)
    runtimes("Harry Potter i Książę Półkrwi")                                      shouldBe Some(0)
    runtimes("Harry Potter i Więzień Azkabanu")                                    shouldBe Some(0)
    runtimes("Harry Potter i Zakon Feniksa")                                       shouldBe Some(0)
    runtimes("Hopnięci")                                                           shouldBe Some(0)
    runtimes("Iron Maiden: Burning Ambition")                                      shouldBe Some(106)
    runtimes("John Williams - A Tribute")                                          shouldBe Some(83)
    runtimes("Klątwa doliny węży -  z autorską narracją Łony")                     shouldBe Some(0)
    runtimes("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")         shouldBe Some(98)
    runtimes("Kosmiczny mecz")                                                     shouldBe Some(0)
    runtimes("Kurozając i Świątynia Świstaka")                                     shouldBe Some(89)
    runtimes("LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC")  shouldBe Some(0)
    runtimes("La Traviata Verdiego z Arena di Verona")                             shouldBe Some(160)
    runtimes("Mandalorian i Grogu")                                                shouldBe Some(132)
    runtimes("Maraton: Powrót do przyszłości")                                     shouldBe Some(0)
    runtimes("Merrily We Roll Along")                                              shouldBe Some(145)
    runtimes("Michael")                                                            shouldBe Some(127)
    runtimes("Milczenie owiec")                                                    shouldBe Some(0)
    runtimes("Milcząca przyjaciółka")                                              shouldBe Some(147)
    runtimes("Mortal Kombat 2")                                                    shouldBe Some(116)
    runtimes("Moulin Rouge! – wersja oryginalna")                                  shouldBe Some(0)
    runtimes("Mumia: Film Lee Cronina")                                            shouldBe Some(0)
    runtimes("NT Live: Audiencja")                                                 shouldBe Some(180)
    runtimes("NT Live: Niebezpieczne związki")                                     shouldBe Some(150)
    runtimes("NT Live: Playboy zachodniego świata")                                shouldBe Some(150)
    runtimes("NT Live: Wszyscy moi synowie")                                       shouldBe Some(130)
    runtimes("Niesamowite przygody skarpetek 3. Ale kosmos!")                      shouldBe Some(55)
    runtimes("Obsesja")                                                            shouldBe Some(109)
    runtimes("Odlot")                                                              shouldBe Some(0)
    runtimes("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")                shouldBe Some(104)
    runtimes("Piep*zyć Mickiewicza 3")                                             shouldBe Some(0)
    runtimes("Podziemny krąg")                                                     shouldBe Some(0)
    runtimes("Powrót do przyszłości")                                              shouldBe Some(0)
    runtimes("Powrót do przyszłości II")                                           shouldBe Some(0)
    runtimes("Powrót do przyszłości III")                                          shouldBe Some(0)
    runtimes("Projekt Hail Mary")                                                  shouldBe Some(157)
    runtimes("Pucio")                                                              shouldBe Some(45)
    runtimes("Rambo: Pierwsza krew")                                               shouldBe Some(0)
    runtimes("Romeo i Julia – wersja oryginalna")                                  shouldBe Some(0)
    runtimes("Sprawiedliwość owiec")                                               shouldBe Some(109)
    runtimes("Straszny film")                                                      shouldBe Some(0)
    runtimes("Super Mario Galaxy Film")                                            shouldBe Some(100)
    runtimes("The Amazing Digital Circus: Ostatni Akt")                            shouldBe Some(93)
    runtimes("Top Gun")                                                            shouldBe Some(0)
    runtimes("Top Gun: Maverick")                                                  shouldBe Some(130)
    runtimes("Werdykt")                                                            shouldBe Some(0)
    runtimes("Wolność po włosku")                                                  shouldBe Some(117)
    runtimes("Za duży na bajki 3")                                                 shouldBe Some(0)
    runtimes("Zaplątani")                                                          shouldBe Some(0)
    runtimes("Zaproszenie")                                                        shouldBe Some(0)
    runtimes("Zawieście czerwone latarnie")                                        shouldBe Some(0)
    runtimes("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana")            shouldBe Some(94)
  }

  // ── Release years ─────────────────────────────────────────────────────────

  it should "return correct release year for every movie" in {
    byTitle("90. urodziny Pavarottiego").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza").movie.releaseYear shouldBe Some(2026)
    byTitle("Art Beats: Muzeum Prado - kolekcja cudów").movie.releaseYear                           shouldBe Some(2026)
    byTitle("Art Beats: Nenufary Moneta - cuda z wody i światła").movie.releaseYear                 shouldBe Some(2026)
    byTitle("Art Beats: Rafael. Młody geniusz").movie.releaseYear                                   shouldBe Some(2026)
    byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour").movie.releaseYear                     shouldBe Some(2026)
    byTitle("Bluey w kinie: Kolekcja Zabawy z przyjaciółmi").movie.releaseYear                      shouldBe Some(2026)
    byTitle("Caravaggio. Arcydzieła niepokornego geniusza").movie.releaseYear                       shouldBe Some(2026)
    byTitle("Cirque du Soleil: Kooza").movie.releaseYear                                            shouldBe Some(2026)
    byTitle("Cirque du Soleil: Kurios - Gabinet osobliwości").movie.releaseYear                     shouldBe Some(2026)
    byTitle("Diabeł ubiera się u Prady 2").movie.releaseYear                                        shouldBe Some(2026)
    byTitle("Drama").movie.releaseYear                                                              shouldBe Some(2026)
    byTitle("Drugie życie").movie.releaseYear                                                       shouldBe Some(2026)
    byTitle("Drzewo magii").movie.releaseYear                                                       shouldBe Some(2026)
    byTitle("Erupcja").movie.releaseYear                                                            shouldBe Some(2026)
    byTitle("FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ").movie.releaseYear                            shouldBe Some(2026)
    byTitle("FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA").movie.releaseYear                      shouldBe Some(2026)
    byTitle("Fantastyczne zwierzęta: Tajemnice Dumbledorea").movie.releaseYear                      shouldBe Some(2026)
    byTitle("Harry Potter i Czara ognia").movie.releaseYear                                         shouldBe Some(2024)
    byTitle("Harry Potter i Insygnia Śmierci cz. 1").movie.releaseYear                              shouldBe Some(2024)
    byTitle("Harry Potter i Insygnia Śmierci cz. 2").movie.releaseYear                              shouldBe Some(2024)
    byTitle("Harry Potter i Kamień filozoficzny").movie.releaseYear                                 shouldBe Some(2024)
    byTitle("Harry Potter i Komnata Tajemnic").movie.releaseYear                                    shouldBe Some(2024)
    byTitle("Harry Potter i Książę Półkrwi").movie.releaseYear                                      shouldBe Some(2024)
    byTitle("Harry Potter i Więzień Azkabanu").movie.releaseYear                                    shouldBe Some(2024)
    byTitle("Harry Potter i Zakon Feniksa").movie.releaseYear                                       shouldBe Some(2024)
    byTitle("Hopnięci").movie.releaseYear                                                           shouldBe Some(2026)
    byTitle("Iron Maiden: Burning Ambition").movie.releaseYear                                      shouldBe Some(2026)
    byTitle("John Williams - A Tribute").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Klątwa doliny węży -  z autorską narracją Łony").movie.releaseYear                     shouldBe Some(2026)
    byTitle("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas").movie.releaseYear         shouldBe Some(2026)
    byTitle("Kosmiczny mecz").movie.releaseYear                                                     shouldBe Some(2026)
    byTitle("Kurozając i Świątynia Świstaka").movie.releaseYear                                     shouldBe Some(2026)
    byTitle("LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC").movie.releaseYear  shouldBe Some(2026)
    byTitle("La Traviata Verdiego z Arena di Verona").movie.releaseYear                             shouldBe Some(2026)
    byTitle("Mandalorian i Grogu").movie.releaseYear                                                shouldBe Some(2026)
    byTitle("Maraton: Powrót do przyszłości").movie.releaseYear                                     shouldBe Some(2026)
    byTitle("Merrily We Roll Along").movie.releaseYear                                              shouldBe Some(2026)
    byTitle("Michael").movie.releaseYear                                                            shouldBe Some(2026)
    byTitle("Milczenie owiec").movie.releaseYear                                                    shouldBe Some(2026)
    byTitle("Milcząca przyjaciółka").movie.releaseYear                                              shouldBe Some(2026)
    byTitle("Mortal Kombat 2").movie.releaseYear                                                    shouldBe Some(2026)
    byTitle("Moulin Rouge! – wersja oryginalna").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Mumia: Film Lee Cronina").movie.releaseYear                                            shouldBe Some(2026)
    byTitle("NT Live: Audiencja").movie.releaseYear                                                 shouldBe Some(2026)
    byTitle("NT Live: Niebezpieczne związki").movie.releaseYear                                     shouldBe Some(2026)
    byTitle("NT Live: Playboy zachodniego świata").movie.releaseYear                                shouldBe Some(2026)
    byTitle("NT Live: Wszyscy moi synowie").movie.releaseYear                                       shouldBe Some(2026)
    byTitle("Niesamowite przygody skarpetek 3. Ale kosmos!").movie.releaseYear                      shouldBe Some(2026)
    byTitle("Obsesja").movie.releaseYear                                                            shouldBe Some(2026)
    byTitle("Odlot").movie.releaseYear                                                              shouldBe Some(2026)
    byTitle("Odrodzony jako galareta. Film: Łzy Morza Lazurowego").movie.releaseYear                shouldBe Some(2026)
    byTitle("Piep*zyć Mickiewicza 3").movie.releaseYear                                             shouldBe Some(2026)
    byTitle("Podziemny krąg").movie.releaseYear                                                     shouldBe Some(2026)
    byTitle("Powrót do przyszłości").movie.releaseYear                                              shouldBe Some(2026)
    byTitle("Powrót do przyszłości II").movie.releaseYear                                           shouldBe Some(2026)
    byTitle("Powrót do przyszłości III").movie.releaseYear                                          shouldBe Some(2026)
    byTitle("Projekt Hail Mary").movie.releaseYear                                                  shouldBe Some(2026)
    byTitle("Pucio").movie.releaseYear                                                              shouldBe Some(2026)
    byTitle("Rambo: Pierwsza krew").movie.releaseYear                                               shouldBe Some(2026)
    byTitle("Romeo i Julia – wersja oryginalna").movie.releaseYear                                  shouldBe Some(2026)
    byTitle("Sprawiedliwość owiec").movie.releaseYear                                               shouldBe Some(2026)
    byTitle("Straszny film").movie.releaseYear                                                      shouldBe Some(2026)
    byTitle("Super Mario Galaxy Film").movie.releaseYear                                            shouldBe Some(2026)
    byTitle("The Amazing Digital Circus: Ostatni Akt").movie.releaseYear                            shouldBe Some(2026)
    byTitle("Top Gun").movie.releaseYear                                                            shouldBe Some(2026)
    byTitle("Top Gun: Maverick").movie.releaseYear                                                  shouldBe Some(2022)
    byTitle("Werdykt").movie.releaseYear                                                            shouldBe Some(2026)
    byTitle("Wolność po włosku").movie.releaseYear                                                  shouldBe Some(2026)
    byTitle("Za duży na bajki 3").movie.releaseYear                                                 shouldBe Some(2026)
    byTitle("Zaplątani").movie.releaseYear                                                          shouldBe Some(2026)
    byTitle("Zaproszenie").movie.releaseYear                                                        shouldBe Some(2026)
    byTitle("Zawieście czerwone latarnie").movie.releaseYear                                        shouldBe Some(2026)
    byTitle("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana").movie.releaseYear            shouldBe Some(2026)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("90. urodziny Pavarottiego")                                          shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/koncerty/pavarotti/pavarotti90_plakat.jpg?rev=10b0b0776a1740c3bc284f5e077c5434")
    posters("Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza") shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/wystawy/lotto-plakat.jpg?rev=38a9f6140a1a4ffcab6674adfc953e0e")
    posters("Art Beats: Muzeum Prado - kolekcja cudów")                           shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/wystawy/prado-plakat.jpg?rev=a833f56d8ce4498ba2d4722bd2c43df0")
    posters("Art Beats: Nenufary Moneta - cuda z wody i światła")                 shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/wystawy/monet-nenufary-plakat.jpg?rev=0ec23b8a082f4b7bb70c073696776d8a")
    posters("Art Beats: Rafael. Młody geniusz")                                   shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/wystawy/rafael-plakat.jpg?rev=e127f4cbbb104fde8a20d94cf4ba7dc0")
    posters("Billie Eilish - Hit Me Hard and Soft: The Tour")                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/billie-eilish/billie-eilish_plakat_cut.jpg?rev=25c4d4367da24664815b62a9c3b17741")
    posters("Bluey w kinie: Kolekcja Zabawy z przyjaciółmi")                      shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/bluey/bluey_zabawazprzyjaciolmi_plakat2.jpg?rev=4a0380d5d9b54c4ba1959fbd958a05e6")
    posters("Caravaggio. Arcydzieła niepokornego geniusza")                       shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/wystawy/caravaggio_plakat.jpg?rev=743d8bdf88b64f9ea981dbbf1ef55121")
    posters("Cirque du Soleil: Kooza")                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/img/import/4e7d682d-c594-4525-a0d9-6a0b2fd97021_cirque-du-soleil-kooza_posters_koo_full_2000x3000px_712px.png?rev=ecb26b3f261f45559f7c9bd2c71b1b22")
    posters("Cirque du Soleil: Kurios - Gabinet osobliwości")                     shouldBe Some("https://www.multikino.pl/-/media/multikino/img/import/bba92643-7ded-4bb4-aa17-d9d7ec7a4df9_cirque-du-soleil-kurios---gabinet-osobliwosci_posters_kur_full_.png?rev=bfbf4ae611944a2a92962535709bcfe0")
    posters("Diabeł ubiera się u Prady 2")                                        shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/diabel-ubiera-sie-u-prady-2/diabelprada2_plakatoficial-cut.jpg?rev=6e854aefaa8e47c0b27446b60a57f68a")
    posters("Drama")                                                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/drama/drama-plakat-final-cut.jpg?rev=99e50bcc8ce449fab18d52c1caef4c08")
    posters("Drugie życie")                                                       shouldBe Some("https://www.multikino.pl/-/media/multikino/images/offowe-czwartki/drugiezycie_plakat.jpg?rev=b3e62875d7b248b78e1a7d07fdf010d2")
    posters("Drzewo magii")                                                       shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/drzewo-magii/drzewo-magii_plakat-glowny_plakat.jpg?rev=78ae4df57ddb40d98d5535803ff32357")
    posters("Erupcja")                                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/offowe-czwartki/erupcja.jpg?rev=66cbdeeffa9b41eca82059aab89bb588")
    posters("FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ")                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/fantastyczne-zwierzeta/fantastyczne_cz1_plakat.jpg?rev=1a4e976bba414fff9058bac59c8a86ae")
    posters("FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA")                      shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/fantastyczne-zwierzeta/fantastyczne_cz2_plakat.jpg?rev=187efed51ce645569fb425ca6a669679")
    posters("Fantastyczne zwierzęta: Tajemnice Dumbledorea")                      shouldBe Some("https://www.multikino.pl/-/media/multikino/multikino_imported/imported_from_external_source/posterimage/pl-fnbst3-plakat-oficjalny-cut_f5cdf984c2.jpg?rev=0a9c0072657b4727ab2bce0e955867d2")
    posters("Harry Potter i Czara ognia")                                         shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/hp-i-czara-ognia-plakat.png?rev=af3507e8ea434b0dad8ffe759ae3c699")
    posters("Harry Potter i Insygnia Śmierci cz. 1")                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/hp-i-insygnia-1-plakat.png?rev=4bb3985b391c48bb8865e6ad3c792e81")
    posters("Harry Potter i Insygnia Śmierci cz. 2")                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/hp-i-insygnia-2-plakat.png?rev=02dfe6e3a9fb4f0db308efee9f943de2")
    posters("Harry Potter i Kamień filozoficzny")                                 shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/harry-potter-i-kamien.jpg?rev=746ac80d53474907bcb22092c0f29d46")
    posters("Harry Potter i Komnata Tajemnic")                                    shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/harry-potter-i-komnata.jpg?rev=2a3cdf37f6a241efa4b8cdb19164d545")
    posters("Harry Potter i Książę Półkrwi")                                      shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/hp-i-ksiaze-plakat.png?rev=44968ea3411d4cf6895a2305451256de")
    posters("Harry Potter i Więzień Azkabanu")                                    shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/harry-potter-i-wiezien.jpg?rev=d9efdcb242c24a7a8f7eafe56cdb8878")
    posters("Harry Potter i Zakon Feniksa")                                       shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/harry-potter/hp-i-zakon-plakat.png?rev=cc1f0e4ef6e14bc684b712282bcabe91")
    posters("Hopnięci")                                                           shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/hopnieci/hopnieci-plakat-final-cut.jpg?rev=9d1707945c83489aad3da15956eb50fa")
    posters("Iron Maiden: Burning Ambition")                                      shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/koncerty/iron-maiden/ironmaiden.jpg?rev=a8cd80d50e264d7baef87d55f4b41156")
    posters("John Williams - A Tribute")                                          shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/koncerty/john-williams/jwilliams_plakat.jpg?rev=c7d1daa261e2416da7adc8881c9a2801")
    posters("Klątwa doliny węży -  z autorską narracją Łony")                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/klatwa-doliny-wezy_plakat.jpg?rev=c58ea807e26643fb8bc1425578f746b0")
    posters("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")         shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/anime/o-dziewczynie-skaczacej-przez-czas-plakat2.jpg?rev=125e66df557f4228bf3605103ca217f9")
    posters("Kosmiczny mecz")                                                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/kosmicznymecz96.jpg?rev=5cf7d42444434cff9f803083a634a9ee")
    posters("Kurozając i Świątynia Świstaka")                                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/kurozajac-i-swiatynia-swistaka/kurozajac_plakat.jpg?rev=8e07534364604957a7cfab295c1dc9ec")
    posters("LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC")  shouldBe Some("https://www.multikino.pl/-/media/multikino/images/euro/2026/plakat_30maja.jpg?rev=b087764f339d4085af181aa6fc494509")
    posters("La Traviata Verdiego z Arena di Verona")                             shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/opera/latraviata2026_plakat.png?rev=d55bc88afb2b4c949d93ba3113f446ce")
    posters("Mandalorian i Grogu")                                                shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/mandalorian-grogu/mandalorian-plakat-fin.jpg?rev=90e8fd31606f41a0b6393770f3fe36d1")
    posters("Maraton: Powrót do przyszłości")                                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/wkrotce_1_plakat.jpg?rev=4671625446e74c9aa1a2bfee37296ea3")
    posters("Merrily We Roll Along")                                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/teatr/merrily/mwra_plakat.jpg?rev=431c7486a5d4411fa41e65b502e0329d")
    posters("Michael")                                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/michael/michael-plakat-net-fin-cut.jpg?rev=50ffa68228ab41d0a9633db28bbd2b6e")
    posters("Milczenie owiec")                                                    shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/milczenie-owiec-cut.jpg?rev=e42068650341416bb3c39e499ad5bc5b")
    posters("Milcząca przyjaciółka")                                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/offowe-czwartki/milczacaprzyjaciolka_plakat.jpg?rev=766542d25ae0462f97c32308cb1b7970")
    posters("Mortal Kombat 2")                                                    shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2025/mortal-kombat-2/mortal-kombat-plakat-cut.jpg?rev=6157d952d8894de6993299b8ba475d50")
    posters("Moulin Rouge! – wersja oryginalna")                                  shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/moulinrouge_plakat.jpg?rev=8ca27e12f37248789e7785bd6c5bc786")
    posters("Mumia: Film Lee Cronina")                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/mumia/mumia_plakat_fin_cut.jpg?rev=c4702594b644437f91c4ba1c5c656aa2")
    posters("NT Live: Audiencja")                                                 shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/teatr/audiencja/audience_plakat.jpg?rev=aff6a9407f624513b3a44c7871e97f7d")
    posters("NT Live: Niebezpieczne związki")                                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/teatr/niebezpieczne-zwiazki/niebezpiecznezwiazki_plakat.jpg?rev=eaa00fa62980438295bc9c57b914a338")
    posters("NT Live: Playboy zachodniego świata")                                shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/teatr/playboy-zachodniego-swiata/playboy_plakat.jpg?rev=61b7db6ed64243cbb0fbb2d2ea71f81d")
    posters("NT Live: Wszyscy moi synowie")                                       shouldBe Some("https://www.multikino.pl/-/media/multikino/images/wydarzenia/teatr/wszyscy-moi-synowie/allmysons_plakat.jpg?rev=7ce83f21ecca4f9e9e3081fe2864919a")
    posters("Niesamowite przygody skarpetek 3. Ale kosmos!")                      shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/niesamowite-przygody-skarpetek-3/niesamowite-przygody-skarpetek-3-plakat-cut.png?rev=f5940b2f416e4b19a59c25edac82e020")
    posters("Obsesja")                                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/obsesja/obsesja-plakat-fin.jpg?rev=0e43004367554ace94bab8ac67bd3742")
    posters("Odlot")                                                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/wkrotce_1_plakat.jpg?rev=4671625446e74c9aa1a2bfee37296ea3")
    posters("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")                shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/odrodzony-jako-galareta/galareta_plakat.jpg?rev=e534e8a0a1784cf5bf68d2b88b8f2ec6")
    posters("Piep*zyć Mickiewicza 3")                                             shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/pieprzyc-mickiewicza-3/pieprzycmickiewicza3_plakat_cut.jpg?rev=47ea7017fe5a409ab3c0ec4cb8219a95")
    posters("Podziemny krąg")                                                     shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/podziemny-krag_online_cut.jpg?rev=8b4016e21dd74d46a7dc788b826c7a8e")
    posters("Powrót do przyszłości")                                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/powrot-do-przyszlosci/bttf-1-plakat-cut2.jpg?rev=604aded5095847c2a0149a31a4f91d11")
    posters("Powrót do przyszłości II")                                           shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/powrot-do-przyszlosci/bttf2-plakat-cut2.jpg?rev=be69cfb939234d949dfec4644c17885e")
    posters("Powrót do przyszłości III")                                          shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/powrot-do-przyszlosci/bttf3-plakat-cut2.jpg?rev=7e089f9f6c1843a998fac58f33e21687")
    posters("Projekt Hail Mary")                                                  shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/projekt-hail-mary/projekthailmary_plakat.jpg?rev=5dee71e5ec014e75a577f1e8055c4dbf")
    posters("Pucio")                                                              shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/pucio/pucio-plakat.jpg?rev=5c59e10082a64578b39f13a3bf9616bd")
    posters("Rambo: Pierwsza krew")                                               shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/rambo-plakat1.png?rev=0a88d5dfb3794a53aa316581298f542a")
    posters("Romeo i Julia – wersja oryginalna")                                  shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/romeojulia_plakat.jpg?rev=fa911f8bb60b4b95bd7cb1861a8adf77")
    posters("Sprawiedliwość owiec")                                               shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/sprawiedliwosc-owiec/sprawiedliwoscowiec_plakat.jpg?rev=be0f91ffd2f248caaed986a915b760f8")
    posters("Straszny film")                                                      shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/straszny-film/straszny-film-plakat-net.jpg?rev=90bf6b7775c948f49f68a67c868d264b")
    posters("Super Mario Galaxy Film")                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/super-mario-galaxy-film/super-mario-galaxy-film-plakat-final.jpg?rev=343089138a9844fb8f7d115642cf2862")
    posters("The Amazing Digital Circus: Ostatni Akt")                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/tadc/tadc_plakat1.jpg?rev=6804403f06ff476592b3fb32300bcdf8")
    posters("Top Gun")                                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/top-gun-rocznica40.jpg?rev=9d8790a579a34b90a653b01fa16444c9")
    posters("Top Gun: Maverick")                                                  shouldBe Some("https://www.multikino.pl/-/media/multikino/multikino_imported/imported_from_external_source/posterimage/top-gun-maverick-plakat-finalny-cut_f040b47882.jpg?rev=973ff8b6bd434f0eb0e2aad7eef2a685")
    posters("Werdykt")                                                            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/offowe-czwartki/werdykt_plakat_cut.jpg?rev=433b248d041a4fed8dfc6fbed59f146c")
    posters("Wolność po włosku")                                                  shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/wolnosc-po-wlosku/wolnosc-po-wlosku_plakat-cut.jpg?rev=4ea29ca18f8d4b8797416311488768ed")
    posters("Za duży na bajki 3")                                                 shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/za-duzy-na-bajki-3/zaduzynabajki3_plakat.jpg?rev=9b8f08e7580848568d5657d0881b4a65")
    posters("Zaplątani")                                                          shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/wkrotce_1_plakat.jpg?rev=4671625446e74c9aa1a2bfee37296ea3")
    posters("Zaproszenie")                                                        shouldBe Some("https://www.multikino.pl/-/media/multikino/images/film-and-events/wkrotce_1_plakat.jpg?rev=4671625446e74c9aa1a2bfee37296ea3")
    posters("Zawieście czerwone latarnie")                                        shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/zawiecie_czerwone_laternie_plakat.jpg?rev=300dbab82eef4904a6bb9c1888bbea70")
    posters("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana")            shouldBe Some("https://www.multikino.pl/-/media/multikino/images/kultowe-kino/ywot_briana_plakat.jpg?rev=9ec79149ee78480185000b354a311e71")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("90. urodziny Pavarottiego")                                          shouldBe Some("https://www.multikino.pl/filmy/90-urodziny-pavarottiego")
    filmUrls("Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza") shouldBe Some("https://www.multikino.pl/filmy/art-beats-lotto-i-berenson---splecione-losy")
    filmUrls("Art Beats: Muzeum Prado - kolekcja cudów")                           shouldBe Some("https://www.multikino.pl/filmy/art-beats-muzeum-prado---kolekcja-cudow")
    filmUrls("Art Beats: Nenufary Moneta - cuda z wody i światła")                 shouldBe Some("https://www.multikino.pl/filmy/art-beats-nenufary-moneta---cuda-z-wody-i-swiata")
    filmUrls("Art Beats: Rafael. Młody geniusz")                                   shouldBe Some("https://www.multikino.pl/filmy/art-beats-rafael-mody-geniusz")
    filmUrls("Billie Eilish - Hit Me Hard and Soft: The Tour")                     shouldBe Some("https://www.multikino.pl/filmy/billie-eilish---hit-me-hard-and-soft-the-tour")
    filmUrls("Bluey w kinie: Kolekcja Zabawy z przyjaciółmi")                      shouldBe Some("https://www.multikino.pl/filmy/bluey-w-kinie-kolekcja-zabawy-z-przyjaciomi")
    filmUrls("Caravaggio. Arcydzieła niepokornego geniusza")                       shouldBe Some("https://www.multikino.pl/filmy/caravaggio-arcydziela-niepokornego-geniusza")
    filmUrls("Cirque du Soleil: Kooza")                                            shouldBe Some("https://www.multikino.pl/filmy/cirque-du-soleil-kooza")
    filmUrls("Cirque du Soleil: Kurios - Gabinet osobliwości")                     shouldBe Some("https://www.multikino.pl/filmy/cirque-du-soleil-kurios---gabinet-osobliwosci")
    filmUrls("Diabeł ubiera się u Prady 2")                                        shouldBe Some("https://www.multikino.pl/filmy/diabel-ubiera-sie-u-prady-2")
    filmUrls("Drama")                                                              shouldBe Some("https://www.multikino.pl/filmy/drama")
    filmUrls("Drugie życie")                                                       shouldBe Some("https://www.multikino.pl/filmy/drugie-zycie")
    filmUrls("Drzewo magii")                                                       shouldBe Some("https://www.multikino.pl/filmy/drzewo-magii")
    filmUrls("Erupcja")                                                            shouldBe Some("https://www.multikino.pl/filmy/erupcja")
    filmUrls("FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ")                            shouldBe Some("https://www.multikino.pl/filmy/fantastyczne-zwierzeta-i-jak-je-znalezc")
    filmUrls("FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA")                      shouldBe Some("https://www.multikino.pl/filmy/fantastyczne-zwierzeta-zbrodnie-grindelwalda")
    filmUrls("Fantastyczne zwierzęta: Tajemnice Dumbledorea")                      shouldBe Some("https://www.multikino.pl/filmy/fantastyczne-zwierzeta-tajemnice-dumbledore-a")
    filmUrls("Harry Potter i Czara ognia")                                         shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-czara-ognia")
    filmUrls("Harry Potter i Insygnia Śmierci cz. 1")                              shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-insygnia-smierci-cz-1")
    filmUrls("Harry Potter i Insygnia Śmierci cz. 2")                              shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-insygnia-smierci-cz-2")
    filmUrls("Harry Potter i Kamień filozoficzny")                                 shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-kamien-filozoficzny")
    filmUrls("Harry Potter i Komnata Tajemnic")                                    shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-komnata-tajemnic")
    filmUrls("Harry Potter i Książę Półkrwi")                                      shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-ksiaze-polkrwi")
    filmUrls("Harry Potter i Więzień Azkabanu")                                    shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-wiezien-azkabanu")
    filmUrls("Harry Potter i Zakon Feniksa")                                       shouldBe Some("https://www.multikino.pl/filmy/harry-potter-i-zakon-feniksa")
    filmUrls("Hopnięci")                                                           shouldBe Some("https://www.multikino.pl/filmy/hopnieci")
    filmUrls("Iron Maiden: Burning Ambition")                                      shouldBe Some("https://www.multikino.pl/filmy/iron-maiden-burning-ambition")
    filmUrls("John Williams - A Tribute")                                          shouldBe Some("https://www.multikino.pl/filmy/john-williams-a-tribute")
    filmUrls("Klątwa doliny węży -  z autorską narracją Łony")                     shouldBe Some("https://www.multikino.pl/filmy/klatwa-doliny-wezy----z-autorska-narracja-ony")
    filmUrls("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")         shouldBe Some("https://www.multikino.pl/filmy/kolekcja-mamoru-hosody-o-dziewczynie-skaczacej-przez-czas")
    filmUrls("Kosmiczny mecz")                                                     shouldBe Some("https://www.multikino.pl/filmy/kosmiczny-mecz")
    filmUrls("Kurozając i Świątynia Świstaka")                                     shouldBe Some("https://www.multikino.pl/filmy/kurozajac-i-swiatynia-swistaka")
    filmUrls("LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC")  shouldBe Some("https://www.multikino.pl/filmy/liga-mistrzow-uefa-final-2026-30052026")
    filmUrls("La Traviata Verdiego z Arena di Verona")                             shouldBe Some("https://www.multikino.pl/filmy/la-traviata-verdiego-z-arena-di-verona")
    filmUrls("Mandalorian i Grogu")                                                shouldBe Some("https://www.multikino.pl/filmy/mandalorian-grogu")
    filmUrls("Maraton: Powrót do przyszłości")                                     shouldBe Some("https://www.multikino.pl/filmy/maraton-powrot-do-przyszosci")
    filmUrls("Merrily We Roll Along")                                              shouldBe Some("https://www.multikino.pl/filmy/merrily-we-roll-along")
    filmUrls("Michael")                                                            shouldBe Some("https://www.multikino.pl/filmy/michael")
    filmUrls("Milczenie owiec")                                                    shouldBe Some("https://www.multikino.pl/filmy/milczenie-owiec")
    filmUrls("Milcząca przyjaciółka")                                              shouldBe Some("https://www.multikino.pl/filmy/milczaca-przyjaciolka")
    filmUrls("Mortal Kombat 2")                                                    shouldBe Some("https://www.multikino.pl/filmy/mortal-kombat-2")
    filmUrls("Moulin Rouge! – wersja oryginalna")                                  shouldBe Some("https://www.multikino.pl/filmy/moulin-rouge-wersja-oryginalna")
    filmUrls("Mumia: Film Lee Cronina")                                            shouldBe Some("https://www.multikino.pl/filmy/mumia-film-lee-cronina")
    filmUrls("NT Live: Audiencja")                                                 shouldBe Some("https://www.multikino.pl/filmy/nt-live-audiencja")
    filmUrls("NT Live: Niebezpieczne związki")                                     shouldBe Some("https://www.multikino.pl/filmy/nt-live-niebezpieczne-zwiazki")
    filmUrls("NT Live: Playboy zachodniego świata")                                shouldBe Some("https://www.multikino.pl/filmy/nt-live-playboy-zachodniego-swiata")
    filmUrls("NT Live: Wszyscy moi synowie")                                       shouldBe Some("https://www.multikino.pl/filmy/nt-live-wszyscy-moi-synowie")
    filmUrls("Niesamowite przygody skarpetek 3. Ale kosmos!")                      shouldBe Some("https://www.multikino.pl/filmy/niesamowite-przygody-skarpetek-3")
    filmUrls("Obsesja")                                                            shouldBe Some("https://www.multikino.pl/filmy/obsesja")
    filmUrls("Odlot")                                                              shouldBe Some("https://www.multikino.pl/filmy/odlot")
    filmUrls("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")                shouldBe Some("https://www.multikino.pl/filmy/odrodzony-jako-galareta-film-lzy-morza--lazurowego")
    filmUrls("Piep*zyć Mickiewicza 3")                                             shouldBe Some("https://www.multikino.pl/filmy/piepzyc-mickiewicza-3")
    filmUrls("Podziemny krąg")                                                     shouldBe Some("https://www.multikino.pl/filmy/podziemny-krag")
    filmUrls("Powrót do przyszłości")                                              shouldBe Some("https://www.multikino.pl/filmy/powrot-do-przyszlosci")
    filmUrls("Powrót do przyszłości II")                                           shouldBe Some("https://www.multikino.pl/filmy/powrot-do-przyszlosci-ii")
    filmUrls("Powrót do przyszłości III")                                          shouldBe Some("https://www.multikino.pl/filmy/powrot-do-przyszlosci-iii")
    filmUrls("Projekt Hail Mary")                                                  shouldBe Some("https://www.multikino.pl/filmy/projekt-hail-mary")
    filmUrls("Pucio")                                                              shouldBe Some("https://www.multikino.pl/filmy/pucio")
    filmUrls("Rambo: Pierwsza krew")                                               shouldBe Some("https://www.multikino.pl/filmy/rambo-pierwsza-krew")
    filmUrls("Romeo i Julia – wersja oryginalna")                                  shouldBe Some("https://www.multikino.pl/filmy/romeo-i-julia-wersja-oryginalna")
    filmUrls("Sprawiedliwość owiec")                                               shouldBe Some("https://www.multikino.pl/filmy/sprawiedliwosc-owiec")
    filmUrls("Straszny film")                                                      shouldBe Some("https://www.multikino.pl/filmy/straszny-film")
    filmUrls("Super Mario Galaxy Film")                                            shouldBe Some("https://www.multikino.pl/filmy/super-mario-galaxy-film")
    filmUrls("The Amazing Digital Circus: Ostatni Akt")                            shouldBe Some("https://www.multikino.pl/filmy/the-amazing-digital-circus-ostatni-akt")
    filmUrls("Top Gun")                                                            shouldBe Some("https://www.multikino.pl/filmy/top-gun")
    filmUrls("Top Gun: Maverick")                                                  shouldBe Some("https://www.multikino.pl/filmy/top-gun-maverick")
    filmUrls("Werdykt")                                                            shouldBe Some("https://www.multikino.pl/filmy/werdykt")
    filmUrls("Wolność po włosku")                                                  shouldBe Some("https://www.multikino.pl/filmy/wolnosc-po-wlosku")
    filmUrls("Za duży na bajki 3")                                                 shouldBe Some("https://www.multikino.pl/filmy/za-duzy-na-bajki-3")
    filmUrls("Zaplątani")                                                          shouldBe Some("https://www.multikino.pl/filmy/zaplatani")
    filmUrls("Zaproszenie")                                                        shouldBe Some("https://www.multikino.pl/filmy/kino-na-obcasach-zaproszenie")
    filmUrls("Zawieście czerwone latarnie")                                        shouldBe Some("https://www.multikino.pl/filmy/zawiescie-czerwone-latarnie")
    filmUrls("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana")            shouldBe Some("https://www.multikino.pl/filmy/zywot-briana-grupy-monty-pythona-wersja-zremasterowana")
  }

  // ── Directors ─────────────────────────────────────────────────────────────

  it should "return correct director for movies that have one" in {
    byTitle("90. urodziny Pavarottiego").director                                          shouldBe Some("Luigi Antonini, Matteo Parmeggiani")
    byTitle("Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza").director shouldBe Some("Simona Risi")
    byTitle("Art Beats: Muzeum Prado - kolekcja cudów").director                           shouldBe Some("Valeria Parisi")
    byTitle("Art Beats: Nenufary Moneta - cuda z wody i światła").director                 shouldBe Some("Giovanni Troilo")
    byTitle("Art Beats: Rafael. Młody geniusz").director                                   shouldBe Some(" Massimo Ferrari")
    byTitle("Billie Eilish - Hit Me Hard and Soft: The Tour").director                     shouldBe Some("James Cameron")
    byTitle("Bluey w kinie: Kolekcja Zabawy z przyjaciółmi").director                      shouldBe Some("Joe Brumm")
    byTitle("Caravaggio. Arcydzieła niepokornego geniusza").director                       shouldBe Some("David Bickerstaff, Phil Grabsky")
    byTitle("Cirque du Soleil: Kooza").director                                            shouldBe Some("Mario Janelle, David Shiner")
    byTitle("Cirque du Soleil: Kurios - Gabinet osobliwości").director                     shouldBe Some("Michel Laprise")
    byTitle("Diabeł ubiera się u Prady 2").director                                        shouldBe Some("David Frankel")
    byTitle("Drama").director                                                              shouldBe Some("Kristoffer Borgli ")
    byTitle("Erupcja").director                                                            shouldBe Some("Pete Ohs")
    byTitle("FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ").director                            shouldBe Some("David Yates")
    byTitle("Fantastyczne zwierzęta: Tajemnice Dumbledorea").director                      shouldBe Some("David Yates")
    byTitle("Harry Potter i Czara ognia").director                                         shouldBe Some("Mike Newell")
    byTitle("Harry Potter i Insygnia Śmierci cz. 1").director                              shouldBe Some("David Yates")
    byTitle("Harry Potter i Insygnia Śmierci cz. 2").director                              shouldBe Some("David Yates")
    byTitle("Harry Potter i Kamień filozoficzny").director                                 shouldBe Some("Chris Columbus")
    byTitle("Harry Potter i Komnata Tajemnic").director                                    shouldBe Some("Chris Columbus")
    byTitle("Harry Potter i Książę Półkrwi").director                                      shouldBe Some("David Yates")
    byTitle("Harry Potter i Więzień Azkabanu").director                                    shouldBe Some("David Yates")
    byTitle("Harry Potter i Zakon Feniksa").director                                       shouldBe Some("David Yates")
    byTitle("Hopnięci").director                                                           shouldBe Some("Daniel Chong")
    byTitle("Iron Maiden: Burning Ambition").director                                      shouldBe Some("Malcolm Venville")
    byTitle("John Williams - A Tribute").director                                          shouldBe Some("Antony Hermus")
    byTitle("Klątwa doliny węży -  z autorską narracją Łony").director                     shouldBe Some("Marek Piestrak")
    byTitle("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas").director         shouldBe Some("Mamoru Hosoda")
    byTitle("Kosmiczny mecz").director                                                     shouldBe Some("Joe Pytka")
    byTitle("Kurozając i Świątynia Świstaka").director                                     shouldBe Some("Benjamin Mousquet")
    byTitle("La Traviata Verdiego z Arena di Verona").director                             shouldBe Some("Michele Olcese, Francesco Ivan Ciampa")
    byTitle("Mandalorian i Grogu").director                                                shouldBe Some("Jon Favreau")
    byTitle("Merrily We Roll Along").director                                              shouldBe Some("Maria Friedman")
    byTitle("Michael").director                                                            shouldBe Some("Antoine Fuqua")
    byTitle("Milczenie owiec").director                                                    shouldBe Some("Jonathan Demme")
    byTitle("Milcząca przyjaciółka").director                                              shouldBe Some("Ildikó Enyedi")
    byTitle("Mortal Kombat 2").director                                                    shouldBe Some("Simon McQuoid")
    byTitle("Moulin Rouge! – wersja oryginalna").director                                  shouldBe Some("Baz Luhrmann")
    byTitle("Mumia: Film Lee Cronina").director                                            shouldBe Some("Lee Cronin")
    byTitle("Niesamowite przygody skarpetek 3. Ale kosmos!").director                      shouldBe Some("Elżbieta Wąsik, Paweł Wendorff\t")
    byTitle("Odlot").director                                                              shouldBe Some("Pete Docter, Bob Peterson")
    byTitle("Odrodzony jako galareta. Film: Łzy Morza Lazurowego").director                shouldBe Some(" Yasuhito Kikuchi ")
    byTitle("Piep*zyć Mickiewicza 3").director                                             shouldBe Some("Sara Bustamante-Drozdek")
    byTitle("Podziemny krąg").director                                                     shouldBe Some("David Fincher")
    byTitle("Powrót do przyszłości").director                                              shouldBe Some("Robert Zemeckis")
    byTitle("Powrót do przyszłości II").director                                           shouldBe Some("Robert Zemeckis")
    byTitle("Powrót do przyszłości III").director                                          shouldBe Some("Robert Zemeckis")
    byTitle("Projekt Hail Mary").director                                                  shouldBe Some("Phil Lord, Christopher Miller")
    byTitle("Pucio").director                                                              shouldBe Some("Marta Stróżycka")
    byTitle("Rambo: Pierwsza krew").director                                               shouldBe Some("Ted Kotcheff")
    byTitle("Romeo i Julia – wersja oryginalna").director                                  shouldBe Some("Baz Luhrmann")
    byTitle("Sprawiedliwość owiec").director                                               shouldBe Some("Kyle Balda")
    byTitle("Super Mario Galaxy Film").director                                            shouldBe Some("Aaron Horvath, Michael Jelenic")
    byTitle("The Amazing Digital Circus: Ostatni Akt").director                            shouldBe Some("Gooseworx  ")
    byTitle("Top Gun").director                                                            shouldBe Some("Tony Scott")
    byTitle("Top Gun: Maverick").director                                                  shouldBe Some("Joseph Kosinski")
    byTitle("Werdykt").director                                                            shouldBe Some("David Merriman, Jim Sheridan")
    byTitle("Wolność po włosku").director                                                  shouldBe Some("Mario Martone")
    byTitle("Za duży na bajki 3").director                                                 shouldBe Some("Kristoffer Rus")
    byTitle("Zaplątani").director                                                          shouldBe Some("Nathan Greno, Byron Howard")
    byTitle("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana").director            shouldBe Some("Terry Jones")
    byTitle("Drugie życie").director                                                       shouldBe None
    byTitle("Drzewo magii").director                                                       shouldBe None
    byTitle("FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA").director                      shouldBe None
    byTitle("LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC").director  shouldBe None
    byTitle("Maraton: Powrót do przyszłości").director                                     shouldBe None
    byTitle("NT Live: Audiencja").director                                                 shouldBe None
    byTitle("NT Live: Niebezpieczne związki").director                                     shouldBe None
    byTitle("NT Live: Playboy zachodniego świata").director                                shouldBe None
    byTitle("NT Live: Wszyscy moi synowie").director                                       shouldBe None
    byTitle("Obsesja").director                                                            shouldBe None
    byTitle("Straszny film").director                                                      shouldBe None
    byTitle("Zaproszenie").director                                                        shouldBe None
    byTitle("Zawieście czerwone latarnie").director                                        shouldBe None
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("90. urodziny Pavarottiego")                                          shouldBe 2
    counts("Art Beats: Lotto i Berenson - splecione losy. Śladami renesansowego mistrza") shouldBe 2
    counts("Art Beats: Muzeum Prado - kolekcja cudów")                           shouldBe 1
    counts("Art Beats: Nenufary Moneta - cuda z wody i światła")                 shouldBe 2
    counts("Art Beats: Rafael. Młody geniusz")                                   shouldBe 2
    counts("Billie Eilish - Hit Me Hard and Soft: The Tour")                     shouldBe 22
    counts("Bluey w kinie: Kolekcja Zabawy z przyjaciółmi")                      shouldBe 16
    counts("Caravaggio. Arcydzieła niepokornego geniusza")                       shouldBe 4
    counts("Cirque du Soleil: Kooza")                                            shouldBe 1
    counts("Cirque du Soleil: Kurios - Gabinet osobliwości")                     shouldBe 1
    counts("Diabeł ubiera się u Prady 2")                                        shouldBe 100
    counts("Drama")                                                              shouldBe 17
    counts("Drugie życie")                                                       shouldBe 1
    counts("Drzewo magii")                                                       shouldBe 3
    counts("Erupcja")                                                            shouldBe 1
    counts("FANTASTYCZNE ZWIERZĘTA I JAK JE ZNALEŹĆ")                            shouldBe 2
    counts("FANTASTYCZNE ZWIERZĘTA: ZBRODNIE GRINDELWALDA")                      shouldBe 2
    counts("Fantastyczne zwierzęta: Tajemnice Dumbledorea")                      shouldBe 2
    counts("Harry Potter i Czara ognia")                                         shouldBe 1
    counts("Harry Potter i Insygnia Śmierci cz. 1")                              shouldBe 1
    counts("Harry Potter i Insygnia Śmierci cz. 2")                              shouldBe 1
    counts("Harry Potter i Kamień filozoficzny")                                 shouldBe 7
    counts("Harry Potter i Komnata Tajemnic")                                    shouldBe 1
    counts("Harry Potter i Książę Półkrwi")                                      shouldBe 1
    counts("Harry Potter i Więzień Azkabanu")                                    shouldBe 1
    counts("Harry Potter i Zakon Feniksa")                                       shouldBe 1
    counts("Hopnięci")                                                           shouldBe 9
    counts("Iron Maiden: Burning Ambition")                                      shouldBe 7
    counts("John Williams - A Tribute")                                          shouldBe 4
    counts("Klątwa doliny węży -  z autorską narracją Łony")                     shouldBe 1
    counts("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")         shouldBe 2
    counts("Kosmiczny mecz")                                                     shouldBe 5
    counts("Kurozając i Świątynia Świstaka")                                     shouldBe 33
    counts("LIGA MISTRZÓW UEFA - FINAŁ 2026: Paris Saint-Germain - Arsenal FC")  shouldBe 1
    counts("La Traviata Verdiego z Arena di Verona")                             shouldBe 1
    counts("Mandalorian i Grogu")                                                shouldBe 27
    counts("Maraton: Powrót do przyszłości")                                     shouldBe 1
    counts("Merrily We Roll Along")                                              shouldBe 2
    counts("Michael")                                                            shouldBe 71
    counts("Milczenie owiec")                                                    shouldBe 2
    counts("Milcząca przyjaciółka")                                              shouldBe 1
    counts("Mortal Kombat 2")                                                    shouldBe 55
    counts("Moulin Rouge! – wersja oryginalna")                                  shouldBe 2
    counts("Mumia: Film Lee Cronina")                                            shouldBe 7
    counts("NT Live: Audiencja")                                                 shouldBe 1
    counts("NT Live: Niebezpieczne związki")                                     shouldBe 1
    counts("NT Live: Playboy zachodniego świata")                                shouldBe 1
    counts("NT Live: Wszyscy moi synowie")                                       shouldBe 1
    counts("Niesamowite przygody skarpetek 3. Ale kosmos!")                      shouldBe 1
    counts("Obsesja")                                                            shouldBe 22
    counts("Odlot")                                                              shouldBe 2
    counts("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")                shouldBe 4
    counts("Piep*zyć Mickiewicza 3")                                             shouldBe 1
    counts("Podziemny krąg")                                                     shouldBe 5
    counts("Powrót do przyszłości")                                              shouldBe 2
    counts("Powrót do przyszłości II")                                           shouldBe 2
    counts("Powrót do przyszłości III")                                          shouldBe 4
    counts("Projekt Hail Mary")                                                  shouldBe 17
    counts("Pucio")                                                              shouldBe 40
    counts("Rambo: Pierwsza krew")                                               shouldBe 1
    counts("Romeo i Julia – wersja oryginalna")                                  shouldBe 2
    counts("Sprawiedliwość owiec")                                               shouldBe 35
    counts("Straszny film")                                                      shouldBe 17
    counts("Super Mario Galaxy Film")                                            shouldBe 30
    counts("The Amazing Digital Circus: Ostatni Akt")                            shouldBe 23
    counts("Top Gun")                                                            shouldBe 7
    counts("Top Gun: Maverick")                                                  shouldBe 7
    counts("Werdykt")                                                            shouldBe 1
    counts("Wolność po włosku")                                                  shouldBe 1
    counts("Za duży na bajki 3")                                                 shouldBe 5
    counts("Zaplątani")                                                          shouldBe 2
    counts("Zaproszenie")                                                        shouldBe 1
    counts("Zawieście czerwone latarnie")                                        shouldBe 1
    counts("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana")            shouldBe 2
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for John Williams - A Tribute" in {
    val st = byTitle("John Williams - A Tribute").showtimes
    st.size shouldBe 4
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 6, 6,  15, 0), Some("https://www.multikino.pl/rezerwacja-biletow/podsumowanie/0011/HO00002683/98855"), Some("Sala 3"), None),
      Showtime(LocalDateTime.of(2026, 6, 16, 18, 0), Some("https://www.multikino.pl/rezerwacja-biletow/podsumowanie/0011/HO00002683/93217"), Some("Sala 3"), None),
      Showtime(LocalDateTime.of(2026, 7, 22, 18, 0), Some("https://www.multikino.pl/rezerwacja-biletow/podsumowanie/0011/HO00002683/93218"), Some("Sala 3"), None),
      Showtime(LocalDateTime.of(2026, 8, 29, 15, 0), Some("https://www.multikino.pl/rezerwacja-biletow/podsumowanie/0011/HO00002683/93219"), Some("Sala 1"), None),
    )
  }

  it should "return exact showtime for Cirque du Soleil: Kooza" in {
    val st = byTitle("Cirque du Soleil: Kooza").showtimes
    st.size shouldBe 1
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 7, 16, 18, 0), Some("https://www.multikino.pl/rezerwacja-biletow/podsumowanie/0011/HO00002648/91260"), Some("Sala 1"), None),
    )
  }
}
