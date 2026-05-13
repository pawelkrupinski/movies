package clients.cinema_city

import clients.CinemaCityClient
import clients.tools.FakeHttpFetch
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class CinemaCityClientSpec extends AnyFlatSpec with Matchers {

  private val kinepolis   = new CinemaCityClient(new FakeHttpFetch("cinema-city-kinepolis")).fetch("1081", CinemaCityKinepolis)
  private val byKinepolis = kinepolis.map(cm => cm.movie.title -> cm).toMap

  private val plaza   = new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza")).fetch("1078", CinemaCityPoznanPlaza)
  private val byPlaza = plaza.map(cm => cm.movie.title -> cm).toMap

  // ─── Kinepolis: totals ────────────────────────────────────────────────────

  "CinemaCityClient.fetch (Kinepolis)" should "return exactly 39 films from fixture" in {
    kinepolis.size shouldBe 39
  }

  it should "return 778 showtimes in total" in {
    kinepolis.flatMap(_.showtimes).size shouldBe 778
  }

  it should "assign CinemaCityKinepolis cinema to all entries" in {
    kinepolis.map(_.cinema).toSet shouldBe Set(CinemaCityKinepolis)
  }

  // ─── Kinepolis: title set ─────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    kinepolis.map(_.movie.title).toSet shouldBe Set(
      "2026 TXT MOA CON IN JAPAN: LIVE VIEWING",
      "90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona",
      "Athiradi",
      "Billie Eilish – Hit Me Hard and Soft: The Tour",
      "Diabeł ubiera się u Prady 2",
      "Drama",
      "Drzewo magii",
      "Erupcja",
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Hopnięci",
      "Iron Maiden: Burning Ambition",
      "Karuppu",
      "Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas",
      "Kurozając i świątynia świstaka",
      "La Traviata Verdiego z Arena di Verona",
      "Liga Mistrzów UEFA - Finał",
      "Michael",
      "Milcząca przyjaciółka",
      "Mortal Kombat II",
      "Mumia: Film Lee Cronina",
      "Narodziny gwiazdy",
      "Normal",
      "Obsesja",
      "Odrodzony jako galareta. Film: Łzy Morza Lazurowego",
      "Projekt Hail Mary",
      "Przepis na morderstwo",
      "Pucio",
      "Romeria",
      "Sabotażysta 5",
      "Sprawiedliwość owiec",
      "Straszny film",
      "Super Mario Galaxy Film",
      "The Amazing Digital Circus: The Last Act",
      "Top Gun 40th Anniversary",
      "Top Gun Maverick",
      "Wolność po włosku",
      "Władcy Wszechświata",
      "Za duży na bajki 3",
      "Zabawa w pochowanego 2",
    )
  }

  // ─── Kinepolis: runtimes ──────────────────────────────────────────────────

  it should "return correct runtime for every film" in {
    val runtimes = kinepolis.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("2026 TXT MOA CON IN JAPAN: LIVE VIEWING")                      shouldBe Some(225)
    runtimes("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe Some(92)
    runtimes("Athiradi")                                                      shouldBe Some(157)
    runtimes("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe Some(114)
    runtimes("Diabeł ubiera się u Prady 2")                                  shouldBe Some(119)
    runtimes("Drama")                                                         shouldBe Some(106)
    runtimes("Drzewo magii")                                                  shouldBe Some(110)
    runtimes("Erupcja")                                                       shouldBe Some(71)
    runtimes("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe Some(132)
    runtimes("Hopnięci")                                                      shouldBe Some(104)
    runtimes("Iron Maiden: Burning Ambition")                                 shouldBe Some(105)
    runtimes("Karuppu")                                                       shouldBe Some(150)
    runtimes("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe Some(98)
    runtimes("Kurozając i świątynia świstaka")                                shouldBe Some(88)
    runtimes("La Traviata Verdiego z Arena di Verona")                        shouldBe Some(160)
    runtimes("Liga Mistrzów UEFA - Finał")                                    shouldBe Some(140)
    runtimes("Michael")                                                       shouldBe Some(127)
    runtimes("Milcząca przyjaciółka")                                         shouldBe Some(145)
    runtimes("Mortal Kombat II")                                              shouldBe Some(116)
    runtimes("Mumia: Film Lee Cronina")                                       shouldBe Some(134)
    runtimes("Narodziny gwiazdy")                                             shouldBe Some(135)
    runtimes("Normal")                                                        shouldBe Some(90)
    runtimes("Obsesja")                                                       shouldBe Some(109)
    runtimes("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe Some(105)
    runtimes("Projekt Hail Mary")                                             shouldBe Some(156)
    runtimes("Przepis na morderstwo")                                         shouldBe Some(105)
    runtimes("Pucio")                                                         shouldBe Some(45)
    runtimes("Romeria")                                                       shouldBe Some(114)
    runtimes("Sabotażysta 5")                                                 shouldBe None
    runtimes("Sprawiedliwość owiec")                                          shouldBe Some(109)
    runtimes("Straszny film")                                                 shouldBe Some(94)
    runtimes("Super Mario Galaxy Film")                                       shouldBe Some(98)
    runtimes("The Amazing Digital Circus: The Last Act")                      shouldBe Some(93)
    runtimes("Top Gun 40th Anniversary")                                      shouldBe Some(110)
    runtimes("Top Gun Maverick")                                              shouldBe Some(131)
    runtimes("Wolność po włosku")                                             shouldBe Some(115)
    runtimes("Władcy Wszechświata")                                           shouldBe Some(141)
    runtimes("Za duży na bajki 3")                                            shouldBe Some(90)
    runtimes("Zabawa w pochowanego 2")                                        shouldBe Some(104)
  }

  // ─── Kinepolis: poster URLs ───────────────────────────────────────────────

  it should "return correct poster URL for every film" in {
    val posters = kinepolis.map(m => m.movie.title -> m.posterUrl).toMap
    posters("2026 TXT MOA CON IN JAPAN: LIVE VIEWING")                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8178O2R.jpg")
    posters("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8131S2R.jpg")
    posters("Athiradi")                                                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8204S2R.jpg")
    posters("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7912S2R.jpg")
    posters("Diabeł ubiera się u Prady 2")                                  shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7795S2R.jpg")
    posters("Drama")                                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7997S2R.jpg")
    posters("Drzewo magii")                                                  shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8180D2R.jpg")
    posters("Erupcja")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8170S2R.jpg")
    posters("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7667S2R.jpg")
    posters("Hopnięci")                                                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7499D2R.jpg")
    posters("Iron Maiden: Burning Ambition")                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8078S2R.jpg")
    posters("Karuppu")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8203S2R.jpg")
    posters("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8068S2R.jpg")
    posters("Kurozając i świątynia świstaka")                                shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7921D2R.jpg")
    posters("La Traviata Verdiego z Arena di Verona")                        shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8130S2R.jpg")
    posters("Liga Mistrzów UEFA - Finał")                                    shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8063O2R.jpg")
    posters("Michael")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7785S2R.jpg")
    posters("Milcząca przyjaciółka")                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8143S2R.jpg")
    posters("Mortal Kombat II")                                              shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7478S2R.jpg")
    posters("Mumia: Film Lee Cronina")                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7949S2R.jpg")
    posters("Narodziny gwiazdy")                                             shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/3026S2R1.jpg")
    posters("Normal")                                                        shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8129S2R.jpg")
    posters("Obsesja")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8134S2R.jpg")
    posters("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8076S2R.jpg")
    posters("Projekt Hail Mary")                                             shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7449S2R.jpg")
    posters("Przepis na morderstwo")                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7933S2R.jpg")
    posters("Pucio")                                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8099D2R.jpg")
    posters("Romeria")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7807S2R.jpg")
    posters("Sabotażysta 5")                                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8186O2R.jpg")
    posters("Sprawiedliwość owiec")                                          shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7956D2R.jpg")
    posters("Straszny film")                                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8117S2R.jpg")
    posters("Super Mario Galaxy Film")                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7999D2R.jpg")
    posters("The Amazing Digital Circus: The Last Act")                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8150O2R.jpg")
    posters("Top Gun 40th Anniversary")                                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8091S2R.jpg")
    posters("Top Gun Maverick")                                              shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/3639S2R.jpg")
    posters("Wolność po włosku")                                             shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8002S2R.jpg")
    posters("Władcy Wszechświata")                                           shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7992S2R.jpg")
    posters("Za duży na bajki 3")                                            shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7982O2R.jpg")
    posters("Zabawa w pochowanego 2")                                        shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7998S2R.jpg")
  }

  // ─── Kinepolis: film URLs ─────────────────────────────────────────────────

  it should "return correct film URL for every film" in {
    val filmUrls = kinepolis.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("2026 TXT MOA CON IN JAPAN: LIVE VIEWING")                      shouldBe Some("https://www.cinema-city.pl/filmy/2026-txt-moa-con-in-japan-live-viewing/8178o2r")
    filmUrls("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe Some("https://www.cinema-city.pl/filmy/90-urodziny-pavarottiego-koncert-gwiazd-z-arena-di-verona/8131s2r")
    filmUrls("Athiradi")                                                      shouldBe Some("https://www.cinema-city.pl/filmy/athiradi/8204s2r")
    filmUrls("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe Some("https://www.cinema-city.pl/filmy/billie-eilish-hit-me-hard-and-soft-the-tour/7912s2r")
    filmUrls("Diabeł ubiera się u Prady 2")                                  shouldBe Some("https://www.cinema-city.pl/filmy/diabel-ubiera-sie-u-prady-2/7795s2r")
    filmUrls("Drama")                                                         shouldBe Some("https://www.cinema-city.pl/filmy/drama/7997s2r")
    filmUrls("Drzewo magii")                                                  shouldBe Some("https://www.cinema-city.pl/filmy/drzewo-magii/8180d2r")
    filmUrls("Erupcja")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/erupcja/8170s2r")
    filmUrls("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe Some("https://www.cinema-city.pl/filmy/gwiezdne-wojny-mandalorian-i-grogu/7667s2r")
    filmUrls("Hopnięci")                                                      shouldBe Some("https://www.cinema-city.pl/filmy/hopnieci/7499d2r")
    filmUrls("Iron Maiden: Burning Ambition")                                 shouldBe Some("https://www.cinema-city.pl/filmy/iron-maiden-burning-ambition/8078s2r")
    filmUrls("Karuppu")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/karuppu/8203s2r")
    filmUrls("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe Some("https://www.cinema-city.pl/filmy/kolekcja-mamoru-hosody-o-dziewczynie-skaczacej-przez-czas/8068s2r")
    filmUrls("Kurozając i świątynia świstaka")                                shouldBe Some("https://www.cinema-city.pl/filmy/kurozajac-i-swiatynia-swistaka/7921d2r")
    filmUrls("La Traviata Verdiego z Arena di Verona")                        shouldBe Some("https://www.cinema-city.pl/filmy/la-traviata-verdiego-z-arena-di-verona/8130s2r")
    filmUrls("Liga Mistrzów UEFA - Finał")                                    shouldBe Some("https://www.cinema-city.pl/filmy/liga-mistrzow-uefa-final/8063o2r")
    filmUrls("Michael")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/michael/7785s2r")
    filmUrls("Milcząca przyjaciółka")                                         shouldBe Some("https://www.cinema-city.pl/filmy/milczaca-przyjaciolka/8143s2r")
    filmUrls("Mortal Kombat II")                                              shouldBe Some("https://www.cinema-city.pl/filmy/mortal-kombat-ii/7478s2r")
    filmUrls("Mumia: Film Lee Cronina")                                       shouldBe Some("https://www.cinema-city.pl/filmy/mumia-film-lee-cronina/7949s2r")
    filmUrls("Narodziny gwiazdy")                                             shouldBe Some("https://www.cinema-city.pl/filmy/ladies-night-narodziny-gwiazdy/3026s2r1")
    filmUrls("Normal")                                                        shouldBe Some("https://www.cinema-city.pl/filmy/normal/8129s2r")
    filmUrls("Obsesja")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/obsesja/8134s2r")
    filmUrls("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe Some("https://www.cinema-city.pl/filmy/odrodzony-jako-galareta-film-lzy-morza-lazurowego/8076s2r")
    filmUrls("Projekt Hail Mary")                                             shouldBe Some("https://www.cinema-city.pl/filmy/projekt-hail-mary/7449s2r")
    filmUrls("Przepis na morderstwo")                                         shouldBe Some("https://www.cinema-city.pl/filmy/przepis-na-morderstwo/7933s2r")
    filmUrls("Pucio")                                                         shouldBe Some("https://www.cinema-city.pl/filmy/pucio/8099d2r")
    filmUrls("Romeria")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/romeria/7807s2r")
    filmUrls("Sabotażysta 5")                                                 shouldBe Some("https://www.cinema-city.pl/filmy/sabotazysta-5/8186o2r")
    filmUrls("Sprawiedliwość owiec")                                          shouldBe Some("https://www.cinema-city.pl/filmy/sprawiedliwosc-owiec/7956d2r")
    filmUrls("Straszny film")                                                 shouldBe Some("https://www.cinema-city.pl/filmy/straszny-film/8117s2r")
    filmUrls("Super Mario Galaxy Film")                                       shouldBe Some("https://www.cinema-city.pl/filmy/super-mario-galaxy-film/7999d2r")
    filmUrls("The Amazing Digital Circus: The Last Act")                      shouldBe Some("https://www.cinema-city.pl/filmy/the-amazing-digital-circus-the-last-act/8150o2r")
    filmUrls("Top Gun 40th Anniversary")                                      shouldBe Some("https://www.cinema-city.pl/filmy/top-gun-40th-anniversary/8091s2r")
    filmUrls("Top Gun Maverick")                                              shouldBe Some("https://www.cinema-city.pl/filmy/top-gun-maverick/3639s2r")
    filmUrls("Wolność po włosku")                                             shouldBe Some("https://www.cinema-city.pl/filmy/wolnosc-po-wlosku/8002s2r")
    filmUrls("Władcy Wszechświata")                                           shouldBe Some("https://www.cinema-city.pl/filmy/wladcy-wszechswiata/7992s2r")
    filmUrls("Za duży na bajki 3")                                            shouldBe Some("https://www.cinema-city.pl/filmy/za-duzy-na-bajki-3/7982o2r")
    filmUrls("Zabawa w pochowanego 2")                                        shouldBe Some("https://www.cinema-city.pl/filmy/zabawa-w-pochowanego-2/7998s2r")
  }

  // ─── Kinepolis: showtime counts ───────────────────────────────────────────

  it should "return correct showtime count for every film" in {
    val counts = kinepolis.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("2026 TXT MOA CON IN JAPAN: LIVE VIEWING")                      shouldBe 1
    counts("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe 2
    counts("Athiradi")                                                      shouldBe 3
    counts("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe 55
    counts("Diabeł ubiera się u Prady 2")                                  shouldBe 153
    counts("Drama")                                                         shouldBe 21
    counts("Drzewo magii")                                                  shouldBe 3
    counts("Erupcja")                                                       shouldBe 6
    counts("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe 47
    counts("Hopnięci")                                                      shouldBe 5
    counts("Iron Maiden: Burning Ambition")                                 shouldBe 14
    counts("Karuppu")                                                       shouldBe 3
    counts("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe 2
    counts("Kurozając i świątynia świstaka")                                shouldBe 30
    counts("La Traviata Verdiego z Arena di Verona")                        shouldBe 2
    counts("Liga Mistrzów UEFA - Finał")                                    shouldBe 1
    counts("Michael")                                                       shouldBe 41
    counts("Milcząca przyjaciółka")                                         shouldBe 11
    counts("Mortal Kombat II")                                              shouldBe 59
    counts("Mumia: Film Lee Cronina")                                       shouldBe 2
    counts("Narodziny gwiazdy")                                             shouldBe 1
    counts("Normal")                                                        shouldBe 4
    counts("Obsesja")                                                       shouldBe 28
    counts("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe 11
    counts("Projekt Hail Mary")                                             shouldBe 20
    counts("Przepis na morderstwo")                                         shouldBe 11
    counts("Pucio")                                                         shouldBe 7
    counts("Romeria")                                                       shouldBe 7
    counts("Sabotażysta 5")                                                 shouldBe 22
    counts("Sprawiedliwość owiec")                                          shouldBe 31
    counts("Straszny film")                                                 shouldBe 43
    counts("Super Mario Galaxy Film")                                       shouldBe 22
    counts("The Amazing Digital Circus: The Last Act")                      shouldBe 44
    counts("Top Gun 40th Anniversary")                                      shouldBe 24
    counts("Top Gun Maverick")                                              shouldBe 3
    counts("Wolność po włosku")                                             shouldBe 2
    counts("Władcy Wszechświata")                                           shouldBe 21
    counts("Za duży na bajki 3")                                            shouldBe 7
    counts("Zabawa w pochowanego 2")                                        shouldBe 9
  }

  // ─── Kinepolis: full showtime details ────────────────────────────────────

  it should "return exact showtime for Liga Mistrzów UEFA - Finał" in {
    val st = byKinepolis("Liga Mistrzów UEFA - Finał").showtimes
    st.size shouldBe 1
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 30, 17, 45), Some("https://tickets.cinema-city.pl/api/order/1326845?lang=pl"), Some("Sala6"), List("2D")),
    )
  }

  it should "return exact showtimes for La Traviata Verdiego z Arena di Verona" in {
    val st = byKinepolis("La Traviata Verdiego z Arena di Verona").showtimes
    st.size shouldBe 2
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 18, 0), Some("https://tickets.cinema-city.pl/api/order/1411051?lang=pl"), Some("Sala3"), List("2D")),
      Showtime(LocalDateTime.of(2026, 6,  7, 15, 0), Some("https://tickets.cinema-city.pl/api/order/1411056?lang=pl"), Some("Sala4"), List("2D")),
    )
  }

  it should "extract 2D / 3D format on Kinepolis showtimes" in {
    val all2D = kinepolis.flatMap(_.showtimes).count(_.format.contains("2D"))
    val all3D = kinepolis.flatMap(_.showtimes).count(_.format.contains("3D"))
    all2D shouldBe 738
    all3D shouldBe 40
  }

  // ─── Plaza: totals ────────────────────────────────────────────────────────

  "CinemaCityClient.fetch (Plaza)" should "return exactly 35 films from fixture" in {
    plaza.size shouldBe 35
  }

  it should "return 577 showtimes in total" in {
    plaza.flatMap(_.showtimes).size shouldBe 577
  }

  it should "assign CinemaCityPoznanPlaza cinema to all entries" in {
    plaza.map(_.cinema).toSet shouldBe Set(CinemaCityPoznanPlaza)
  }

  // ─── Plaza: title set ─────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    plaza.map(_.movie.title).toSet shouldBe Set(
      "90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona",
      "Billie Eilish – Hit Me Hard and Soft: The Tour",
      "Diabeł ubiera się u Prady 2",
      "Diabeł ubiera się u Prady 2 ukraiński dubbing",
      "Drama",
      "Drzewo magii",
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Gwiezdne Wojny: Mandalorian i Grogu - ukraiński dubbing",
      "Hopnięci",
      "Iron Maiden: Burning Ambition",
      "Kicia Kocia w podróży",
      "Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas",
      "Kurozając i świątynia świstaka",
      "La Traviata Verdiego z Arena di Verona",
      "Lars jest LOL",
      "Liga Mistrzów UEFA - Finał",
      "Michael",
      "Mortal Kombat II",
      "Mortal Kombat II ukraiński dubbing",
      "Narodziny gwiazdy",
      "Niesamowite przygody skarpetek 3. Ale ko",
      "Normal",
      "Obsesja",
      "Odrodzony jako galareta. Film: Łzy Morza Lazurowego",
      "Piep*zyć Mickiewicza 3",
      "Projekt Hail Mary",
      "Pucio",
      "Sabotażysta 5",
      "Sprawiedliwość owiec",
      "Straszny film",
      "Super Mario Galaxy Film",
      "The Amazing Digital Circus: The Last Act",
      "Top Gun 40th Anniversary",
      "Top Gun Maverick",
      "Władcy Wszechświata",
    )
  }

  // ─── Plaza: runtimes ──────────────────────────────────────────────────────

  it should "return correct runtime for every film" in {
    val runtimes = plaza.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe Some(92)
    runtimes("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe Some(114)
    runtimes("Diabeł ubiera się u Prady 2")                                  shouldBe Some(119)
    runtimes("Diabeł ubiera się u Prady 2 ukraiński dubbing")                shouldBe Some(119)
    runtimes("Drama")                                                         shouldBe Some(106)
    runtimes("Drzewo magii")                                                  shouldBe Some(110)
    runtimes("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe Some(132)
    runtimes("Gwiezdne Wojny: Mandalorian i Grogu - ukraiński dubbing")       shouldBe Some(132)
    runtimes("Hopnięci")                                                      shouldBe Some(104)
    runtimes("Iron Maiden: Burning Ambition")                                 shouldBe Some(105)
    runtimes("Kicia Kocia w podróży")                                         shouldBe Some(45)
    runtimes("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe Some(98)
    runtimes("Kurozając i świątynia świstaka")                                shouldBe Some(88)
    runtimes("La Traviata Verdiego z Arena di Verona")                        shouldBe Some(160)
    runtimes("Lars jest LOL")                                                 shouldBe Some(89)
    runtimes("Liga Mistrzów UEFA - Finał")                                    shouldBe Some(140)
    runtimes("Michael")                                                       shouldBe Some(127)
    runtimes("Mortal Kombat II")                                              shouldBe Some(116)
    runtimes("Mortal Kombat II ukraiński dubbing")                            shouldBe Some(116)
    runtimes("Narodziny gwiazdy")                                             shouldBe Some(135)
    runtimes("Niesamowite przygody skarpetek 3. Ale ko")                      shouldBe Some(55)
    runtimes("Normal")                                                        shouldBe Some(90)
    runtimes("Obsesja")                                                       shouldBe Some(109)
    runtimes("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe Some(105)
    runtimes("Piep*zyć Mickiewicza 3")                                        shouldBe Some(89)
    runtimes("Projekt Hail Mary")                                             shouldBe Some(156)
    runtimes("Pucio")                                                         shouldBe Some(45)
    runtimes("Sabotażysta 5")                                                 shouldBe None
    runtimes("Sprawiedliwość owiec")                                          shouldBe Some(109)
    runtimes("Straszny film")                                                 shouldBe Some(94)
    runtimes("Super Mario Galaxy Film")                                       shouldBe Some(98)
    runtimes("The Amazing Digital Circus: The Last Act")                      shouldBe Some(93)
    runtimes("Top Gun 40th Anniversary")                                      shouldBe Some(110)
    runtimes("Top Gun Maverick")                                              shouldBe Some(131)
    runtimes("Władcy Wszechświata")                                           shouldBe Some(141)
  }

  // ─── Plaza: poster URLs ───────────────────────────────────────────────────

  it should "return correct poster URL for every film" in {
    val posters = plaza.map(m => m.movie.title -> m.posterUrl).toMap
    posters("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8131S2R.jpg")
    posters("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7912S2R.jpg")
    posters("Diabeł ubiera się u Prady 2")                                  shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7795S2R.jpg")
    posters("Diabeł ubiera się u Prady 2 ukraiński dubbing")                shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7795S2R2.jpg")
    posters("Drama")                                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7997S2R.jpg")
    posters("Drzewo magii")                                                  shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8180D2R.jpg")
    posters("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7667S2R.jpg")
    posters("Gwiezdne Wojny: Mandalorian i Grogu - ukraiński dubbing")       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7667S2R3.jpg")
    posters("Hopnięci")                                                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7499D2R.jpg")
    posters("Iron Maiden: Burning Ambition")                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8078S2R.jpg")
    posters("Kicia Kocia w podróży")                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7975O2R.jpg")
    posters("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8068S2R.jpg")
    posters("Kurozając i świątynia świstaka")                                shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7921D2R.jpg")
    posters("La Traviata Verdiego z Arena di Verona")                        shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8130S2R.jpg")
    posters("Lars jest LOL")                                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/6547D2R.jpg")
    posters("Liga Mistrzów UEFA - Finał")                                    shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8063O2R.jpg")
    posters("Michael")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7785S2R.jpg")
    posters("Mortal Kombat II")                                              shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7478S2R.jpg")
    posters("Mortal Kombat II ukraiński dubbing")                            shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7478S2R1.jpg")
    posters("Narodziny gwiazdy")                                             shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/3026S2R1.jpg")
    posters("Niesamowite przygody skarpetek 3. Ale ko")                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8188D2R.jpg")
    posters("Normal")                                                        shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8129S2R.jpg")
    posters("Obsesja")                                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8134S2R.jpg")
    posters("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8076S2R.jpg")
    posters("Piep*zyć Mickiewicza 3")                                        shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7841O2R.jpg")
    posters("Projekt Hail Mary")                                             shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7449S2R.jpg")
    posters("Pucio")                                                         shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8099D2R.jpg")
    posters("Sabotażysta 5")                                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8186O2R.jpg")
    posters("Sprawiedliwość owiec")                                          shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7956D2R.jpg")
    posters("Straszny film")                                                 shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8117S2R.jpg")
    posters("Super Mario Galaxy Film")                                       shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7999D2R.jpg")
    posters("The Amazing Digital Circus: The Last Act")                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8150O2R.jpg")
    posters("Top Gun 40th Anniversary")                                      shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/8091S2R.jpg")
    posters("Top Gun Maverick")                                              shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/3639S2R.jpg")
    posters("Władcy Wszechświata")                                           shouldBe Some("https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7992S2R.jpg")
  }

  // ─── Plaza: film URLs ─────────────────────────────────────────────────────

  it should "return correct film URL for every film" in {
    val filmUrls = plaza.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe Some("https://www.cinema-city.pl/filmy/90-urodziny-pavarottiego-koncert-gwiazd-z-arena-di-verona/8131s2r")
    filmUrls("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe Some("https://www.cinema-city.pl/filmy/billie-eilish-hit-me-hard-and-soft-the-tour/7912s2r")
    filmUrls("Diabeł ubiera się u Prady 2")                                  shouldBe Some("https://www.cinema-city.pl/filmy/diabel-ubiera-sie-u-prady-2/7795s2r")
    filmUrls("Diabeł ubiera się u Prady 2 ukraiński dubbing")                shouldBe Some("https://www.cinema-city.pl/filmy/diabel-ubiera-sie-u-prady-2-ukrainski-dubbing/7795s2r2")
    filmUrls("Drama")                                                         shouldBe Some("https://www.cinema-city.pl/filmy/drama/7997s2r")
    filmUrls("Drzewo magii")                                                  shouldBe Some("https://www.cinema-city.pl/filmy/drzewo-magii/8180d2r")
    filmUrls("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe Some("https://www.cinema-city.pl/filmy/gwiezdne-wojny-mandalorian-i-grogu/7667s2r")
    filmUrls("Gwiezdne Wojny: Mandalorian i Grogu - ukraiński dubbing")       shouldBe Some("https://www.cinema-city.pl/filmy/gwiezdne-wojny-mandalorian-i-grogu-ukrainski-dubbing/7667s2r3")
    filmUrls("Hopnięci")                                                      shouldBe Some("https://www.cinema-city.pl/filmy/hopnieci/7499d2r")
    filmUrls("Iron Maiden: Burning Ambition")                                 shouldBe Some("https://www.cinema-city.pl/filmy/iron-maiden-burning-ambition/8078s2r")
    filmUrls("Kicia Kocia w podróży")                                         shouldBe Some("https://www.cinema-city.pl/filmy/kicia-kocia-w-podrozy/7975o2r")
    filmUrls("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe Some("https://www.cinema-city.pl/filmy/kolekcja-mamoru-hosody-o-dziewczynie-skaczacej-przez-czas/8068s2r")
    filmUrls("Kurozając i świątynia świstaka")                                shouldBe Some("https://www.cinema-city.pl/filmy/kurozajac-i-swiatynia-swistaka/7921d2r")
    filmUrls("La Traviata Verdiego z Arena di Verona")                        shouldBe Some("https://www.cinema-city.pl/filmy/la-traviata-verdiego-z-arena-di-verona/8130s2r")
    filmUrls("Lars jest LOL")                                                 shouldBe Some("https://www.cinema-city.pl/filmy/lars-jest-lol/6547d2r")
    filmUrls("Liga Mistrzów UEFA - Finał")                                    shouldBe Some("https://www.cinema-city.pl/filmy/liga-mistrzow-uefa-final/8063o2r")
    filmUrls("Michael")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/michael/7785s2r")
    filmUrls("Mortal Kombat II")                                              shouldBe Some("https://www.cinema-city.pl/filmy/mortal-kombat-ii/7478s2r")
    filmUrls("Mortal Kombat II ukraiński dubbing")                            shouldBe Some("https://www.cinema-city.pl/filmy/mortal-kombat-ii-ukrainski-dubbing/7478s2r1")
    filmUrls("Narodziny gwiazdy")                                             shouldBe Some("https://www.cinema-city.pl/filmy/ladies-night-narodziny-gwiazdy/3026s2r1")
    filmUrls("Niesamowite przygody skarpetek 3. Ale ko")                      shouldBe Some("https://www.cinema-city.pl/filmy/niesamowite-przygody-skarpetek-3-ale-ko/8188d2r")
    filmUrls("Normal")                                                        shouldBe Some("https://www.cinema-city.pl/filmy/normal/8129s2r")
    filmUrls("Obsesja")                                                       shouldBe Some("https://www.cinema-city.pl/filmy/obsesja/8134s2r")
    filmUrls("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe Some("https://www.cinema-city.pl/filmy/odrodzony-jako-galareta-film-lzy-morza-lazurowego/8076s2r")
    filmUrls("Piep*zyć Mickiewicza 3")                                        shouldBe Some("https://www.cinema-city.pl/filmy/piepzyc-mickiewicza-3/7841o2r")
    filmUrls("Projekt Hail Mary")                                             shouldBe Some("https://www.cinema-city.pl/filmy/projekt-hail-mary/7449s2r")
    filmUrls("Pucio")                                                         shouldBe Some("https://www.cinema-city.pl/filmy/pucio/8099d2r")
    filmUrls("Sabotażysta 5")                                                 shouldBe Some("https://www.cinema-city.pl/filmy/sabotazysta-5/8186o2r")
    filmUrls("Sprawiedliwość owiec")                                          shouldBe Some("https://www.cinema-city.pl/filmy/sprawiedliwosc-owiec/7956d2r")
    filmUrls("Straszny film")                                                 shouldBe Some("https://www.cinema-city.pl/filmy/straszny-film/8117s2r")
    filmUrls("Super Mario Galaxy Film")                                       shouldBe Some("https://www.cinema-city.pl/filmy/super-mario-galaxy-film/7999d2r")
    filmUrls("The Amazing Digital Circus: The Last Act")                      shouldBe Some("https://www.cinema-city.pl/filmy/the-amazing-digital-circus-the-last-act/8150o2r")
    filmUrls("Top Gun 40th Anniversary")                                      shouldBe Some("https://www.cinema-city.pl/filmy/top-gun-40th-anniversary/8091s2r")
    filmUrls("Top Gun Maverick")                                              shouldBe Some("https://www.cinema-city.pl/filmy/top-gun-maverick/3639s2r")
    filmUrls("Władcy Wszechświata")                                           shouldBe Some("https://www.cinema-city.pl/filmy/wladcy-wszechswiata/7992s2r")
  }

  // ─── Plaza: showtime counts ───────────────────────────────────────────────

  it should "return correct showtime count for every film" in {
    val counts = plaza.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("90. urodziny Pavarottiego - Koncert gwiazd z Arena di Verona") shouldBe 2
    counts("Billie Eilish – Hit Me Hard and Soft: The Tour")          shouldBe 38
    counts("Diabeł ubiera się u Prady 2")                                  shouldBe 107
    counts("Diabeł ubiera się u Prady 2 ukraiński dubbing")                shouldBe 11
    counts("Drama")                                                         shouldBe 10
    counts("Drzewo magii")                                                  shouldBe 4
    counts("Gwiezdne Wojny: Mandalorian i Grogu")                           shouldBe 50
    counts("Gwiezdne Wojny: Mandalorian i Grogu - ukraiński dubbing")       shouldBe 3
    counts("Hopnięci")                                                      shouldBe 1
    counts("Iron Maiden: Burning Ambition")                                 shouldBe 7
    counts("Kicia Kocia w podróży")                                         shouldBe 2
    counts("Kolekcja Mamoru Hosody: O dziewczynie skaczącej przez czas")    shouldBe 2
    counts("Kurozając i świątynia świstaka")                                shouldBe 29
    counts("La Traviata Verdiego z Arena di Verona")                        shouldBe 2
    counts("Lars jest LOL")                                                 shouldBe 1
    counts("Liga Mistrzów UEFA - Finał")                                    shouldBe 1
    counts("Michael")                                                       shouldBe 29
    counts("Mortal Kombat II")                                              shouldBe 80
    counts("Mortal Kombat II ukraiński dubbing")                            shouldBe 9
    counts("Narodziny gwiazdy")                                             shouldBe 1
    counts("Niesamowite przygody skarpetek 3. Ale ko")                      shouldBe 1
    counts("Normal")                                                        shouldBe 9
    counts("Obsesja")                                                       shouldBe 14
    counts("Odrodzony jako galareta. Film: Łzy Morza Lazurowego")           shouldBe 2
    counts("Piep*zyć Mickiewicza 3")                                        shouldBe 1
    counts("Projekt Hail Mary")                                             shouldBe 11
    counts("Pucio")                                                         shouldBe 8
    counts("Sabotażysta 5")                                                 shouldBe 8
    counts("Sprawiedliwość owiec")                                          shouldBe 31
    counts("Straszny film")                                                 shouldBe 20
    counts("Super Mario Galaxy Film")                                       shouldBe 25
    counts("The Amazing Digital Circus: The Last Act")                      shouldBe 22
    counts("Top Gun 40th Anniversary")                                      shouldBe 12
    counts("Top Gun Maverick")                                              shouldBe 6
    counts("Władcy Wszechświata")                                           shouldBe 18
  }

  // ─── Plaza: full showtime details ─────────────────────────────────────────

  it should "return exact showtime for Liga Mistrzów UEFA - Finał" in {
    val st = byPlaza("Liga Mistrzów UEFA - Finał").showtimes
    st.size shouldBe 1
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 30, 17, 45), Some("https://tickets.cinema-city.pl/api/order/1326767?lang=pl"), Some("Sala 3"), List("2D")),
    )
  }

  it should "return exact showtime for Lars jest LOL" in {
    val st = byPlaza("Lars jest LOL").showtimes
    st.size shouldBe 1
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 13, 10, 0), Some("https://tickets.cinema-city.pl/api/order/1437816?lang=pl"), Some("Sala 2"), List("2D")),
    )
  }

  it should "extract 2D / 3D / IMAX format on Plaza showtimes" in {
    val formats = plaza.flatMap(_.showtimes).groupBy(_.format).view.mapValues(_.size).toMap
    formats(List("2D"))           shouldBe 484
    formats(List("3D"))           shouldBe 27
    formats(List("IMAX", "2D"))   shouldBe 49
    formats(List("IMAX", "3D"))   shouldBe 17
  }
}
