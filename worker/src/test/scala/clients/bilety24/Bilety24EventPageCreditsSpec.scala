package clients.bilety24

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.FilmDetail
import services.cinemas.pl.{Bilety24Client, Bilety24OrganizerClient}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Each bilety24 venue types its own film facts into its event page's description box — some a
 *  "reżyseria: … / występują: …" block, some one "reż. … | kraj rok | N min" credit line, most
 *  nothing at all. Replays real event pages (recorded 2026-10-06 from the URLs beside each case)
 *  from several venues through the organiser client's detail fetch: the facts a venue states reach
 *  the detail, a venue that states none yields none, and a page billing several films lends none of
 *  their facts to the block. */
class Bilety24EventPageCreditsSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def detailOf(cinema: Cinema, page: String): FilmDetail =
    new Bilety24OrganizerClient(new FakeHttpFetch("bilety24-detail"),
      "https://www.bilety24.pl/kino/organizator/x-1", cinema, titles = titleNormalizer)
      .fetchFilmDetail(s"https://www.bilety24.pl/kino/$page?id=1").value

  "A bilety24 event page" should "give the festival screening's own credit line — ARS, Kino Światowid" in {
    // https://www.bilety24.pl/kino/1503-ars-independent-festival-konkurs-czarny-kon-filmu-gradiva-164478?id=987256
    // "GRADIVA | LA GRADIVA reż. Marine Atlan | Francja, Włochy 2026 | 145 min"
    val detail = detailOf(KinoSwiatowid, "1503-ars-independent-festival-konkurs-czarny-kon-filmu-gradiva-164478")
    detail.director shouldBe Seq("Marine Atlan")
    detail.countries shouldBe Seq("Francja", "Włochy")
    detail.releaseYear.value shouldBe 2026
    detail.runtimeMinutes.value shouldBe 145
    detail.posterUrl.value shouldBe "https://image.bilety24.pl/original/dealer-default/1503/gradiva.jpg"
  }

  it should "read a labelled block and its production line — Kino 60 Krzeseł" in {
    // https://www.bilety24.pl/kino/776-crash-pokaz-w-dkf-megaron--164901?id=991024
    val detail = detailOf(Kino60Krzesel, "776-crash-pokaz-w-dkf-megaron--164901")
    detail.director shouldBe Seq("David Cronenberg")
    detail.cast shouldBe Seq("James Spader", "Holly Hunter", "Elias Koteas")
    detail.countries shouldBe Seq("Kanada", "Wielka Brytania")
    detail.releaseYear.value shouldBe 1996
    detail.runtimeMinutes.value shouldBe 100
  }

  it should "read the original title and the country-and-year label — MCK Grybów" in {
    // https://www.bilety24.pl/kino/1321-everest-druga-strona-165815?id=995791
    val detail = detailOf(KinoMCKGrybow, "1321-everest-druga-strona-165815")
    detail.originalTitle.value shouldBe "Everest: The Other Side"
    detail.countries shouldBe Seq("USA")
    detail.releaseYear.value shouldBe 2026
    detail.runtimeMinutes.value shouldBe 115
  }

  it should "read director, cast and original title — Kino Hel, Pleszew" in {
    // https://www.bilety24.pl/kino/1255-asterix-i-obelix-misja-kleopatra-2d-dubbing-166003?id=997109
    val detail = detailOf(KinoHel, "1255-asterix-i-obelix-misja-kleopatra-2d-dubbing-166003")
    detail.director shouldBe Seq("Alain Chabat")
    detail.cast shouldBe Seq("Monica Bellucci", "Christian Clavier", "Gérard Depardieu", "Jamel Debbouze")
    detail.originalTitle.value shouldBe "Astérix & Obélix: Mission Cléopâtre"
  }

  it should "read 'produkcja: Polska 2026' as country and year — MDK Radomsko" in {
    // https://www.bilety24.pl/kino/1546-lalka-2d-165452?id=993044
    val detail = detailOf(KinoMDK, "1546-lalka-2d-165452")
    detail.director shouldBe Seq("Maciej Kawalski")
    detail.countries shouldBe Seq("Polska")
    detail.releaseYear.value shouldBe 2026
    detail.runtimeMinutes.value shouldBe 162
  }

  it should "read a 'Prod.' line — Kino Jantar, Ostrołęka" in {
    // https://www.bilety24.pl/kino/1068-folwark-zwierzecy-2d-dub-165688?id=995875
    // "Prod. Kanada/USA/Wlk. Brytania 2026, animacja, 95 min"
    val detail = detailOf(KinoJantar, "1068-folwark-zwierzecy-2d-dub-165688")
    detail.countries shouldBe Seq("Kanada", "USA", "Wlk. Brytania")
    detail.releaseYear.value shouldBe 2026
    detail.runtimeMinutes.value shouldBe 95
  }

  it should "read a pipe-joined labelled run, and never a premiere date as the year — Kino Centrum, Brzeg" in {
    // https://www.bilety24.pl/kino/1672-bez-konca-165520?id=993651
    // "BEZ KOŃCA | WERSJA POLSKA | PREMIERA: 11.09.2026 | CZAS TRWANIA: 108 minut | … | PRODUKCJA: POLSKA, FRANCJA"
    val detail = detailOf(KinoCentrumBrzeg, "1672-bez-konca-165520")
    detail.runtimeMinutes.value shouldBe 108
    detail.countries shouldBe Seq("POLSKA", "FRANCJA")
    detail.releaseYear shouldBe None
  }

  it should "state nothing of a block billing two films, each with its own credit line — Kino Janosik" in {
    // https://www.bilety24.pl/kino/1500-19-fga-blok-filmowy-linia-logiczna-bartne-filmy-konkursowe-164682?id=991060
    val detail = detailOf(KinoJanosik, "1500-19-fga-blok-filmowy-linia-logiczna-bartne-filmy-konkursowe-164682")
    detail.director shouldBe empty
    detail.countries shouldBe empty
    detail.releaseYear shouldBe None
    detail.runtimeMinutes shouldBe None
  }

  it should "state nothing, without failing, when the venue typed only a synopsis — Kino Bałtyk" in {
    // https://www.bilety24.pl/kino/1499-baranek-shaun-i-kudlata-bestia-166285?id=1000101
    val detail = detailOf(KinoBaltyk, "1499-baranek-shaun-i-kudlata-bestia-166285")
    detail.synopsis.value should include("W wieczór Halloween")
    detail.copy(synopsis = None, posterUrl = None) shouldBe FilmDetail()
  }

  it should "take no poster from bilety24's image placeholder — MOK Centrum, Zawiercie" in {
    // https://www.bilety24.pl/kino/1305-bella-w-brzuszku-posluchaj-tego-165595?id=993989
    // og:image is "https://image.bilety24.pl/not-found"
    detailOf(KinoMOKCentrum, "1305-bella-w-brzuszku-posluchaj-tego-165595").posterUrl shouldBe None
  }

  // ── The legacy per-venue subdomains' /wydarzenie/?id=N pages: the same free text ──

  private def subdomainEvent(file: String, cinema: Cinema, baseUrl: String): CinemaMovie = {
    val html = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(file)), "UTF-8")
    Bilety24Client.parseEvent(html, cinema, baseUrl, "1", titleNormalizer).value
  }

  "A bilety24 subdomain event page" should "carry its labelled credits onto the listing — Kinoteatr Rialto" in {
    // Trixie: "Reżyseria: Bastien Genoux / Występuje: Beatrice Cordua / Produkcja: Detours Film /
    // Miejsce produkcji: Szwajcaria" — a studio under "Produkcja" is no country.
    val film = subdomainEvent("test/resources/fixtures/08-06-2026/kinoteatrrialto.bilety24.pl/wydarzenie/.55846d8a",
      KinoteatrRialto, "https://kinoteatrrialto.bilety24.pl")
    film.director shouldBe Seq("Bastien Genoux")
    film.cast shouldBe Seq("Beatrice Cordua")
    film.movie.countries shouldBe Seq("Szwajcaria")
  }

  "A bilety24 organiser" should "wait for the event page, since the page can carry identity facts" in {
    new Bilety24OrganizerClient(new FakeHttpFetch("bilety24-detail"), "https://www.bilety24.pl/kino/organizator/x-1",
      KinoBaltyk, titles = titleNormalizer).defersTmdbResolution shouldBe true
  }
}
