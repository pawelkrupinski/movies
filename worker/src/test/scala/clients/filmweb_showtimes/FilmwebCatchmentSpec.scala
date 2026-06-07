package clients.filmweb_showtimes

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FilmwebShowtimesClient

import java.time.LocalDate

/**
 * Replays a recorded seances + title/info capture for every Filmweb-catchment
 * venue wired in `CinemaScraperCatalog.filmwebExtra`. Each is just a new
 * `FilmwebShowtimesClient` instance pointed at a Filmweb internal cinema id (the
 * client itself is parsing-tested in [[FilmwebShowtimesClientSpec]]); what's
 * fallible per venue is the id — a transcription slip wires the wrong cinema, or
 * a venue Filmweb lists but serves empty. This asserts each id yields its own
 * non-empty programme. Capture date is per-venue (the first in-window date the
 * venue had published seances when recorded, 2026-06); fixtures under
 * test/resources/fixtures/filmweb-catchment.
 */
class FilmwebCatchmentSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("filmweb-catchment")

  // (Filmweb cinema id, modelled cinema, capture date) — mirrors filmwebExtra.
  private val venues: Seq[(Int, Cinema, LocalDate)] = Seq(
    (117, KinoKawiarnia, LocalDate.of(2026, 6, 7)),
    (135, KinoSwitCzechowiceDziedzice, LocalDate.of(2026, 6, 15)),
    (157, KinoJanosik, LocalDate.of(2026, 6, 8)),
    (168, KinoBaltyk, LocalDate.of(2026, 6, 8)),
    (197, KinoMOKiS, LocalDate.of(2026, 6, 7)),
    (255, KinoBajkaDarlowo, LocalDate.of(2026, 6, 7)),
    (276, KinoWybrzeze, LocalDate.of(2026, 6, 10)),
    (285, KinoMetalowiec, LocalDate.of(2026, 6, 9)),
    (288, KinoMuzaLubin, LocalDate.of(2026, 6, 10)),
    (352, KinoPatria, LocalDate.of(2026, 6, 7)),
    (373, KinoForumBoleslawiec, LocalDate.of(2026, 6, 7)),
    (388, CinemaCity, LocalDate.of(2026, 6, 7)),
    (431, KinoStudio, LocalDate.of(2026, 6, 11)),
    (514, KinoCentrum, LocalDate.of(2026, 6, 7)),
    (561, KinoJaworzyna, LocalDate.of(2026, 6, 7)),
    (588, KinoNaStarowce, LocalDate.of(2026, 6, 7)),
    (619, KinoChemik, LocalDate.of(2026, 6, 8)),
    (1121, KinoPrzedwiosnieKrotoszyn, LocalDate.of(2026, 6, 8)),
    (1122, KinoArtKino, LocalDate.of(2026, 6, 7)),
    (1124, KinoKinomax, LocalDate.of(2026, 6, 11)),
    (1134, KinoKDK, LocalDate.of(2026, 6, 7)),
    (1137, KinoKlaps, LocalDate.of(2026, 6, 7)),
    (1139, KinoPCA, LocalDate.of(2026, 6, 7)),
    (1294, KinoRegis, LocalDate.of(2026, 6, 7)),
    (1420, HeliosLubin, LocalDate.of(2026, 6, 11)),
    (1430, KinoPionierZary, LocalDate.of(2026, 6, 7)),
    (1464, MultikinoRumia, LocalDate.of(2026, 6, 7)),
    (1477, KinoSokolBrzozow, LocalDate.of(2026, 6, 7)),
    (1480, KinoNaszeKino, LocalDate.of(2026, 6, 11)),
    (1481, KinoPlaneta, LocalDate.of(2026, 6, 11)),
    (1484, KinoEcho, LocalDate.of(2026, 6, 10)),
    (1488, KinoSokolDabrowaTarnowska, LocalDate.of(2026, 6, 8)),
    (1490, KinoWislaBrzeszcze, LocalDate.of(2026, 6, 7)),
    (1493, KinoMOKNowaRuda, LocalDate.of(2026, 6, 7)),
    (1494, KinoScenaKultura, LocalDate.of(2026, 6, 7)),
    (1500, KinoSniezka, LocalDate.of(2026, 6, 7)),
    (1513, KinoCentrum3D, LocalDate.of(2026, 6, 7)),
    (1525, KinoMOKCentrum, LocalDate.of(2026, 6, 7)),
    (1528, KinoPlanetCinema, LocalDate.of(2026, 6, 7)),
    (1530, KinoZbyszek, LocalDate.of(2026, 6, 12)),
    (1645, KinoDyskusyjnyKlubFilmowyPolitechnika, LocalDate.of(2026, 6, 11)),
    (1659, KinoSokolSokolka, LocalDate.of(2026, 6, 7)),
    (1672, KinoTwierdza, LocalDate.of(2026, 6, 7)),
    (1673, HeliosTczew, LocalDate.of(2026, 6, 11)),
    (1675, KinoCentrumBialogard, LocalDate.of(2026, 6, 7)),
    (1681, KinoKrapkowice, LocalDate.of(2026, 6, 7)),
    (1697, KinoLewart, LocalDate.of(2026, 6, 7)),
    (1702, KinoKalejdoskop, LocalDate.of(2026, 6, 7)),
    (1703, HeliosKedzierzynKozle, LocalDate.of(2026, 6, 10)),
    (1705, KinoCentrumSkarzyskoKamienna, LocalDate.of(2026, 6, 7)),
    (1707, KinoIkar, LocalDate.of(2026, 6, 9)),
    (1713, KinoKuznica, LocalDate.of(2026, 6, 7)),
    (1714, KinoDKFRumcajs, LocalDate.of(2026, 6, 8)),
    (1718, KinoAurum, LocalDate.of(2026, 6, 7)),
    (1719, KinoKarolinka, LocalDate.of(2026, 6, 7)),
    (1721, KinoWawel, LocalDate.of(2026, 6, 7)),
    (1732, KinoMDK, LocalDate.of(2026, 6, 8)),
    (1771, KinoZdroj, LocalDate.of(2026, 6, 7)),
    (1776, MultikinoCzechowiceDziedzice, LocalDate.of(2026, 6, 7)),
    (1786, KinoCentrum3DPrzemysl, LocalDate.of(2026, 6, 7)),
    (1798, KinoPowisle, LocalDate.of(2026, 6, 7)),
    (1845, HeliosStarachowice, LocalDate.of(2026, 6, 10)),
    (1864, KinoGoplana, LocalDate.of(2026, 6, 7)),
    (1913, KinoKozienickiDomKultury, LocalDate.of(2026, 6, 7)),
    (1941, KinoSCK, LocalDate.of(2026, 6, 7)),
    (1949, KinoZaRogiem, LocalDate.of(2026, 6, 10)),
    (1955, KinoEuropa, LocalDate.of(2026, 6, 7)),
    (2014, HeliosKrosno, LocalDate.of(2026, 6, 11)),
    (2100, KinoNaSzekspirowskim, LocalDate.of(2026, 6, 10)),
    (2118, KinoSDK, LocalDate.of(2026, 6, 11)),
    (2123, KinoFregata, LocalDate.of(2026, 6, 7)),
    (2128, KinoODEON, LocalDate.of(2026, 6, 7)),
    (2130, KinoPlanetariumCentrumNaukiKopernik, LocalDate.of(2026, 6, 11)),
    (2171, KinoMaxKino, LocalDate.of(2026, 6, 7)),
    (2172, KinoNaBiegunach, LocalDate.of(2026, 6, 7)),
    (2180, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha, LocalDate.of(2026, 6, 10)),
    (2313, KinoCyfroweKino, LocalDate.of(2026, 6, 7)),
    (2315, KinoFarys, LocalDate.of(2026, 6, 7)),
    (2320, KinoBajkaKluczbork, LocalDate.of(2026, 6, 7)),
    (2326, HeliosZory, LocalDate.of(2026, 6, 11)),
    (2328, KinoAstra, LocalDate.of(2026, 6, 7)),
    (2331, KinoSDKSwiebodzin, LocalDate.of(2026, 6, 7)),
    (2335, KinoJednosc, LocalDate.of(2026, 6, 7)),
    (2341, KinoJutrzenka, LocalDate.of(2026, 6, 7)),
    (2342, KinoSwitZwolen, LocalDate.of(2026, 6, 7)),
    (2343, KinoDiana, LocalDate.of(2026, 6, 7)),
    (2344, KinoMCK, LocalDate.of(2026, 6, 8)),
    (2346, KinoWarszawa, LocalDate.of(2026, 6, 7)),
    (2350, KinoZacisze, LocalDate.of(2026, 6, 7)),
    (2351, KinoCK, LocalDate.of(2026, 6, 8)),
    (2352, KinoBaszta, LocalDate.of(2026, 6, 7)),
    (2354, KinoIgnacy, LocalDate.of(2026, 6, 7)),
    (2355, KinoNarie, LocalDate.of(2026, 6, 7)),
    (2357, KinoCinemaLumiere, LocalDate.of(2026, 6, 7)),
    (2359, KinoPiastOstrzeszow, LocalDate.of(2026, 6, 7)),
    (2363, KinoPDK, LocalDate.of(2026, 6, 7)),
    (2365, KinoDK, LocalDate.of(2026, 6, 7)),
    (2372, HeliosOstrowWlkp, LocalDate.of(2026, 6, 10)),
    (2403, KinoSpojnia, LocalDate.of(2026, 6, 7)),
    (2404, KinoKolory, LocalDate.of(2026, 6, 7)),
    (2410, KinoSleza, LocalDate.of(2026, 6, 7)),
    (2411, KinoGCK, LocalDate.of(2026, 6, 7)),
    (2414, KinoGOK, LocalDate.of(2026, 6, 7)),
    (2419, KinoPromien, LocalDate.of(2026, 6, 7)),
    (2443, KinoStaryMlyn, LocalDate.of(2026, 6, 7)),
    (2990, MultikinoKlodzko, LocalDate.of(2026, 6, 7)),
    (2993, MultikinoSwidnica, LocalDate.of(2026, 6, 7)),
    (3064, KinoGornik, LocalDate.of(2026, 6, 7)),
    (3119, KinoMiejskieCentrumKultury, LocalDate.of(2026, 6, 7)),
    (3121, KinoRondo, LocalDate.of(2026, 6, 7)),
    (3128, KinoKoneckieCentrumKultury, LocalDate.of(2026, 6, 7)),
    (3130, KinoNawojka, LocalDate.of(2026, 6, 7)),
    (3131, KinoZulawskiOsrodekKultury, LocalDate.of(2026, 6, 9)),
    (3141, KinoTeatrElektryczny, LocalDate.of(2026, 6, 10)),
    (3148, KinoPegaz, LocalDate.of(2026, 6, 7)),
    (3246, KinoNoweKinoWarszawa, LocalDate.of(2026, 6, 7)),
    (3248, KinoPckulKino, LocalDate.of(2026, 6, 7))
  )

  venues.foreach { case (id, cinema, date) =>
    s"${cinema.displayName} (Filmweb id $id)" should "resolve to its own non-empty programme" in {
      val client = new FilmwebShowtimesClient(http, id, cinema, daysAhead = 0, today = date)
      val movies = client.fetch()

      withClue(s"${cinema.displayName} returned no films for $date — wrong id or empty venue: ") {
        movies should not be empty
      }
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      all(movies.map(_.movie.title.trim)) should not be empty
      all(movies.map(_.externalIds.keySet)) should contain("filmweb")
      movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet shouldBe Set(date)
    }
  }
}
