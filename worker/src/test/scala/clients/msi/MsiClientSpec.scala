package clients.msi

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import services.cinemas.MsiClient

import java.time.{LocalDate, LocalDateTime}

/** One spec for every cinema on the shared MSI ticketing platform. Each row
 *  replays that venue's recorded `<base>/MSI/mvc/pl?sort=Name&date=2026-06`
 *  month page through `MsiClient` and pins a concrete screening, proving the
 *  one generic client serves all of them off nothing but a base URL.
 *
 *  `today` is pinned into June 2026 so the client fetches the recorded June
 *  fixture and then attempts July 2026 — which has no fixture, so the
 *  `FileNotFoundException` is swallowed and treated as an empty month (the same
 *  contract `RealHttpFetch` gives for a month with no screenings).
 *
 *  The four municipal venues (GOK Tychowo, MOK Nowa Ruda, Kino Warszawa
 *  Przeworsk, Kino Powiśle Sztum) were previously scraped from Filmweb, whose
 *  API had silently gone empty for them — every poll returned `[]` and the
 *  cinema showed zero showtimes. These fixtures are the proof the data is real
 *  and reachable on each venue's own MSI portal. */
class MsiClientSpec
    extends AnyFlatSpec
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  // (label, fixtureDir, baseUrl, cinema, today, pinned title substring (lower-cased), pinned showtime)
  private val venues = Table(
    ("label", "dir", "baseUrl", "cinema", "today", "title", "when"),
    ("Cinema1 Gdańsk", "cinema1", "https://bilety.cinemaone.pl",
      Cinema1Gdansk: Cinema, LocalDate.of(2026, 6, 7), "90.", LocalDateTime.of(2026, 6, 9, 19, 0)),
    ("Kino GOK Tychowo", "kino-gok", "https://bilety.goktychowo.pl",
      KinoGOK, LocalDate.of(2026, 6, 9), "toy story 5", LocalDateTime.of(2026, 6, 19, 15, 0)),
    ("Kino MOK Nowa Ruda", "kino-mok-nowa-ruda", "https://bilety.nowaruda.pl",
      KinoMOKNowaRuda, LocalDate.of(2026, 6, 9), "straszny film", LocalDateTime.of(2026, 6, 12, 19, 0)),
    ("Kino Warszawa Przeworsk", "kino-warszawa-przeworsk", "https://bilety-kino.przeworsk.um.gov.pl",
      KinoWarszawa, LocalDate.of(2026, 6, 9), "drzewo magii", LocalDateTime.of(2026, 6, 12, 18, 0)),
    ("Kino Powiśle Sztum", "kino-powisle-sztum", "https://kinosztumbilety.pl",
      KinoPowisle, LocalDate.of(2026, 6, 9), "dobra siostra", LocalDateTime.of(2026, 6, 19, 18, 0)),
    ("Kino Centrum Skarżysko", "kino-centrum-skarzysko", "https://bilet-mck.skarzysko.pl",
      KinoCentrumSkarzyskoKamienna, LocalDate.of(2026, 6, 10), "łowczynie demonów", LocalDateTime.of(2026, 6, 12, 16, 15)),
    ("Nowe Kino Warszawa (Gostynin)", "nowe-kino-warszawa", "https://bilety.mck-gostynin.pl",
      KinoNoweKinoWarszawa, LocalDate.of(2026, 6, 10), "łowczynie demonów", LocalDateTime.of(2026, 6, 12, 17, 0)),
    // Kino ODEON (Sochaczew) runs on an Eurobilet backend that serves the
    // identical MSI month page — so the generic client reaches it off a base URL
    // too. Previously scraped from Filmweb, which had silently gone empty.
    ("Kino ODEON (Sochaczew)", "kino-odeon", "https://kinoodeon.eurobilet.pl",
      KinoODEON, LocalDate.of(2026, 6, 8), "drzewo magii", LocalDateTime.of(2026, 6, 11, 10, 25))
  )

  forAll(venues) { (label, dir, baseUrl, cinema, today, titleSub, when) =>
    lazy val movies =
      new MsiClient(new FakeHttpFetch(dir), baseUrl, cinema, today = today).fetch()

    it should s"return a non-empty, single-cinema film list — $label" in {
      movies should not be empty
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      all(movies.map(_.showtimes)) should not be empty
    }

    it should s"pin a concrete screening — $label" in {
      val film = movies.find(_.movie.title.toLowerCase.contains(titleSub)).value
      film.showtimes.map(_.dateTime) should contain(when)
    }
  }

  it should "strip MSI format suffixes and sentence-case titles" in {
    // Raw title in the cinema1 fixture: "BACKROOMS. BEZ WYJŚCIA (2D NAPISY)"
    val movies =
      new MsiClient(new FakeHttpFetch("cinema1"), "https://bilety.cinemaone.pl",
        Cinema1Gdansk, today = LocalDate.of(2026, 6, 7)).fetch()
    val found = movies.find(_.movie.title.startsWith("Backrooms")).value
    found.movie.title should not include "(2D"
    found.movie.title should not include "(3D"
  }

  it should "lift the MSI title's buried format/version into Showtime.format" in {
    // Raw cinema1 title "BACKROOMS. BEZ WYJŚCIA (2D NAPISY)" — the cleaned title
    // collapses the variants, while the version is recovered as format tokens.
    val movies =
      new MsiClient(new FakeHttpFetch("cinema1"), "https://bilety.cinemaone.pl",
        Cinema1Gdansk, today = LocalDate.of(2026, 6, 7)).fetch()
    val backrooms = movies.find(_.movie.title.startsWith("Backrooms")).value
    backrooms.movie.title should not include "NAPISY"
    all(backrooms.showtimes.map(_.format)) shouldBe List("2D", "NAP")
  }

  it should "honour a non-default mvcPath (Kino Planeta serves the page at /Rezerwacja/mvc/pl)" in {
    val movies = new MsiClient(new FakeHttpFetch("kino-planeta"), "https://rezerwacja.planetabrzesko.pl",
      KinoPlaneta, today = LocalDate.of(2026, 6, 10), mvcPath = "/Rezerwacja/mvc/pl").fetch()
    movies should not be empty
    val film = movies.find(_.movie.title.toLowerCase.contains("drzewo magii")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 11, 13, 50))
  }

  // bilety.mok.com.pl hosts two cinemas (Chemik + Twierdza) on one MSI portal,
  // disambiguating by prefixing every title with "Chemik - " / "TWIERDZA - ".
  // `titlePrefix` splits that one feed into a per-cinema scraper: each instance
  // keeps only its own prefixed titles and strips the prefix. today=2026-06-08
  // mirrors prod — the June page is empty, the July page carries the screenings.
  private def mokKedzierzyn(cinema: Cinema, prefix: String) =
    new MsiClient(new FakeHttpFetch("kino-mok-kedzierzyn"), "https://bilety.mok.com.pl",
      cinema, today = LocalDate.of(2026, 6, 8), titlePrefix = Some(prefix)).fetch()

  it should "split the shared MOK portal into Chemik's own feed via titlePrefix" in {
    val movies = mokKedzierzyn(KinoChemik, "Chemik")
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoChemik)
    val titles = movies.map(_.movie.title.toLowerCase)
    // A Chemik-only film is present; a Twierdza-only film ("Zawodowcy") is not.
    titles.exists(_.contains("mandalorian and grogu")) shouldBe true
    titles.exists(_.contains("zawodowcy")) shouldBe false
    // The venue prefix is stripped from the cleaned titles.
    all(movies.map(_.movie.title.toLowerCase)) should not include "chemik -"
  }

  it should "split the shared MOK portal into Twierdza's own feed via titlePrefix" in {
    val movies = mokKedzierzyn(KinoTwierdza, "TWIERDZA")
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoTwierdza)
    val titles = movies.map(_.movie.title.toLowerCase)
    // A Twierdza-only film is present; a Chemik-only film is not.
    titles.exists(_.contains("zawodowcy")) shouldBe true
    titles.exists(_.contains("mandalorian and grogu")) shouldBe false
    all(movies.map(_.movie.title.toLowerCase)) should not include "twierdza -"
  }

  it should "give the two MOK venues overlapping films but distinct cinema tags" in {
    // "Toy Story 5" screens at both halls — the split is by venue prefix, not by
    // film, so the shared title must surface under BOTH cinemas, tagged apart.
    val chemik   = mokKedzierzyn(KinoChemik, "Chemik")
    val twierdza = mokKedzierzyn(KinoTwierdza, "TWIERDZA")
    chemik.exists(_.movie.title.toLowerCase.contains("toy story 5")) shouldBe true
    twierdza.exists(_.movie.title.toLowerCase.contains("toy story 5")) shouldBe true
  }
}
