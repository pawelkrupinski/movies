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
      KinoNoweKinoWarszawa, LocalDate.of(2026, 6, 10), "łowczynie demonów", LocalDateTime.of(2026, 6, 12, 17, 0))
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
}
