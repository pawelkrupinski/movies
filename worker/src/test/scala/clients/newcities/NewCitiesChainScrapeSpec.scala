package clients.newcities

import models._
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas._
import services.cinemas.pl.{CinemaCityClient, HeliosClient, HeliosNuxt, MultikinoClient}

import java.time.LocalDate

/**
 * Replays the recorded `new-cities` corpus (see `clients.tools.RecordNewCities`)
 * through the shared chain clients for every newly-wired venue in the eight
 * smaller cities (Częstochowa, Radom, Sosnowiec, Toruń, Kielce, Rzeszów,
 * Gliwice, Zabrze). Each venue runs the real Cinema City / Multikino / Helios
 * parser against a real captured response and must return films tagged with the
 * right `Cinema`, each carrying at least one showtime — the fail-before/pass-
 * after gate for wiring these cinemas (a wrong cinema id, Helios UUID/slug makes
 * its case return empty and fail).
 */
class NewCitiesChainScrapeSpec extends AnyFlatSpec with Matchers {

  private val http = new FakeHttpFetch("new-cities")
  private val cc   = new CinemaCityClient(http)

  // Helios bakes the capture date into its REST URLs; pin replay to the date
  // RecordNewCities ran so the fixtures still match.
  private val captureDate = LocalDate.of(2026, 6, 7)
  // Radom + Sosnowiec were re-recorded later, after helios.pl renamed their page
  // slug to the chain-default `kino-helios` (the old `kino-helios-<city>` URLs now
  // 302 to the homepage); their fixtures carry this later capture date.
  private val captureDateRadomSosnowiec = LocalDate.of(2026, 6, 21)

  private def check(label: String, cinema: Cinema)(fetch: => Seq[CinemaMovie]): Unit =
    label should "return films tagged with its cinema, each carrying showtimes" in {
      val movies = fetch
      movies should not be empty
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      movies.foreach(_.showtimes should not be empty)
    }

  // ── Cinema City venues ──────────────────────────────────────────────────────
  Seq(
    "1089" -> CinemaCityCzestochowaJurajska, "1075" -> CinemaCityCzestochowaWolnosc,
    "1083" -> CinemaCitySosnowiec,
    "1077" -> CinemaCityTorunCzerwonaDroga, "1093" -> CinemaCityTorunPlaza,
    "1085" -> CinemaCityGliwice,
  ).foreach { case (id, c) =>
    check(s"CinemaCityClient (${c.displayName})", c)(cc.fetch(id, c))
  }

  // ── Multikino venues ────────────────────────────────────────────────────────
  Seq(
    "0026" -> MultikinoRadom, "0029" -> MultikinoKielce,
    "0028" -> MultikinoRzeszow, "0003" -> MultikinoZabrze,
  ).foreach { case (id, c) =>
    check(s"MultikinoClient (${c.displayName})", c)(new MultikinoClient(http, id, c).fetch())
  }

  // ── Helios venues (today pinned to each venue's fixture capture date) ─────────
  Seq(HeliosNuxt.Kielce, HeliosNuxt.Rzeszow)
    .foreach { config =>
      check(s"HeliosClient (${config.cinema.displayName})", config.cinema)(new HeliosClient(http, config, captureDate).fetch())
    }
  // Radom + Sosnowiec: re-recorded under the renamed `kino-helios` slug.
  Seq(HeliosNuxt.Radom, HeliosNuxt.Sosnowiec)
    .foreach { config =>
      check(s"HeliosClient (${config.cinema.displayName})", config.cinema)(new HeliosClient(http, config, captureDateRadomSosnowiec).fetch())
    }
}
