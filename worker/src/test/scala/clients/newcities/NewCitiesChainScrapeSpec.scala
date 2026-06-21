package clients.newcities

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas._

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
 *
 * Exception: the Helios Radom + Sosnowiec captures landed mid-redirect (helios.pl
 * 302'd their /repertuar to the films-less homepage), so they assert the degraded
 * behaviour — `fetch` returns nothing — instead; see the Helios block below.
 */
class NewCitiesChainScrapeSpec extends AnyFlatSpec with Matchers {

  private val http = new FakeHttpFetch("new-cities")
  private val cc   = new CinemaCityClient(http)

  // Helios bakes the capture date into its REST URLs; pin replay to the date
  // RecordNewCities ran so the fixtures still match.
  private val captureDate = LocalDate.of(2026, 6, 7)

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

  // ── Helios venues (today pinned to the fixture capture date) ─────────────────
  // Kielce + Rzeszów were captured with a healthy NUXT repertoire page → the full
  // "wiring returns films" check.
  Seq(HeliosNuxt.Kielce, HeliosNuxt.Rzeszow)
    .foreach { config =>
      check(s"HeliosClient (${config.cinema.displayName})", config.cinema)(new HeliosClient(http, config, captureDate).fetch())
    }

  // Radom + Sosnowiec were captured mid-redirect: helios.pl 302s their /repertuar
  // to the films-less homepage (server-side, intermittent, per-venue — at capture
  // time these two were degraded while Kielce/Rzeszów were healthy), so the
  // recorded NUXT page carries no repertoire. HeliosClient now treats a film-less
  // NUXT page as a degraded scrape and returns nothing — rather than falling
  // through to the REST `/screening` subset, a non-empty-but-truncated list that
  // slipped past MovieCache's empty-scrape guard and pruned the venue's whole
  // repertoire (the per-city served-films "flap"). These recorded redirects are
  // the real-world regression for that fix; see HeliosClientResilienceSpec for the
  // isolated case. If these fixtures are ever re-recorded healthy, move the venues
  // back up to the `check` block above.
  Seq(HeliosNuxt.Radom, HeliosNuxt.Sosnowiec).foreach { config =>
    s"HeliosClient (${config.cinema.displayName}, recorded NUXT redirect)" should
      "return nothing rather than the REST-only backfill when the NUXT page is degraded" in {
        new HeliosClient(http, config, captureDate).fetch() shouldBe empty
      }
  }
}
