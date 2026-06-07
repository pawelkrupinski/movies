package clients.gap

import clients.tools.FakeHttpFetch
import models.{Cinema, CinemaMovie, HeliosOutletPark, KinoCkLublin, KinoWisla}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{Bilety24Client, HeliosClient, HeliosNuxt, NoveKinoClient}

import java.time.LocalDate

/**
 * The three "gap" cinemas that reuse an existing, already-tested client rather
 * than a bespoke one: Kino Wisła (NoveKino chain), Helios Outlet Park (the
 * second Szczecin Helios), and Kino CK Lublin (Bilety24-hosted). Each runs the
 * real client against a recorded response (RecordGapCinemas) and must return
 * films tagged with the right Cinema, each carrying showtimes.
 */
class GapReuseScrapeSpec extends AnyFlatSpec with Matchers {

  private val captureDate = LocalDate.of(2026, 6, 7)

  private def check(label: String, cinema: Cinema)(fetch: => Seq[CinemaMovie]): Unit =
    label should "return films tagged with its cinema, each carrying showtimes" in {
      val movies = fetch
      movies should not be empty
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      movies.foreach(_.showtimes should not be empty)
    }

  check("NoveKinoClient (Kino Wisła)", KinoWisla)(
    new NoveKinoClient(new FakeHttpFetch("kino-wisla"), "wisla", KinoWisla).fetch())

  check("HeliosClient (Helios Outlet Park)", HeliosOutletPark)(
    new HeliosClient(new FakeHttpFetch("helios-outlet-park"), HeliosNuxt.SzczecinOutletPark, captureDate).fetch())

  check("Bilety24Client (Kino CK Lublin)", KinoCkLublin)(
    new Bilety24Client(new FakeHttpFetch("kino-ck-lublin"), "https://ck-lublin.bilety24.pl", KinoCkLublin).fetch())
}
