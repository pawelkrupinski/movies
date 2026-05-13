package clients

import clients.tools.FakeHttpFetch
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Showtime.format is a List of individual tokens. Cinemas use different
// separators in their raw payloads ("/", " "), so we strip them at parse time
// and let the view layer join with whatever it likes (currently a space).
// This spec runs every client against its fixture and asserts that no single
// token contains a slash — i.e. format elements are pre-split, never glued.
class FormatTokensSpec extends AnyFlatSpec with Matchers {

  private def allFormatTokens(ms: Seq[CinemaMovie]): Seq[String] =
    ms.flatMap(_.showtimes).flatMap(_.format)

  private val clients: Seq[(String, () => Seq[CinemaMovie])] = Seq(
    "KinoBulgarska" -> (() => new KinoBulgarskaClient(new FakeHttpFetch("kino-bulgarska")).fetch()),
    "KinoMuza"      -> (() => new KinoMuzaClient(new FakeHttpFetch("kino-muza")).fetch()),
    "KinoPalacowe"  -> (() => new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe")).fetch()),
    "CharlieMonroe" -> (() => new CharlieMonroeClient(new FakeHttpFetch("charlie-monroe")).fetch()),
    "Multikino"     -> (() => new MultikinoClient(new FakeHttpFetch("multikino")).fetch()),
    "Rialto"        -> (() => new RialtoClient(new FakeHttpFetch("rialto")).fetch()),
    "CC Kinepolis"  -> (() => new CinemaCityClient(new FakeHttpFetch("cinema-city-kinepolis")).fetch("1081", CinemaCityKinepolis)),
    "CC Plaza"      -> (() => new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza")).fetch("1078", CinemaCityPoznanPlaza)),
    "Helios"        -> (() => new HeliosClient(new FakeHttpFetch("helios/rest-enrichment")).fetch()),
  )

  for ((name, run) <- clients) {
    s"$name showtime formats" should "be split into individual tokens (no slash inside any token)" in {
      val tokens = allFormatTokens(run())
      withClue(s"$name had glued tokens: ${tokens.filter(_.contains("/"))}: ") {
        tokens.foreach(_ should not include "/")
      }
    }
  }

  // ── Cross-cinema sample (using known fixture data) ────────────────────────

  it should "produce 'IMAX 2D NAP' when Plaza's IMAX 2D + subtitled screening is space-joined" in {
    val plaza  = new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza")).fetch("1078", CinemaCityPoznanPlaza)
    val imax2D = plaza.flatMap(_.showtimes).find(_.format == List("IMAX", "2D", "NAP"))
    imax2D                              shouldBe defined
    imax2D.get.format.mkString(" ")     shouldBe "IMAX 2D NAP"
  }

  it should "produce 'IMAX 3D NAP' / 'IMAX 3D DUB' for Plaza's IMAX 3D screenings" in {
    val plaza  = new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza")).fetch("1078", CinemaCityPoznanPlaza)
    val imax3D = plaza.flatMap(_.showtimes).filter(_.format.startsWith(List("IMAX", "3D")))
    imax3D                              should not be empty
    imax3D.map(_.format.mkString(" ")).toSet shouldBe Set("IMAX 3D NAP", "IMAX 3D DUB")
  }

  it should "produce '2D NAP ATMOS' when Helios joins multi-token formats for display" in {
    val helios = new HeliosClient(new FakeHttpFetch("helios/rest-enrichment")).fetch()
    val triple = helios.flatMap(_.showtimes).find(_.format == List("2D", "NAP", "ATMOS"))
    triple                              shouldBe defined
    triple.get.format.mkString(" ")     shouldBe "2D NAP ATMOS"
  }
}
