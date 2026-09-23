package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.cinemaData` — the ONE slot per venue that detail enrichment, the
 * published evidence and the display vote read. A venue listing the film several
 * ways must be represented by its least-decorated listing, not by whichever title
 * sorts last (Kino Sfinks's "Tani wtorek: …" promo).
 */
class MovieRecordCinemaDataSpec extends AnyFlatSpec with Matchers {

  private def slot(title: String, url: String, runtime: Int) =
    SourceData(title = Some(title), filmUrl = Some(url), runtimeMinutes = Some(runtime))

  private val sfinks = MovieRecord(data = Map[Source, SourceData](
    CinemaShowing(KinoSfinks, "robinhoodkonieclegendy")                          -> slot("Robin hood. Koniec legendy", "plain", 101),
    CinemaShowing(KinoSfinks, "filmowyklubsenioraiseniorkirobinhoodkonieclegendy") -> slot("Filmowy Klub Seniora i Seniorki: Robin hood. Koniec legendy", "senior", 130),
    CinemaShowing(KinoSfinks, "taniwtorekrobinhoodkonieclegendy")                -> slot("Tani wtorek: Robin hood. Koniec legendy", "promo", 120)
  ))

  "cinemaData" should "represent a venue by its least-decorated listing, not its alphabetically last" in {
    sfinks.cinemaData(KinoSfinks).filmUrl shouldBe Some("plain")
  }

  it should "read the venue's published evidence from that same listing" in {
    sfinks.evidence.runtimes shouldBe Seq(101)
  }

  it should "prefer the bare listing over one decorated on BOTH edges" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      CinemaShowing(KinoSfinks, "kinobezbarierojczyznaadcc") -> slot("Kino bez barier: Ojczyzna (AD + CC)", "access", 95),
      CinemaShowing(KinoSfinks, "ojczyzna")                  -> slot("Ojczyzna", "plain", 95)
    ))
    record.cinemaData(KinoSfinks).filmUrl shouldBe Some("plain")
  }

  it should "prefer the spelling most venues share when a venue's listings are equally bare" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      CinemaShowing(KinoSfinks, "diuna") -> slot("Diuna", "diuna", 155),
      CinemaShowing(KinoSfinks, "dune")  -> slot("Dune", "dune", 155),
      Multikino                          -> SourceData(title = Some("Diuna")),
      KinoApollo                         -> SourceData(title = Some("Dune")),
      CinemaCityWroclavia                -> SourceData(title = Some("Diuna"))
    ))
    record.cinemaData(KinoSfinks).filmUrl shouldBe Some("diuna")
  }
}
