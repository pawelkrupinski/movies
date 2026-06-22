package controllers

import models.{Helios, HeliosMagnolia, MovieRecord, Poznan, Showtime, Source, SourceData, Tmdb, Wroclaw}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.time.LocalDateTime

/** A film's displayed synopsis is city-scoped: each city's page shows the best
 *  blurb among the cinemas screening it IN THAT CITY plus TMDB, never a cinema
 *  in another city. Guards that `toSchedules` resolves via
 *  `ResolvedMovie.synopsisFor(city)` rather than the city-independent
 *  `synopsis` — a regression to the latter would show every city the same
 *  (other-city or TMDB) blurb and fail here. `Helios` is a Poznań venue,
 *  `Helios Magnolia` a Wrocław one (see `Cinema.byCity`). */
class CitySynopsisSpec extends AnyFlatSpec with Matchers {

  private val showAt = LocalDateTime.of(2026, 6, 8, 18, 0)
  private val now    = LocalDateTime.of(2026, 6, 8, 0, 0)

  private def slot(synopsis: String): SourceData =
    SourceData(title = Some("Dwa Miasta"), synopsis = Some(synopsis),
               showtimes = Seq(Showtime(showAt, bookingUrl = None)))

  private def screensNoBlurb: SourceData =
    SourceData(title = Some("Dwa Miasta"), showtimes = Seq(Showtime(showAt, bookingUrl = None)))

  "toSchedules" should "show each city its own cinema's synopsis, not another city's" in {
    val poznanText  = "Poznański opis z kina, akapit pierwszy.\n\nAkapit drugi."
    val wroclawText = "Wrocławski opis z kina, akapit pierwszy.\n\nAkapit drugi."
    val record = MovieRecord(data = Map[Source, SourceData](
      Helios         -> slot(poznanText),
      HeliosMagnolia -> slot(wroclawText),
      Tmdb           -> SourceData(title = Some("Dwa Miasta"), synopsis = Some("Krótki opis z TMDB."))
    ))
    val service = new MovieControllerService(
      TestReadModel.fromRecords(Seq(("Dwa Miasta", Some(2026), record))))

    service.toSchedules(Poznan, now).head.synopsis.get  should (include ("Poznański")  and not include ("Wrocławski"))
    service.toSchedules(Wroclaw, now).head.synopsis.get should (include ("Wrocławski") and not include ("Poznański"))
  }

  it should "fall back to TMDB in a city whose cinemas carry no blurb of their own" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Helios         -> slot("Poznański opis z kina."), // Poznań cinema has its own blurb…
      HeliosMagnolia -> screensNoBlurb,                 // …Wrocław screens it but supplies none
      Tmdb           -> SourceData(title = Some("Dwa Miasta"), synopsis = Some("Opis z TMDB."))
    ))
    val service = new MovieControllerService(
      TestReadModel.fromRecords(Seq(("Dwa Miasta", Some(2026), record))))

    service.toSchedules(Wroclaw, now).head.synopsis shouldBe Some("Opis z TMDB.")
    service.toSchedules(Poznan, now).head.synopsis.get should include ("Poznański")
  }
}
