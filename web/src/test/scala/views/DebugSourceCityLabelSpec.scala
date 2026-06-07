package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import models.{Helios, HeliosKonin, MovieRecord, Showtime, Source, SourceData}

/**
 * The debug source-data view lists one slot per cinema, labelled by the
 * cinema's display name. Many venues share a chain display name ("Helios")
 * across cities, so the slot also shows the cinema's city to disambiguate
 * which venue fed which raw values. Locks that the city rides next to the
 * name in the rendered HTML.
 */
class DebugSourceCityLabelSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.now()

  private def recordFrom(cinema: models.Cinema): MovieRecord =
    MovieRecord(
      imdbId = Some("tt0000001"),
      data = Map[Source, SourceData](
        cinema -> SourceData(
          title     = Some("My Film"),
          showtimes = Seq(Showtime(now.plusHours(2), None, None, Nil))
        )
      )
    )

  private def render(cinema: models.Cinema): String =
    views.html.debugDetails("My Film", Some(2024), recordFrom(cinema)).body

  "debugDetails source slot" should "show the cinema name with its city" in {
    render(HeliosKonin) should include ("Helios Konin · Konin")
  }

  it should "disambiguate venues whose display name omits the city" in {
    // "Helios Posnania" carries no city word, so the appended city is what
    // tells you this slot is the Poznań venue.
    render(Helios) should include ("Helios Posnania · Poznań")
  }
}
