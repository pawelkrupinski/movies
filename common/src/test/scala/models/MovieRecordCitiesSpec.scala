package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.cities` — the cities whose cinemas currently screen this film,
 * in `City.all` order. The debug page deep-links each (global-corpus) row into
 * the first of these so the city-scoped /film page resolves instead of 404ing.
 */
class MovieRecordCitiesSpec extends AnyFlatSpec with Matchers {

  "cities" should "list only cities whose cinemas screen the film" in {
    val record = MovieRecord(data = Map[Source, SourceData](CinemaCityWroclavia -> SourceData()))
    record.cities shouldBe Seq(Wroclaw)
  }

  it should "return cities in City.all order so headOption is deterministic" in {
    // KinoApollo is in Poznań (earlier in City.all), CinemaCityWroclavia in Wrocław.
    val record = MovieRecord(data = Map[Source, SourceData](
      CinemaCityWroclavia -> SourceData(),
      KinoApollo          -> SourceData()
    ))
    record.cities shouldBe Seq(Poznan, Wroclaw)
    record.cities.headOption shouldBe Some(Poznan)
  }

  it should "ignore non-cinema (TMDB/IMDb) slots and be empty when no cinema screens it" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("x")),
      Imdb -> SourceData()
    ))
    record.cities shouldBe empty
  }

  // A US wide release lists at ~1000 venues. `cities` used to ask each of the
  // ~800 GLOBAL cities whether it held one of this film's venues, and each of
  // those asks rebuilt `cinemaData` — a sort plus a map build over every slot.
  // That is ~450ms per call on this shape, and the read-model projector calls it
  // on every projection, which made it the worker's dominant CPU driver. The
  // reverse index (`City.forCinema`) makes the cost linear in the film's OWN
  // venues. Held by what a call allocates, not by its wall time: a per-call
  // millisecond bound measured the machine's load as much as the code, and failed
  // a loaded CI runner (52 ms against 50) with the fix in place. Rebuilding per city
  // allocates ~800 `cinemaData` builds; the bound is 10.
  it should "resolve a nationwide venue list without rebuilding cinemaData per city" in {
    val venues = City.all.filter(_.country == Country.UnitedStates).flatMap(_.cinemas).distinct.take(1000)
    venues.size should be >= 500
    val record = MovieRecord(data = venues.map(c => (c: Source) -> SourceData(title = Some("Wide Release"))).toMap)

    record.cities shouldBe City.all.filter(c => venues.exists(c.cinemaSet.contains))

    assume(tools.ThreadAllocation.supported, "this JVM cannot say what a call allocated")
    given services.movies.LatestTitleYear =
      services.movies.LatestTitleYear(java.time.Clock.fixed(java.time.Instant.parse("2026-06-01T00:00:00Z"), java.time.ZoneOffset.UTC))
    def allocated(work: => Any): Long = { work; tools.ThreadAllocation.of(work)._2 }
    val oneBuild = allocated(record.cinemaData)
    val cities   = allocated(record.cities)
    info(s"cities allocated ${cities / 1000} kB for ${venues.size} venues; one cinemaData build ${oneBuild / 1000} kB")
    cities should be < oneBuild * 10
  }
}
