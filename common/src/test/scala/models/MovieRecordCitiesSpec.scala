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
}
