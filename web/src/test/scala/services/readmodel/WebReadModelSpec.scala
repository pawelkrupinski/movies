package services.readmodel

import models.{CityScreening, ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Unit cover for the read cache's read surface. `allScreenings()` exists for the
 * dev `/debug/readmodel` dump, which needs every cached screening across cities
 * (the per-city `screeningsForCity` is the request-time read key, not a dump).
 */
class WebReadModelSpec extends AnyFlatSpec with Matchers {

  private def ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private def movie(id: String) =
    ResolvedMovie(id, id, None, None, Nil, None, None, Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def screening(id: String, film: String, city: String) =
    CityScreening(id, film, city, "Cinema " + id, None, Nil)

  "allScreenings" should "return every cached screening flattened across all city buckets" in {
    val repo = new InMemoryReadModelRepo
    repo.upsertMovie(movie("belle|2021"))
    repo.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repo.upsertScreening(screening("s2", "belle|2021", "krakow"))
    repo.upsertScreening(screening("s3", "belle|2021", "wroclaw"))
    val rm = new WebReadModel(repo)
    rm.reload()

    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "s2", "s3")
    // The per-city read key still partitions them — the dump is the union.
    rm.screeningsForCity("wroclaw").map(_._id) should contain theSameElementsAs Seq("s1", "s3")
  }

  it should "be empty when the cache holds no screenings" in {
    new WebReadModel(new InMemoryReadModelRepo).allScreenings() shouldBe empty
  }
}
