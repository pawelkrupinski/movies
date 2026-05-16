package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MovieRecordMergeSpec extends AnyFlatSpec with Matchers {

  private def slot(poster: String): CinemaShowings = CinemaShowings(
    filmUrl = None, posterUrl = Some(poster), synopsis = None, cast = None,
    director = None, runtimeMinutes = None, releaseYear = None,
    showtimes = Seq.empty
  )

  private val canonical = MovieRecord(
    imdbId        = Some("tt1"),
    imdbRating    = Some(8.0),
    metascore     = Some(75),
    originalTitle = Some("Canonical"),
    tmdbId        = Some(42),
    cinemaScrapes = Set(CinemaScrape(Multikino, "Foo", Some(2024))),
    cinemaShowings = Map(Multikino -> slot("m.jpg"))
  )
  private val victim = MovieRecord(
    imdbId        = Some("tt-stale"),
    imdbRating    = Some(1.0),
    metascore     = None,
    originalTitle = Some("Stale"),
    tmdbId        = Some(42),
    cinemaScrapes  = Set(CinemaScrape(Helios, "Foo", None)),
    cinemaShowings = Map(Helios -> slot("h.jpg"))
  )

  "union" should "keep all enrichment fields from canonical" in {
    val merged = MovieRecordMerge.union(canonical, victim)
    merged.imdbId        shouldBe Some("tt1")
    merged.imdbRating    shouldBe Some(8.0)
    merged.metascore     shouldBe Some(75)
    merged.originalTitle shouldBe Some("Canonical")
    merged.tmdbId        shouldBe Some(42)
  }

  it should "union cinemaScrapes from both rows" in {
    val merged = MovieRecordMerge.union(canonical, victim)
    merged.cinemaScrapes shouldBe Set(
      CinemaScrape(Multikino, "Foo", Some(2024)),
      CinemaScrape(Helios,    "Foo", None)
    )
  }

  it should "union cinemaShowings; victim wins on slot collision (it's the fresher write)" in {
    val withConflict = victim.copy(cinemaShowings = Map(Multikino -> slot("fresh.jpg")))
    val merged = MovieRecordMerge.union(canonical, withConflict)
    merged.cinemaShowings.keySet      shouldBe Set(Multikino)
    merged.cinemaShowings(Multikino).posterUrl shouldBe Some("fresh.jpg")
  }
}
