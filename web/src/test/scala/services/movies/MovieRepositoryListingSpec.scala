package services.movies

import models.{CinemaCityWroclavia, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * `findAllForListing` is the corpus-LISTING read: every field the /debug table
 * row renders (per-cinema metadata + counts) but with the heavy per-cinema
 * `showtimes` dropped — they're ~58% of the corpus bytes (measured on prod) and
 * the table never renders them (they're fetched per-row on expand). The trait
 * default strips in-process via `MovieRepository.withoutShowtimes`, keeping the
 * in-memory store's listing view identical to the Mongo one (which strips
 * server-side). `findAll` still carries showtimes — the strip is listing-only.
 */
class MovieRepositoryListingSpec extends AnyFlatSpec with Matchers with LoneElement {

  private val withShowtimes = MovieRecord(
    imdbId = Some("tt0000001"),
    tmdbId = Some(42),
    data = Map[Source, SourceData](CinemaCityWroclavia -> SourceData(
      title     = Some("Belle"),
      showtimes = Seq(
        Showtime(LocalDateTime.of(2026, 6, 1, 18, 30), Some("https://book/1")),
        Showtime(LocalDateTime.of(2026, 6, 1, 21, 0),  Some("https://book/2"))))))

  private def repo = new InMemoryMovieRepository(Seq(("Belle", Some(2021), withShowtimes)))

  "findAllForListing" should "drop per-cinema showtimes but keep all the metadata the table renders" in {
    val row = repo.findAllForListing().loneElement.record
    row.cinemaData(CinemaCityWroclavia).showtimes shouldBe empty       // the heavy part is gone…
    row.cinemaData(CinemaCityWroclavia).title shouldBe Some("Belle")   // …but the per-cinema title stays
    row.cinemaData should have size 1                                  // the cinema count the table shows
    row.imdbId shouldBe Some("tt0000001")
    row.tmdbId shouldBe Some(42)
  }

  it should "leave findAll's showtimes intact — the strip is listing-only" in {
    repo.findAll().loneElement.record.cinemaData(CinemaCityWroclavia).showtimes should have size 2
  }

  "MovieRepository.withoutShowtimes" should "clear showtimes on every source while preserving the rest" in {
    val before = repo.findAll().loneElement
    val after  = MovieRepository.withoutShowtimes(before)
    after.record.data(CinemaCityWroclavia).showtimes shouldBe empty
    after.record.data(CinemaCityWroclavia).title shouldBe Some("Belle")
    after.title shouldBe before.title
    after.record.copy(data = Map.empty) shouldBe before.record.copy(data = Map.empty) // nothing else touched
  }
}
