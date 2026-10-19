package services.readmodel

import models._
import services.movies.{FilmId, InMemoryMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/** The film "Foo" the projector and sweeper specs project, and the projector they drive it through. */
trait ReadModelProjectorFixtures {

  protected def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  protected val fid = s"${titleNormalizer.sanitize("Foo")}|2024"

  protected def slot(showtimes: Seq[Showtime]) =
    SourceData(title = Some("Foo"), releaseYear = Some(2024), filmUrl = Some("https://mk/foo"), showtimes = showtimes)

  protected def venueSlot(title: String, showtimes: Seq[Showtime], url: String = "https://x/foo") =
    SourceData(title = Some(title), releaseYear = Some(2024), filmUrl = Some(url), showtimes = showtimes)

  // tmdbId set → `readyToProject` (TMDB concluded); the projector only
  // publishes rows whose enrichment has settled.
  // One tmdbId per FILM, as the repository's unique index holds it: a spec seeding a second
  // film passes its own.
  protected def record(rating: Option[Double], showtimes: Seq[Showtime], tmdbId: Int = 1): MovieRecord =
    MovieRecord(imdbRating = rating, tmdbId = Some(tmdbId), data = Map[Source, SourceData](Multikino -> slot(showtimes)))

  /** A clock at which the rolling content check's next sweeps start on the slice AFTER `row`'s.
   *  The slice a sweep checks is numbered by the clock, so a spec counting projections across a
   *  few sweeps otherwise also counts the content check re-projecting its row — in the one or two
   *  half-hours a day that slice comes up, and nowhere else. */
  protected def clockSkippingSliceOf(id: FilmId): java.time.Clock =
    java.time.Clock.fixed(java.time.Instant.ofEpochSecond(1800L * (ReadModelSweeper.contentSliceOf(id) + 1)),
                          java.time.ZoneOffset.UTC)

  /** The clock these specs' projectors run on unless a case pins its own: the rolling content
   *  check's next sweeps start on the slice after Foo's, so a case counting projections across a
   *  few sweeps never also counts the check re-projecting its row. */
  protected val specClock = clockSkippingSliceOf(FilmId(fid))

  protected def fixture(): (ReadModelProjector, InMemoryMovieRepository, InMemoryReadModelRepository) = {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm   = new InMemoryReadModelRepository()
    (new ReadModelProjector(repository, rm, rm, clock = specClock), repository, rm)
  }
}
