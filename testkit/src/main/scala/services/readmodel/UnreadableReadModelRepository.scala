package services.readmodel

import models.{CityScreening, ResolvedMovie}
import tools.contracts.FailsOnPurpose

/**
 * A [[ReadModelReader]] whose whole-collection reads fail while the collections are
 * genuinely populated — the shape `MongoReadModelRepository.pagedFindAll` produces when a
 * keyset scan exhausts its retries against an unreachable Mongo: `Seq.empty`, indistinguishable
 * from a corpus that really is empty.
 *
 * `countMovies` / `countScreenings` keep reporting the true size, because that is what the
 * server-side count does once Mongo answers again. That asymmetry is the point: it lets a spec
 * prove the consumer notices it is serving nothing while the database holds films, instead of
 * sitting on an empty corpus until its next backstop.
 *
 * Writes and watches delegate to the real in-memory store, so a spec can seed a corpus, fail
 * only the reads, and then restore them with [[healReads]].
 */
class UnreadableReadModelRepository extends InMemoryReadModelRepository with FailsOnPurpose {
  @volatile var failingReads: Boolean = true

  /** Fail only the `web_movies` reads, leaving `web_screenings` readable — the one-sided
   *  outage a caller combining the two must still notice. */
  @volatile var screeningsReadable: Boolean = false

  /** Let the whole-collection reads see the store again — a recovered Mongo. */
  def healReads(): Unit = failingReads = false

  private def moviesFail     = failingReads
  private def screeningsFail = failingReads && !screeningsReadable

  override def findAllMovies(): Seq[ResolvedMovie] =
    if (moviesFail) { findAllMoviesCalls.incrementAndGet(); Seq.empty } else super.findAllMovies()

  override def findAllScreenings(): Seq[CityScreening] =
    if (screeningsFail) { findAllScreeningsCalls.incrementAndGet(); Seq.empty } else super.findAllScreenings()

  // The checked reads report the failure the way `MongoReadModelRepository`'s do — empty
  // AND incomplete. Inheriting the in-memory defaults would derive them from the empty
  // reads above and call them complete, a lie the Mongo store never tells.
  override def findAllMoviesChecked(): (Seq[ResolvedMovie], Boolean) =
    if (moviesFail) (findAllMovies(), false) else super.findAllMoviesChecked()
  override def findAllMovieIdsChecked(): (Seq[String], Boolean) =
    if (moviesFail) (Seq.empty, false) else super.findAllMovieIdsChecked()
  override def findAllShareCardRefsChecked(): (Seq[ShareCardRef], Boolean) =
    if (moviesFail) (Seq.empty, false) else super.findAllShareCardRefsChecked()
  override def findAllScreeningRefsChecked(): (Seq[ScreeningRef], Boolean) =
    if (screeningsFail) (Seq.empty, false) else super.findAllScreeningRefsChecked()
}
