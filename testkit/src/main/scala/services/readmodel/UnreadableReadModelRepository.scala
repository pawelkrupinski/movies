package services.readmodel

import models.{CityScreening, ResolvedMovie}

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
class UnreadableReadModelRepository extends InMemoryReadModelRepository {
  @volatile var failingReads: Boolean = true

  /** Let the whole-collection reads see the store again — a recovered Mongo. */
  def healReads(): Unit = failingReads = false

  override def findAllMovies(): Seq[ResolvedMovie] =
    if (failingReads) { findAllMoviesCalls.incrementAndGet(); Seq.empty } else super.findAllMovies()

  override def findAllScreenings(): Seq[CityScreening] =
    if (failingReads) { findAllScreeningsCalls.incrementAndGet(); Seq.empty } else super.findAllScreenings()
}
