package services.readmodel

import models.{CityScreening, ResolvedMovie}
import tools.contracts.FailsOnPurpose

/**
 * A [[ReadModelReader]] whose whole-collection reads fail while the collections are
 * genuinely populated — the shape `MongoReadModelRepository` produces when a keyset scan
 * exhausts its retries against an unreachable Mongo: a failed read, never the empty corpus.
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

  /** Fail the stream checkpoint too — Mongo's `hello` gets no further than its reads do. Off by
   *  default, so a spec can fail the reads alone, as a Mongo that answers a `hello` but times out a
   *  long scan does. */
  @volatile var failingCheckpoint: Boolean = false

  /** Let the reads (and the checkpoint) see the store again — a recovered Mongo. */
  def healReads(): Unit = { failingReads = false; failingCheckpoint = false }

  private def moviesFail     = failingReads
  private def screeningsFail = failingReads && !screeningsReadable

  private def unreadable(what: String) = new java.io.IOException(s"$what unreadable on purpose")
  private def failed(what: String) = tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(unreadable(what)))

  // As `MongoReadModelRepository`'s: a whole-collection read that cannot complete throws, and a
  // checked one answers the failure — never the empty collection a real outage is not.
  override def findAllMovies(): Seq[ResolvedMovie] =
    if (moviesFail) { findAllMoviesCalls.incrementAndGet(); throw unreadable("web_movies") } else super.findAllMovies()

  override def findAllScreenings(): Seq[CityScreening] =
    if (screeningsFail) { findAllScreeningsCalls.incrementAndGet(); throw unreadable("web_screenings") } else super.findAllScreenings()

  override def findAllMoviesChecked(): tools.ReadOutcome[Seq[ResolvedMovie]] =
    if (moviesFail) { findAllMoviesCalls.incrementAndGet(); failed("web_movies") } else super.findAllMoviesChecked()
  override def findAllMovieIdsChecked(): tools.ReadOutcome[Seq[String]] =
    if (moviesFail) failed("web_movies") else super.findAllMovieIdsChecked()
  override def findAllShareCardRefsChecked(): tools.ReadOutcome[Seq[ShareCardRef]] =
    if (moviesFail) failed("web_movies") else super.findAllShareCardRefsChecked()
  override def foreachScreening(f: CityScreening => Unit): tools.ScanOutcome =
    if (screeningsFail) { findAllScreeningsCalls.incrementAndGet(); tools.ScanOutcome.Incomplete(unreadable("web_screenings")) }
    else super.foreachScreening(f)
  override def findAllScreeningRefsChecked(): tools.ReadOutcome[Seq[ScreeningRef]] =
    if (screeningsFail) failed("web_screenings") else super.findAllScreeningRefsChecked()
  override def streamCheckpoint(): tools.ReadOutcome[StreamCheckpoint] =
    if (failingCheckpoint) failed("stream checkpoint") else super.streamCheckpoint()
  // A card is read from both collections, and the Mongo store fails the read when either fails.
  override def findCard(id: String): tools.ReadOutcome[StoredCard] =
    if (moviesFail) failed("web_movies") else if (screeningsFail) failed("web_screenings") else super.findCard(id)
}
