package modules

import models.MovieRecord
import services.MongoConnection
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}
import services.readmodel.{InMemoryReadModelRepository, ReadModelProjection, ReadModelReader}
import play.api.test.Helpers.stubControllerComponents

/** The web composition root over a DISABLED Mongo and an in-memory corpus, so a spec
 *  drives the real `Wiring` — every lazy val as production builds it — without a
 *  cluster. `seed` is projected through the real `ReadModelProjection`, exactly as
 *  the worker writes it, so boot's hydrate has production's single fill path.
 *  Override any member by name for what a spec needs to pin (the clock, a provider). */
class TestWebWiring(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) extends Wiring {
  // A connection with no URI never dials Mongo; `required = false` keeps the
  // disabled state a silent no-op rather than a boot failure.
  override lazy val mongoConnection: MongoConnection =
    new MongoConnection(uri = None, dbName = "kinowo", required = false)
  override lazy val movieRepository = new InMemoryMovieRepository(seed)
  override lazy val readModelRepository: ReadModelReader = {
    val store = new InMemoryReadModelRepository()
    seed.foreach { case (title, year, record) =>
      val stored = StoredMovieRecord(title, year, record)
      store.upsertMovie(ReadModelProjection.resolve(stored, titleNormalizer))
      ReadModelProjection.screenings(stored, titleNormalizer).foreach(store.upsertScreening)
    }
    store
  }

  val controllerComponents = stubControllerComponents()
  def environmentMode       = play.api.Mode.Test
  def messagesApi           = testsupport.TestMessages.messagesApi
  implicit def materializer: org.apache.pekko.stream.Materializer = null

  /** Expose the protected data-layer start so a spec can drive it. */
  def boot(): Unit = start()
}
