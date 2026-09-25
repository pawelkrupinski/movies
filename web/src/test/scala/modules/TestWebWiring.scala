package modules

import models.{Country, MovieRecord}
import services.{MongoAddress, MongoConnection}
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}
import services.readmodel.{InMemoryReadModelRepository, ReadModelProjection, ReadModelReader}
import play.api.test.Helpers.stubControllerComponents

/** The web composition root over a DISABLED Mongo and an in-memory corpus, so a spec
 *  drives the real `Wiring` — every lazy val as production builds it — without a
 *  cluster. `seed` is projected through the real `ReadModelProjection`, exactly as
 *  the worker writes it, so boot's hydrate has production's single fill path.
 *  `country` is the deployment it boots as — handed in as `AppLoader` hands in the
 *  production one, never set in the environment. Override any member by name for what
 *  a spec needs to pin (the clock, a provider). */
class TestWebWiring(
    seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty,
    val country: Country = Country.default) extends Wiring {
  // The process environment, like a production boot; a spec pins a knob by
  // overriding this with `Env.of(...)`.
  lazy val env: tools.Env = tools.Env.fromProcess()
  // No cluster: every connection the wiring opens at this address stays disabled — the
  // corpus, the users database and the /debug stacks alike — whatever MONGODB_URI the
  // shell holds (CI runs the unit suites with it set).
  val mongoAddress: MongoAddress = MongoAddress.Disabled
  // The /debug read-mirror is the one Mongo the wiring still opens from an env knob
  // (MONGODB_MOVIES_MIRROR_URI, a dev-only convenience), so it stays shut here too — the
  // /debug stacks would otherwise dial it with every OTHER country in tow.
  override protected lazy val debugExtraClient: Option[org.mongodb.scala.MongoClient]  = None
  override lazy val movieMirrorConnection: MongoConnection = mongoConnection
  override lazy val movieRepository = new InMemoryMovieRepository(seed, normalizer = titleNormalizer)
  override lazy val readModelRepository: ReadModelReader = {
    val store = new InMemoryReadModelRepository()
    seed.foreach { case (title, year, record) =>
      val stored = StoredMovieRecord.synthesised(title, year, record, services.movies.SingleCountryNormalizer.titleNormalizer)
      store.upsertMovie(ReadModelProjection.resolve(stored, titleNormalizer))
      ReadModelProjection.screenings(stored, titleNormalizer).foreach(store.upsertScreening)
    }
    store
  }

  // The real bundles, as production's: a controller resolves its deployment `Messages`
  // off these components, and the stub default has none (every key renders as itself).
  val controllerComponents = stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi)
  def environmentMode       = play.api.Mode.Test
  def messagesApi           = testsupport.TestMessages.messagesApi
  implicit def materializer: org.apache.pekko.stream.Materializer = null

  /** Expose the protected data-layer start so a spec can drive it. */
  def boot(): Unit = start()
}
