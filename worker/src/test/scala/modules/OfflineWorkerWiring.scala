package modules

import models.{Cinema, Country}
import services.MongoConnection
import tools.{HttpFetch, ObjectGraph, SameThreadExecutionBudget}

/** A whole worker wiring over a disabled Mongo whose network leaf refuses every call: what the specs
 *  that walk EVERY member a wiring declares build (`CountryIsolationMatrixSpec`, `WorkerWiringLifecycleSpec`). */
class OfflineWorkerWiring(c: Country) extends WorkerWiring(c, new SameThreadExecutionBudget) {
  override lazy val mongoConnection: MongoConnection =
    new MongoConnection(uri = None, dbName = settings.MongoDatabaseName("unused"), required = services.MongoRequirement.Optional)
  override protected def realHttpLeaf: HttpFetch = new HttpFetch {
    def get(url: String): String = throw new java.io.IOException(s"no network in this spec: $url")
    def post(url: String, body: String, contentType: String): String = get(url)
  }
  override protected lazy val filmwebFallbackIds: Map[Cinema, Int] = Map.empty

  /** Build EVERY member the wiring declares — not a hand-kept list of the ones someone
   *  thought of, which a component added tomorrow would not be on — and name any that could
   *  not be built (the network leaf refuses every call). */
  def forceBoot(): Seq[(String, Throwable)] = {
    val failed = ObjectGraph.forceLazyMembers(this)
    registerCacheMetrics()
    failed
  }
}
