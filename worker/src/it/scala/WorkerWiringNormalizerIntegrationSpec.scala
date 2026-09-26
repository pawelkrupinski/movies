package modules

import models.Country
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import services.titlerules.TitleRuleSet
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Every component a wiring builds keys through THAT wiring's country.
 *
 * No component resolves its normalizer from the environment — each takes one as
 * a required argument, since no one answer would fit a worker running several
 * countries. So the composition root has to build one per country and hand it
 * down, and this asserts it does: built for Germany, in a JVM whose environment
 * names no country, every seam must be German rather than Poland.
 *
 * In the integration layer because `WorkerWiring.mongoConnection` refuses to
 * start without `MONGODB_URI` — building the root is the thing under test, so
 * there is nothing to fake. Each wiring is pointed at its OWN database through
 * the existing `mongoDbName` seam: an earlier version let them share the suite's
 * database and that alone broke `StagingFoldIntegrationSpec`'s retired-key case,
 * because constructing a root hydrates a cache and watches `movies`. Overriding
 * the seam rather than rebuilding the component keeps this a real wiring.
 */
class WorkerWiringNormalizerIntegrationSpec extends AnyFlatSpec with BeforeAndAfterAll {

  private val built        = scala.collection.mutable.ListBuffer.empty[WorkerWiring]
  private val ownDatabases = scala.collection.mutable.ListBuffer.empty[String]

  /** A real wiring on a database of its own, so nothing it touches is shared. */
  private def isolated(forCountry: Country): WorkerWiring = {
    val ownDatabase = s"kinowo_it_wiring_${forCountry.code}"
    // The run's own Env (its MONGODB_URI): a wiring built without one is hermetic and dials nothing.
    val wiring = new WorkerWiring(forCountry, env = tools.Env.fromProcess()) {
      override protected def mongoDbName: settings.MongoDatabaseName = settings.MongoDatabaseName(ownDatabase)
    }
    built += wiring
    ownDatabases += ownDatabase
    wiring
  }

  /**
   * Drop each wiring's database through a FRESH client, after every wiring has stopped.
   *
   * Two things had to be true for this to work, and neither was. The drop went through
   * `w.mongoConnection`, which `w.stop()` has already CLOSED — so it raised
   * `state should be: open`, straight into a `Try` that discarded it. And it was never
   * awaited, so even on an open client the JVM would have exited before the command
   * landed. `kinowo_it_wiring_de` / `_pl` / `_uk` survived every run for both reasons.
   */
  override def afterAll(): Unit = {
    built.foreach(w => scala.util.Try(w.stop()))
    val client = MongoClient(Env.fromProcess().get("MONGODB_URI").get)
    try ownDatabases.distinct.foreach(name =>
      scala.util.Try(Await.result(client.getDatabase(name).drop().toFuture(), 60.seconds)))
    finally client.close()
    super.afterAll()
  }

  "a wiring's components" should "all key through its own country, not the environment's" in {
    val de = isolated(Country.Germany)
    assert(de.titleNormalizer.rules == TitleRuleSet.forCountry(Country.Germany))
    assert(de.movieRepository.normalizer.eq(de.titleNormalizer))
    assert(de.stagingRepository.normalizer.eq(de.titleNormalizer))
    assert(de.movieCache.normalizer.eq(de.titleNormalizer))
  }

  it should "give a UK wiring the UK's rules" in {
    val uk = isolated(Country.UnitedKingdom)
    assert(uk.titleNormalizer.rules == TitleRuleSet.forCountry(Country.UnitedKingdom))
    assert(uk.movieRepository.normalizer.eq(uk.titleNormalizer))
    assert(uk.movieCache.normalizer.eq(uk.titleNormalizer))
  }

  it should "not share a normalizer between two countries' wirings in one JVM" in {
    val pl = isolated(Country.Poland)
    val de = isolated(Country.Germany)
    assert(pl.titleNormalizer.ne(de.titleNormalizer))
    // …and they really do disagree about a title, the point of the split.
    assert(pl.titleNormalizer.sanitize("Minions & Monster") == "minionsimonster")
    assert(de.titleNormalizer.sanitize("Minions & Monster") == "minionsmonster")
  }
}
