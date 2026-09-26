package services

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env
import org.mongodb.scala.SingleObservableFuture
import scala.util.chaining.scalaUtilChainingOps

/** Two countries on one database prune each other's read model — the first worker to
 *  boot stamps the database with its country and every other country is refused. */
class DatabaseOwnerIntegrationSpec extends AnyFlatSpec with Matchers {
  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  private val uri = Env.fromProcess().get("MONGODB_URI").get

  private val target = tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get
  "a database" should "be claimed by its first country and refuse every other" in
    tools.IntegrationCorpusDatabase.withDatabase(target, "database-owner") { db =>
      val owner = new DatabaseOwner(db)
      owner.owner() shouldBe None

      owner.claim(Country.Poland)
      owner.owner() shouldBe Some(Country.Poland.code)
      noException should be thrownBy owner.claim(Country.Poland)   // the same country boots again

      val refused = the[IllegalStateException] thrownBy owner.claim(Country.Germany)
      refused.getMessage should include (Country.Poland.code)
      owner.owner() shouldBe Some(Country.Poland.code)              // the stamp is never taken over
    }

  // Two workers for different countries booting together against one database: both
  // must not come away owning it. A read-then-write claim let both read "unowned" and
  // both stamp it, the last write winning while the first worker kept running.
  it should "let exactly one of two countries claiming it at once win" in
    tools.IntegrationCorpusDatabase.withDatabase(target, "database-owner-race") { db =>
      tools.ConcurrentInstances.rounds(20, tools.ConcurrentInstances.baseSeed(Env.fromProcess())) { round =>
        new DatabaseOwner(db).owner().foreach(_ => db.getCollection(DatabaseOwner.Collection)
          .drop().toFuture().pipe(scala.concurrent.Await.result(_, scala.concurrent.duration.Duration(10, "s"))))
        val claims = tools.ConcurrentInstances.race(Seq(Country.Poland, Country.Germany)
          .map(country => () => new DatabaseOwner(db).claim(country)), Some(round))
        claims.count(_.isRight) shouldBe 1
      }
    }
}
