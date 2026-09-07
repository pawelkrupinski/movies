package services

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

/** Two countries on one database prune each other's read model — the first worker to
 *  boot stamps the database with its country and every other country is refused. */
class DatabaseOwnerIntegrationSpec extends AnyFlatSpec with Matchers {
  private val uri = Env.get("MONGODB_URI").get

  "a database" should "be claimed by its first country and refuse every other" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "database-owner") { db =>
      val owner = new DatabaseOwner(db)
      owner.owner() shouldBe None

      owner.claim(Country.Poland)
      owner.owner() shouldBe Some(Country.Poland.code)
      noException should be thrownBy owner.claim(Country.Poland)   // the same country boots again

      val refused = the[IllegalStateException] thrownBy owner.claim(Country.Germany)
      refused.getMessage should include (Country.Poland.code)
      owner.owner() shouldBe Some(Country.Poland.code)              // the stamp is never taken over
    }
}
