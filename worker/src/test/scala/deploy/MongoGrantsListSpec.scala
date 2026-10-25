package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.FleetDatabase

/** `scripts/mongo-grants.tsv` is the list `scripts/mongo-grants --check` holds prod's users to. A country added
 *  to the model without regenerating it would pass the check while its database had no grant, which fails
 *  silently in prod: both tiers boot and every read comes back `not authorized` (adding-a-country.md, phase 3). */
class MongoGrantsListSpec extends AnyFlatSpec with Matchers {

  "scripts/mongo-grants.tsv" should "be what MongoGrantsList generates from the model today" in {
    withClue(s"stale: run sbt 'worker/Test/runMain deploy.MongoGrantsList' and commit ${MongoGrantsList.Path}\n") {
      RepoFile.read(MongoGrantsList.Path) shouldBe MongoGrantsList.rendered
    }
  }

  private def databases(who: String, role: String) =
    MongoGrantsList.grants.collect { case (`who`, `role`, db) => db }.toSet

  it should "give the app readWrite on every country's corpus and on the fleet's own database" in {
    databases("app", "readWrite") shouldBe Country.all.map(_.mongoDb).toSet + FleetDatabase.Dedicated.value
  }

  it should "give the CI reader read on the country corpora and nothing else" in {
    databases("ci-read", "read") shouldBe Country.all.map(_.mongoDb).toSet
    MongoGrantsList.grants.filter(_._1 == "ci-read").map(_._2).toSet shouldBe Set("read")
  }
}
