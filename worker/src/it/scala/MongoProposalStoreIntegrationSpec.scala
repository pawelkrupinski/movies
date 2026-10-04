package services.identity

import tools.SpecTimeouts

import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.Await

/** `identity_proposals` over Mongo: a title's proposal round-trips, and a newer one replaces it. */
class MongoProposalStoreIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {
  "the proposal store" should "keep one proposal per title, the latest" in {
    val client = MongoClient(mongoTarget.uri.value)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "proposals"))
    try {
      val store = new MongoProposalStore(db)
      val at    = Instant.parse("2026-10-03T12:00:00Z")
      store.put(StoredProposal("dyrygent", "Dyrygent", Proposal("unclear"), "m1", at))
      store.put(StoredProposal("dyrygent", "Dyrygent", Proposal("film", Some("Dyrygent"), Some(1980), Seq("Andrzej Wajda")), "m2", at))
      store.put(StoredProposal("warsztaty", "Warsztaty ceramiczne", Proposal("event"), "m2", at))
      store.all().sortBy(_.key) shouldBe Seq(
        StoredProposal("dyrygent", "Dyrygent", Proposal("film", Some("Dyrygent"), Some(1980), Seq("Andrzej Wajda")), "m2", at),
        StoredProposal("warsztaty", "Warsztaty ceramiczne", Proposal("event"), "m2", at))
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }
}
