package services.identity
package adapters

import models.{ListingKey, MovieRecord, Tmdb}
import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identitymodel.{MongoPendingListingStore, MongoRetainedListingStore, RetainedListingStore}, services.identityprojection.ProjectionDrafter, services.tmdb.HistoryEntry

/** What the identity adapters read and could not use -- a stored listing that no longer decodes, a film's TMDB details
 *  that failed -- each left out as before, and now said out loud: the read gives less than the store holds. */
class IdentityReadSwallowsSpec extends AnyFlatSpec with Matchers {

  private val undecodable = new BsonDocument("not", new BsonString("a listing"))
  private val listingLogger = "services.identitymodel.StoredListing"
  private def warnings(loggerName: String)(body: => Unit): Seq[String] =
    tools.LogCapture.capture(loggerName)(body).map(_.getFormattedMessage)

  "a pending listing whose ready version no longer decodes" should "read as not ready, and say so" in {
    val doc = new BsonDocument("_id", new BsonString(ListingKey.serialised(ListingKey.Native("venue", "page", "Raw"))))
      .append("since", new BsonInt64(1L)).append("ready", undecodable)
    var ready: Option[Any] = Some(())
    warnings(listingLogger)({ ready = MongoPendingListingStore.pendingOf(doc).flatMap(_.ready) }).exists(_.contains("pending listings")) shouldBe true
    ready shouldBe None
  }

  "a retained listing that no longer decodes" should "be left out, and say so" in {
    var departed: Option[Any] = Some(())
    warnings(listingLogger)({ departed = MongoRetainedListingStore.departedOf(new BsonDocument("listing", undecodable)) })
      .exists(_.contains("retained listings")) shouldBe true
    departed shouldBe None
  }

  "a history entry whose listing no longer decodes" should "be left out, and say so" in {
    val entry = HistoryEntry(HistoryEntry.Listing, "listing-1", 1L, None, Some(undecodable), "build")
    var listings: Seq[Any] = Seq(())
    warnings(listingLogger)({ listings = RetainedListingStore.fromHistory(Seq(entry)) }).exists(_.contains("identity history")) shouldBe true
    listings shouldBe empty
  }

  "a film whose TMDB details fail" should "be drafted without them, and say so" in {
    val draft  = FilmDraft(1L, None, Nil, MovieRecord(tmdbId = Some(42)), "anchor")
    var drafted: Seq[FilmDraft] = Nil
    val lines = warnings("services.identityprojection.ProjectionDrafter") {
      drafted = ProjectionDrafter.detailed(Seq(draft), (_, _) => throw new IllegalStateException("TMDB down"))
    }
    drafted shouldBe Seq(draft)
    drafted.head.record.data.contains(Tmdb) shouldBe false
    lines.exists(_.contains("TMDB down")) shouldBe true
  }
}
