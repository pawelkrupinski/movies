package services.identity

import org.bson.{BsonArray, BsonDocument, BsonValue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import scala.jdk.CollectionConverters._

/** How a family's listing-keyed arrays are laid out in its document: in hash buckets once large, so that a listing
 *  joining moves one bucket of each and Mongo's diff of the update is the size of that, not of the document. */
class MongoIdentityModelStoreLayoutSpec extends AnyFlatSpec with Matchers {
  import MongoIdentityModelStore.{decode, encode}

  private val family = LargeIdentityFamily.of(LargeIdentityFamily.venues(3000), digest = 1L)

  private def arrays(doc: BsonDocument): Seq[BsonArray] =
    Seq(doc.getArray("listings"), doc.getArray("nodes")) ++ doc.getArray("decisions").asScala.map(_.asDocument.getArray("members"))

  private def flattened(value: BsonArray): BsonArray =
    new BsonArray(value.asScala.toSeq.flatMap(v => if (v.isArray) v.asArray.asScala.toSeq else Seq(v)).asJava)

  /** `doc` as the store wrote it before buckets: every listing-keyed array one flat, sorted array. */
  private def flat(doc: BsonDocument): BsonDocument = {
    val out = doc.clone()
    def sortedBy(key: BsonValue => BsonDocument)(a: BsonArray) =
      new BsonArray(flattened(a).asScala.toSeq.sortBy(v => ListingKeyBson.decode(key(v)))(using ListingKey.ordering).asJava)
    out.put("listings", sortedBy(_.asDocument)(out.getArray("listings")))
    out.put("nodes", sortedBy(_.asDocument.getDocument("listing"))(out.getArray("nodes")))
    out.getArray("decisions").asScala.foreach { d =>
      d.asDocument.put("members", sortedBy(_.asDocument)(d.asDocument.getArray("members")))
    }
    out
  }

  "a large family" should "store its listings, nodes and members in buckets" in {
    arrays(encode(family)).foreach { array =>
      array.size shouldBe 128
      array.asScala.foreach(_.isArray shouldBe true)
    }
  }

  it should "read back as it was written" in {
    decode(encode(family)) shouldBe family
  }

  it should "move one bucket of each array when a listing joins it" in {
    val joined = LargeIdentityFamily.of(LargeIdentityFamily.venues(3000) :+ "Cinemark Theatre 1500 B", digest = 1L)
    arrays(encode(family)).zip(arrays(encode(joined))).foreach { case (before, after) =>
      before.asScala.zip(after.asScala).count { case (b, a) => b != a } shouldBe 1
    }
  }

  it should "read back from the flat layout every family was stored in before" in {
    val before = flat(encode(family))
    arrays(before).foreach(_.asScala.foreach(_.isArray shouldBe false))
    decode(before) shouldBe family
  }

  it should "keep members out of their sorted order as they stand, flat" in {
    val decision = family.family.decisions.head
    val shuffled = family.copy(family = family.family.copy(decisions = Seq(decision.copy(members = decision.members.reverse)(decision.trace))))
    val stored   = encode(shuffled).getArray("decisions").get(0).asDocument.getArray("members")
    stored.asScala.foreach(_.isArray shouldBe false)
    decode(encode(shuffled)) shouldBe shuffled
  }

  "a small family" should "store its arrays flat, as every family was before" in {
    val small = LargeIdentityFamily.of(LargeIdentityFamily.venues(31), digest = 1L)
    arrays(encode(small)).foreach(_.asScala.foreach(_.isArray shouldBe false))
    decode(encode(small)) shouldBe small
  }
}
