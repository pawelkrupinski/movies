package tools

import org.mongodb.scala.model.{IndexOptions, Indexes}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await

/** What [[MongoIndex.ensure]] makes of an index already in the way of the wanted one, against a real mongod: each
 *  conflict `createIndex` answers (85, 86) settled the one way it can be without ever dropping the old index. */
class MongoIndexReconcileIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite {

  private val Collection = "reconciled"

  private def withDatabase(suite: String)(body: MongoDatabase => Unit): Unit =
    IntegrationCorpusDatabase.withDatabase(mongoTarget, suite)(body)

  private def existing(db: MongoDatabase, keys: org.bson.conversions.Bson, options: IndexOptions): Unit = {
    Await.result(db.getCollection[Document](Collection).createIndex(keys, options).toFuture(), SpecTimeouts.Io)
    ()
  }

  private def indexes(db: MongoDatabase): Seq[(String, Boolean)] =
    Await.result(db.getCollection[Document](Collection).listIndexes().toFuture(), SpecTimeouts.Io)
      .map(index => index("name").asString.getValue -> index.get("unique").exists(_.asBoolean.getValue))
      .filterNot(_._1 == "_id_").sortBy(_._1)

  private def ensure(db: MongoDatabase, options: IndexOptions): MongoIndex.Outcome =
    MongoIndex.ensure(db, Collection, Indexes.ascending("a"), options, "reconcile-spec")

  "an index of the same spec under another name" should "be the wanted one, present" in withDatabase("index-same-spec") { db =>
    existing(db, Indexes.ascending("a"), IndexOptions().name("custom"))
    ensure(db, IndexOptions()) shouldBe MongoIndex.Outcome.Present
    indexes(db) shouldBe Seq("custom" -> false)
  }

  "an index of the wanted name on other keys" should "be left as it is, the wanted one not in place" in withDatabase("index-other-keys") { db =>
    existing(db, Indexes.ascending("b"), IndexOptions().name("a_1"))
    ensure(db, IndexOptions()) shouldBe MongoIndex.Outcome.NotInPlace("an index named `a_1` exists on other keys")
    indexes(db) shouldBe Seq("a_1" -> false)
  }

  "a plain index differing in more than uniqueness" should "be kept, the wanted one not in place" in withDatabase("index-differs") { db =>
    existing(db, Indexes.ascending("a"), IndexOptions())
    ensure(db, IndexOptions().unique(true).sparse(true)) match {
      case MongoIndex.Outcome.NotInPlace(reason) => reason should include("differs in more than uniqueness")
      case other                                 => fail(s"converted what it cannot: $other")
    }
    indexes(db) shouldBe Seq("a_1" -> false)
  }

  "a unique index where a plain one is wanted" should "be kept unique, the wanted one not in place" in withDatabase("index-unique-kept") { db =>
    existing(db, Indexes.ascending("a"), IndexOptions().unique(true))
    ensure(db, IndexOptions()) match {
      case MongoIndex.Outcome.NotInPlace(reason) => reason should include("is unique and a plain one is wanted")
      case other                                 => fail(s"settled what it cannot: $other")
    }
    indexes(db) shouldBe Seq("a_1" -> true)
  }

  "a plain index where a unique one is wanted" should "be converted in place" in withDatabase("index-to-unique") { db =>
    existing(db, Indexes.ascending("a"), IndexOptions())
    ensure(db, IndexOptions().unique(true)) shouldBe MongoIndex.Outcome.ConvertedToUnique
    indexes(db) shouldBe Seq("a_1" -> true)
  }
}
