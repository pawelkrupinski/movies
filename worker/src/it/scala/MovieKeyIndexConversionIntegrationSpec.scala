package services.movies

import ch.qos.logback.classic.Level
import org.mongodb.scala.bson.collection.immutable.{Document => ImmutableDocument}
import org.mongodb.scala.model.{IndexOptions, Indexes}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.{ConcurrentInstances, LogCapture}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** The `movies.key` index was made unique (096df7444) over databases that already held a
 *  PLAIN `key_1`. `createIndex` answers that with IndexKeySpecsConflict (86), which the
 *  repository's index helper did not handle: it logged a WARN on every boot and the unique
 *  index was never built in any of the five country databases. A boot now converts the
 *  plain index in place (`collMod` prepareUnique → unique) — never by dropping it, which
 *  would leave the collection with no index while it rebuilds, and with NONE at all if a
 *  duplicate made the rebuild fail. */
class MovieKeyIndexConversionIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def keyIndex(db: MongoDatabase): Option[Document] =
    Await.result(db.getCollection[Document](MovieRepository.Collection).listIndexes().toFuture(), 10.seconds)
      .find(_.get("name").exists(_.asString().getValue == "key_1"))

  private def flag(index: Option[Document], name: String): Boolean =
    index.flatMap(_.get(name)).exists(_.asBoolean().getValue)

  /** A collection as production holds it: rows with a `key` and a PLAIN `key_1`. */
  private def seedPlain(db: MongoDatabase, keys: String*): Unit = {
    val movies = db.getCollection[Document](MovieRepository.Collection)
    Await.result(movies.insertMany(keys.zipWithIndex.map { case (key, i) => ImmutableDocument("_id" -> s"film-$i", "key" -> key) }).toFuture(), 10.seconds)
    Await.result(movies.createIndex(Indexes.ascending("key"), IndexOptions()).toFuture(), 10.seconds)
    ()
  }

  /** Boot the repository on `instance`, polling `listIndexes` from a second thread the whole
   *  time, and return every snapshot in which `key_1` was missing plus the log lines. */
  private def boot(instance: ConcurrentInstances.Instance, observer: MongoDatabase): (Seq[String], Seq[ch.qos.logback.classic.spi.ILoggingEvent]) = {
    val gaps    = new ConcurrentLinkedQueue[String]()
    val running = new AtomicBoolean(true)
    val poller  = new Thread(() => while (running.get) {
      val names = Await.result(observer.getCollection[Document](MovieRepository.Collection).listIndexes().toFuture(), 10.seconds)
        .flatMap(_.get("name").map(_.asString().getValue))
      if (!names.contains("key_1")) gaps.add(names.mkString(","))
    }, "key-index-poller")
    poller.start()
    val events = try LogCapture.capture("services.MongoIndex", Some(Level.INFO)) {
      val repository = new MongoMovieRepository(Some(instance.database), normalizer = titleNormalizer)
      try repository.enabled shouldBe true finally repository.close()
    } finally { running.set(false); poller.join(10000) }
    (gaps.asScala.toSeq, events)
  }

  private def drops(instance: ConcurrentInstances.Instance): Seq[ConcurrentInstances.SentCommand] =
    instance.indexCommands(MovieRepository.Collection).filter(_.name == "dropIndexes")

  private def collMods(instance: ConcurrentInstances.Instance): Seq[ConcurrentInstances.SentCommand] =
    instance.commands.filter(_.name == "collMod")

  "a boot over a plain key_1 with no duplicates" should "convert it to unique in place, and the index never disappears" in
    ConcurrentInstances.withInstances(mongoTarget, "key-index-convert", count = 2) { instances =>
      val (pod, observer) = (instances(0), instances(1))
      seedPlain(observer.database, "a", "b", "c")
      flag(keyIndex(observer.database), "unique") shouldBe false

      val (gaps, _) = boot(pod, observer.database)

      withClue("key_1 must be unique after the boot: ")(flag(keyIndex(observer.database), "unique") shouldBe true)
      withClue("listIndexes snapshots without key_1: ")(gaps shouldBe empty)
      withClue("indexes dropped: ")(drops(pod) shouldBe empty)
    }

  "a boot over a plain key_1 whose rows hold a duplicate" should "keep the non-unique index and log an ERROR" in
    ConcurrentInstances.withInstances(mongoTarget, "key-index-duplicates", count = 2) { instances =>
      val (pod, observer) = (instances(0), instances(1))
      seedPlain(observer.database, "dup", "dup", "other")

      val (gaps, events) = boot(pod, observer.database)

      val index = keyIndex(observer.database)
      withClue(s"key_1 must survive, still plain: $index ")(index.isDefined shouldBe true)
      flag(index, "unique") shouldBe false
      withClue("a failed conversion must hand back exactly the index that was there: ")(flag(index, "prepareUnique") shouldBe false)
      gaps shouldBe empty
      drops(pod) shouldBe empty
      val errors = events.filter(_.getLevel == Level.ERROR).map(_.getFormattedMessage)
      withClue(s"log lines: ${events.map(_.getFormattedMessage)}\n") {
        errors.exists(line => line.contains("key_1") && line.contains("1 key value(s) are held by 2 documents")) shouldBe true
      }
    }

  "a boot over an already-unique key_1" should "change nothing" in
    ConcurrentInstances.withInstances(mongoTarget, "key-index-unique", count = 2) { instances =>
      val (pod, observer) = (instances(0), instances(1))
      val movies = observer.database.getCollection[Document](MovieRepository.Collection)
      Await.result(movies.insertOne(ImmutableDocument("_id" -> "film-0", "key" -> "a")).toFuture(), 10.seconds)
      Await.result(movies.createIndex(Indexes.ascending("key"), IndexOptions().unique(true)).toFuture(), 10.seconds)

      val (gaps, events) = boot(pod, observer.database)

      flag(keyIndex(observer.database), "unique") shouldBe true
      gaps shouldBe empty
      drops(pod) shouldBe empty
      withClue("no collMod for an index that already agrees: ")(collMods(pod) shouldBe empty)
      events.filter(_.getLevel.isGreaterOrEqual(Level.WARN)) shouldBe empty
    }
}
