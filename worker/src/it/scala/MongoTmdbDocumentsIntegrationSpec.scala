package integration

import services.identityadapters.{MongoTmdbDocuments, TmdbDocuments, TmdbKind}
import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}

import org.scalatest.BeforeAndAfterAll
import tools.{IntegrationMongoSuite, IsolatedMongoDatabase}

import scala.concurrent.Await

/** The normalized TMDB store's storage contract over Mongo: the cases `TmdbDocumentsSpec` runs in memory. */
class MongoTmdbDocumentsIntegrationSpec extends services.identity.adapters.TmdbDocumentRetentionBehaviour with IntegrationMongoSuite
    with BeforeAndAfterAll {
  // A database of this run's own, dropped (and its client closed) when the suite ends: a per-suite
  // `<MONGODB_DB>_tmdb` was never dropped, and two runs on one server shared it.
  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "tmdb")
  private val database: MongoDatabase = isolated.database

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  protected def newDocuments(): TmdbDocuments & services.identityadapters.TmdbDocumentRetention = {
    TmdbKind.values.foreach(k => Await.result(database.getCollection(k.collection).drop().toFuture(), SpecTimeouts.Io))
    new MongoTmdbDocuments(database)
  }

  // Left on the server, not trimmed after the read: the partials are the bytes a take-up spent its
  // store batches fetching and decoding (UK: 12.4 s of a 21 s context).
  "an answer's read" should "ask the server for the kind's answer fields only" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val watched = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      }).build())
    try {
      val documents = new MongoTmdbDocuments(watched.getDatabase(database.name))
      newDocuments()
      documents.answers(TmdbKind.Film, Seq("1018"))
      import scala.jdk.CollectionConverters._
      val projection = finds.asScala.toSeq.map(_.getDocument("projection", new org.bson.BsonDocument()))
      projection should have size 1
      projection.head.keySet.asScala.toSet shouldBe Set("record", "hit", "local.imdb_id", "english.imdb_id", "local.runtime", "english.runtime")
    } finally watched.close()
  }

  // A take-up asks every film its searches name for its hit; read whole, each record was most of the bytes fetched.
  "a film's hit" should "be read off the hit's fields alone, as the whole document reads it" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val watched = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      }).build())
    try {
      val stored   = newDocuments()
      val record   = services.identity.IdentityAnswerBson.film(Some(services.identity.measures.Film("Lalka", Some("Lalka"),
        (1 to 20).map(n => s"Alternative $n"), Some(2025), Some(101), Some(Seq("Maciej Kawalski")), popularity = Some(7.3))))
      val searched = new org.bson.BsonDocument("title", new org.bson.BsonString("Dziki")).append("year", new org.bson.BsonInt32(2024))
        .append("popularity", new org.bson.BsonInt32(3))
      val films = Seq(
        "1018" -> new org.bson.BsonDocument("record", record).append("hit", searched),
        "1019" -> new org.bson.BsonDocument("hit", searched),
        "1020" -> new org.bson.BsonDocument("record", org.bson.BsonNull.VALUE).append("hit", searched),
        "1021" -> new org.bson.BsonDocument("record", new org.bson.BsonDocument("title", new org.bson.BsonString("Bare"))),
        "1022" -> new org.bson.BsonDocument("record", org.bson.BsonNull.VALUE))
      stored.put(TmdbKind.Film, films)
      val documents = new MongoTmdbDocuments(watched.getDatabase(database.name))
      finds.clear()
      documents.filmHits(films.map(_._1) :+ "9999") shouldBe
        films.flatMap { case (id, d) => services.identityadapters.TmdbStore.filmHit(id.toInt, d).map(id -> _) }.toMap
      import scala.jdk.CollectionConverters._
      finds.asScala.toSeq.map(_.getDocument("projection", new org.bson.BsonDocument()).keySet.asScala.toSet) shouldBe
        Seq(services.identityadapters.TmdbStore.HitFields.toSet)
    } finally watched.close()
  }

  // A take-up of an empty store files every film its questions name from 64 prefetch threads, a read
  // and a write each: coalesced, those are a few commands, not one per film (`CoalescedTmdbDocuments`).
  "coalesced filings" should "land every caller's document in far fewer commands than callers" in {
    val commands = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val watched = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit = commands.add(event.getCommandName)
      }).build())
    try {
      newDocuments()
      val documents = new services.identityadapters.CoalescedTmdbDocuments(new MongoTmdbDocuments(watched.getDatabase(database.name)))
      val pool  = java.util.concurrent.Executors.newThreadPerTaskExecutor(Thread.ofVirtual().factory())
      val start = new java.util.concurrent.CountDownLatch(1)
      val filed = (0 until 64).map(i => pool.submit((() => {
        start.await()
        documents.get(TmdbKind.Film, Seq(i.toString))
        documents.put(TmdbKind.Film, Seq(i.toString -> new org.bson.BsonDocument("n", new org.bson.BsonInt32(i))))
      }): java.util.concurrent.Callable[Unit]))
      start.countDown()
      try filed.foreach(_.get(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)) finally pool.shutdown()
      import scala.jdk.CollectionConverters._
      val sent = commands.asScala.toSeq
      sent.count(_ == "find") should be < 32
      sent.count(_ == "update") should be < 32
      documents.get(TmdbKind.Film, (0 until 64).map(_.toString)).view.mapValues(_.getInt32("n").getValue).toMap shouldBe
        (0 until 64).map(i => i.toString -> i).toMap
    } finally watched.close()
  }

  // Every projection tick re-read the same answers (worker-us: 237 `tmdb_films` finds in 90 s): kept
  // across ticks, a document is asked of the server again only once something wrote it.
  "cached answers" should "send no find for documents read before and unwritten since, and one for those written" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val watched = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.getString("find").getValue)
      }).build())
    try {
      newDocuments()
      val documents = new services.identityadapters.CoalescedTmdbDocuments(new services.identityadapters.CachedTmdbDocuments(
        new MongoTmdbDocuments(watched.getDatabase(database.name))))
      def film(n: Int) = new org.bson.BsonDocument("hit", new org.bson.BsonDocument("n", new org.bson.BsonInt32(n)))
      documents.put(TmdbKind.Film, (1 to 600).map(i => i.toString -> film(i)))
      val ids = (1 to 700).map(_.toString)
      documents.answers(TmdbKind.Film, ids) should have size 600
      finds.size should be > 0
      finds.clear()
      documents.answers(TmdbKind.Film, ids) should have size 600
      finds.size shouldBe 0
      documents.put(TmdbKind.Film, Seq("5" -> film(-5), "650" -> film(650)))
      val again = documents.answers(TmdbKind.Film, ids)
      import scala.jdk.CollectionConverters._
      finds.asScala.toSeq shouldBe Seq("tmdb_films")
      again("5") shouldBe film(-5)
      again("650") shouldBe film(650)
    } finally watched.close()
  }
}
