package services

import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent, CommandSucceededEvent}
import models.{CinemaCityKinepolis, Helios, HeliosMagnolia, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.bson.BsonDocument
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{CorpusCensus, WorkerCorpusMetrics, WorkerShowtimesMetrics, WorkerSlotFanoutMetrics, WorkerSourceFilmsMetrics}
import services.movies.{BootCorpusReader, CaffeineMovieCache, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.readmodel.{BootCorpusStudy, MongoReadModelRepository, ReadModelProjector}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await
import scala.jdk.CollectionConverters._

/**
 * A worker boot reads the corpus ONCE. Its whole-corpus readers — the cache hydrate, the read-model
 * projector's missing-card check (and its learning) and the corpus census's first pass — each read
 * every film with its `movie_slots` and `screenings`, three times in a boot's first two minutes
 * (worker-us, 2026-10-04: 9.4 s + 4.0 s + 12.5 s). Now the hydrate hands its read to the projector,
 * and the census counts the cache, reading nothing at all.
 *
 * Against a real split store, counting the documents Mongo returns per collection: the in-memory
 * repository has no side collections and no wire to count on. Each boot is measured both ways —
 * the old shape (no handover: each reader reads for itself) and the new — so the gain is a number.
 */
class BootCorpusReadOnceIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val Films    = 300
  private val cinemas  = Seq[Source](Multikino, Helios, HeliosMagnolia, CinemaCityKinepolis)
  private val showtime = (film: Int, n: Int) => Showtime(java.time.LocalDateTime.parse("2031-06-12T10:00").plusDays(n.toLong).plusMinutes(film.toLong),
    bookingUrl = Some(s"https://book/$film/$n"))

  /** Documents Mongo returned, per collection, from `find` and `getMore`. */
  private final class Returned extends CommandListener {
    private val collectionOf = new ConcurrentHashMap[Int, String]()
    val byCollection = new ConcurrentHashMap[String, AtomicLong]()
    override def commandStarted(event: CommandStartedEvent): Unit = event.getCommandName match {
      case "find"    => collectionOf.put(event.getRequestId, event.getCommand.getString("find").getValue); ()
      case "getMore" => collectionOf.put(event.getRequestId, event.getCommand.getString("collection").getValue); ()
      case _         => ()
    }
    override def commandSucceeded(event: CommandSucceededEvent): Unit = Option(collectionOf.remove(event.getRequestId)).foreach { collection =>
      val cursor = event.getResponse.getDocument("cursor", new BsonDocument())
      val batch  = if (cursor.containsKey("firstBatch")) cursor.getArray("firstBatch") else cursor.getArray("nextBatch", new org.bson.BsonArray())
      byCollection.computeIfAbsent(collection, _ => new AtomicLong()).addAndGet(batch.size().toLong); ()
    }
    def of(collection: String): Long = Option(byCollection.get(collection)).fold(0L)(_.get())
    def reset(): Unit = byCollection.clear()
    def summary: String = byCollection.asScala.toSeq.sortBy(_._1).map { case (c, n) => s"$c=${n.get}" }.mkString(", ")
  }

  private def withDatabase(label: String)(body: (MongoDatabase, Returned) => Unit): Unit = {
    val returned = new Returned
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)
      .addCommandListener(returned).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, label))
    try body(db, returned)
    finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }

  private def repositoryOn(db: MongoDatabase) =
    new MongoMovieRepository(Some(db), _root_.tools.SpecClock.Pinned, screenings = Some(new MongoScreeningsRepository(Some(db))),
      slots = Some(new MongoSlotsRepository(Some(db))), normalizer = titleNormalizer)

  /** One worker boot's corpus readers, as `WorkerWiring` builds and starts them; how long it took. */
  private def boot(db: MongoDatabase, handOver: Boolean): Long = {
    val started    = System.nanoTime()
    val repository = repositoryOn(db)
    val readModel  = new MongoReadModelRepository(Some(db))
    val study      = new BootCorpusStudy(titleNormalizer)
    val projector  = new ReadModelProjector(repository, readModel, readModel, clock = _root_.tools.SpecClock.Pinned, bootStudy = Some(study))
    val readers: Seq[BootCorpusReader] = if (handOver) Seq(study) else Nil
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned, bootReaders = readers)
    projector.prepare()
    val registry = new io.prometheus.metrics.model.registry.PrometheusRegistry()
    val census   = new CorpusCensus(cache, WorkerCorpusMetrics.gauge(registry), WorkerSourceFilmsMetrics.gauge(registry),
      WorkerShowtimesMetrics.gauge(registry), WorkerSlotFanoutMetrics.gauge(registry), "pl", models.City.all, _root_.tools.SpecClock.Pinned)
    census.start()
    org.scalatest.concurrent.Eventually.eventually(org.scalatest.concurrent.Eventually.timeout(SpecTimeouts.Settle)) {
      census.reading().subset(WorkerCorpusMetrics.Subset.Total) shouldBe Films
    }
    val took = (System.nanoTime() - started) / 1000000
    census.stop(); projector.stop(); cache.stop(); repository.close()
    took
  }

  "a worker boot" should "read the corpus once, where its readers read it three times" in withDatabase("boot-corpus-read-once") { (db, returned) =>
    val repository = repositoryOn(db)
    (1 to Films).foreach { film =>
      repository.upsert(s"Film $film", Some(2024), MovieRecord(tmdbId = Some(film), data = cinemas.map { cinema =>
        cinema -> SourceData(title = Some(s"Film $film"), filmUrl = Some(s"https://site/$film"), showtimes = (1 to 8).map(showtime(film, _)))
      }.toMap))
    }
    // The last process projected the corpus: the boot's check finds nothing to heal, so every
    // `movies` document a boot reads is one of its corpus scans.
    val readModel = new MongoReadModelRepository(Some(db))
    new ReadModelProjector(repository, readModel, readModel, clock = _root_.tools.SpecClock.Pinned).reconcile()
    repository.close()

    boot(db, handOver = true)               // warm the JVM, so neither measured boot pays for it
    returned.reset()
    val before = boot(db, handOver = false)
    val readBefore = (returned.of("movies"), returned.of("movie_slots"), returned.of("screenings"))
    info(s"before: ${before}ms, returned ${returned.summary}")

    returned.reset()
    val after = boot(db, handOver = true)
    val readAfter = (returned.of("movies"), returned.of("movie_slots"), returned.of("screenings"))
    info(s"after: ${after}ms, returned ${returned.summary}")

    val (slots, screenings) = (Films * cinemas.size, Films * cinemas.size)
    readBefore shouldBe ((2L * Films, 2L * slots, 1L * screenings))   // hydrate, slots-only check; the census reads nothing
    readAfter  shouldBe ((1L * Films, 1L * slots, 1L * screenings))   // the hydrate alone
  }
}
