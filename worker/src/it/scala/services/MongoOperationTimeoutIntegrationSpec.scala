package services

import com.mongodb.MongoOperationTimeoutException
import com.mongodb.event.{CommandFailedEvent, CommandListener, CommandSucceededEvent}
import models.{KinoMuranow, MovieRecord, Source, SourceData}
import org.mongodb.scala.{Document, MongoClient, MongoClientSettings, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{ChangeStreamDemand, MongoMovieRepository, MongoSlotsRepository}
import services.tasks.{MongoTaskQueue, TaskType}
import settings.{MongoDatabaseName, MongoOperationTimeout}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The client-side operation timeout ([[MongoConnection.DefaultOperationTimeout]]) against a real mongod.
 *
 * Without it a blocking call site's `Await.result(…, budget)` abandoned only the CALLER: the operation ran
 * on in mongod (`currentOp` kept listing it) for as long as it took. With it the driver sends the budget as
 * `maxTimeMS`, mongod stops the operation, and the caller gets a `MongoOperationTimeoutException`.
 *
 * The long-running operations opt out ([[OperationTimeoutOptOut]]): a change stream under a timeout dies
 * with `MongoOperationTimeoutException` once it has idled that long, so every watcher — the films'
 * [[services.movies.MovieChangeStream]], the side collections' [[services.movies.SideCollectionWatch]] and
 * the task queue's insert doorbell — is run here on a one-second-timeout client, left idle for three of
 * the stream's empty getMores (each one waits a second in mongod), and must still deliver the next write
 * without a single failed command.
 */
class MongoOperationTimeoutIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val Short = MongoOperationTimeout(1.second)
  /** Empty getMores an idle stream sits through — each waits mongod's default second for an event, so
   *  three are three timeouts' worth: a stream under the timeout dies within the first. */
  private val IdleGetMores = 3

  /** A sleep mongod can interrupt (`$where` checks for a kill between documents; a `$function` sleep is not
   *  interrupted), tagged so `currentOp` can find it. */
  private def slowFind(database: MongoDatabase, comment: String) = {
    val collection = database.getCollection[Document]("slow")
    Await.result(collection.insertOne(Document("_id" -> comment)).toFuture(), tools.SpecTimeouts.Io)
    collection.find(Document("$where" -> "sleep(5000); return true")).comment(comment).toFuture()
  }

  /** How many operations tagged `comment` mongod is running (`currentOp` answers on `admin` only). */
  private def running(comment: String): Int = {
    val client = MongoClient(mongoTarget.uri.value)
    try Await.result(client.getDatabase("admin").runCommand(Document("currentOp" -> 1, "command.comment" -> comment)).toFuture(), tools.SpecTimeouts.Io)
      .toBsonDocument.getArray("inprog").size()
    finally client.close()
  }

  /** A throwaway database seen through a connection on an instrumented client: `idle()` returns once
   *  [[IdleGetMores]] more getMores have succeeded, `failedCommands` is every command that failed. */
  private final case class Connected(database: MongoDatabase, idle: () => Unit, failedCommands: () => Seq[String])

  private def withConnection[A](timeout: Option[MongoOperationTimeout])(body: Connected => A): A =
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "operation-timeout") { isolated =>
      val failed   = new ConcurrentLinkedQueue[String]()
      val getMores = new AtomicInteger(0)
      val client = MongoClient(MongoClientSettings.builder(MongoConnection.clientSettings(mongoTarget.uri.value, operationTimeout = timeout))
        .addCommandListener(new CommandListener {
          override def commandSucceeded(event: CommandSucceededEvent): Unit =
            if (event.getCommandName == "getMore") { getMores.incrementAndGet(); () }
          override def commandFailed(event: CommandFailedEvent): Unit = { failed.add(s"${event.getCommandName}: ${event.getThrowable}"); () }
        }).build())
      val connection = MongoConnection.forDatabase(Some(mongoTarget.uri), MongoDatabaseName(isolated.name), MongoRequirement.Optional,
        MongoTuning.Default.copy(operationTimeout = timeout), sharedClient = Some(client))
      def idle(): Unit = {
        val from = getMores.get()
        withClue(s"the stream stopped answering getMores (failed: ${failed.asScala.toList}): ") {
          tools.Eventually.poll()(getMores.get() >= from + IdleGetMores) shouldBe true
        }
        ()
      }
      try body(Connected(connection.database.get, () => idle(), () => failed.asScala.toSeq))
      finally { connection.close(); client.close() }
    }

  "An operation past the connection's operation timeout" should "be stopped by mongod and fail the caller as a timeout" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "operation-timeout") { isolated =>
      // The connection's OWN client, built from its tuning — the path a web or worker boot takes.
      val connection = MongoConnection.forDatabase(Some(mongoTarget.uri), MongoDatabaseName(isolated.name), MongoRequirement.Optional,
        MongoTuning.Default.copy(operationTimeout = Some(Short)))
      try {
        val started = System.nanoTime()
        val failure = Try(Await.result(slowFind(connection.database.get, "csot-stopped"), tools.SpecTimeouts.Io)).failed.get
        withClue(s"the 1 s operation timeout never fired (${failure.getClass.getName}): ") {
          failure shouldBe a[MongoOperationTimeoutException]
        }
        withClue("mongod ran the whole 5 s sleep before answering: ") {
          (System.nanoTime() - started).nanos should be < 5.seconds
        }
        withClue("mongod is still running the operation its caller gave up on: ") {
          running("csot-stopped") shouldBe 0
        }
        MongoConnection.isTransient(failure) shouldBe true
      } finally connection.close()
    }
  }

  it should "run on, abandoned, in mongod when the client has no operation timeout (what a tool's client keeps)" in {
    withConnection(timeout = None) { connected =>
      Try(Await.result(slowFind(connected.database, "no-csot-lingers"), tools.SpecTimeouts.quiet(1.second))).failed.get shouldBe a[java.util.concurrent.TimeoutException]
      running("no-csot-lingers") shouldBe 1
    }
  }

  "The films' change stream" should "outlive the client's operation timeout" in {
    withConnection(Some(Short)) { connected =>
      import connected.database
      val repository = new MongoMovieRepository(Some(database), tools.SpecClock.Pinned, normalizer = titleNormalizer,
        stopwatch = _root_.tools.Stopwatch.System)
      val delivered = new AtomicInteger(0)
      val handle    = repository.watchChanges(_ => { delivered.incrementAndGet(); () }, _ => ()).get
      try {
        connected.idle()
        repository.upsert("__csot-sentinel__", Some(2026), MovieRecord(data = Map[Source, SourceData](
          KinoMuranow -> SourceData(title = Some("__csot-sentinel__")))))
        tools.Eventually.poll()(delivered.get() > 0) shouldBe true
        connected.failedCommands() shouldBe empty
      } finally handle.close()
    }
  }

  "A side collection's change stream" should "outlive the client's operation timeout" in {
    withConnection(Some(Short)) { connected =>
      import connected.database
      val slots     = new MongoSlotsRepository(Some(database), clock = tools.SpecClock.Pinned, stopwatch = _root_.tools.Stopwatch.System)
      val delivered = new AtomicInteger(0)
      val handle    = slots.watchApplied((_, applied) => { delivered.incrementAndGet(); applied() }, ChangeStreamDemand.unbounded).get
      try {
        connected.idle()
        slots.upsertSlot("csot|2026", KinoMuranow.displayName, SourceData(title = Some("csot")))
        tools.Eventually.poll()(delivered.get() > 0) shouldBe true
        connected.failedCommands() shouldBe empty
      } finally handle.close()
    }
  }

  "The task queue's insert doorbell" should "outlive the client's operation timeout" in {
    withConnection(Some(Short)) { connected =>
      import connected.database
      val queue  = new MongoTaskQueue(Some(database))
      val rung   = new AtomicInteger(0)
      val handle = queue.watchWaiting(_ => { rung.incrementAndGet(); () }).get
      try {
        connected.idle()
        queue.enqueue(TaskType.ResolveImdbId, "csot|2026", Map("title" -> "Csot"), tools.SpecClock.Pinned.instant())
        tools.Eventually.poll()(rung.get() > 0) shouldBe true
        connected.failedCommands() shouldBe empty
      } finally handle.close()
    }
  }
}
