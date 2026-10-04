package tools

import com.mongodb.event.{CommandListener, CommandStartedEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import org.bson.{BsonArray, BsonDocument, BsonString, BsonValue}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The query plan of every command a piece of code sends to Mongo, read with `explain` against a real
 * server — so a spec can hold a store's reads and writes to an index instead of trusting a comment.
 *
 * The class of bug this closes: a filter on a field no index carries (`uptimeServiceTags.service`
 * scanned the collection on every tag upsert, 93% of all documents the server examined), or a "cheap"
 * probe that is a collection scan (the read model's drift counts). Neither shows in a test that only
 * checks results: a collection scan returns the right answer, slowly, at production size.
 *
 * [[recording]] opens a client that records every query and write command sent through it; [[explain]]
 * then runs each recorded statement through `explain` (a write statement is planned, never applied).
 */
object QueryPlans {

  /** The commands whose plan matters: reads, counts, and the filter half of every write. */
  private val Planned = Set("find", "aggregate", "count", "distinct", "update", "delete", "findAndModify")
  /** Driver plumbing `explain` refuses or does not need. */
  private val Plumbing = Seq("$db", "lsid", "$clusterTime", "txnNumber", "$readPreference", "readConcern", "writeConcern",
    "ordered", "bypassDocumentValidation", "batchSize", "singleBatch", "maxTimeMS", "apiVersion", "apiStrict", "apiDeprecationErrors")

  /** One planned statement: the collection, the statement as sent, and its winning plan. */
  final case class Plan(collection: String, statement: BsonDocument, stages: Seq[String], docsExamined: Long, keysExamined: Long) {
    def collectionScan: Boolean = stages.contains("COLLSCAN")
    /** A blocking in-memory sort: the index served the filter but not the order. */
    def inMemorySort: Boolean   = stages.contains("SORT")
    /** The statement without its values — what a spec names when it allows a shape. */
    def shape: String = s"$collection ${statement.getFirstKey} ${keysOf(statement)}"
    override def toString: String = s"$shape -> ${stages.mkString(">")} (docs $docsExamined, keys $keysExamined)"
  }

  /** A database on its own client that records every planned command `body` sends through it. */
  def recording[A](target: IntegrationMongoTarget, purpose: String)(body: (MongoDatabase, () => Seq[BsonDocument]) => A): A = {
    target.requireThrowaway()
    val sent   = new ConcurrentLinkedQueue[BsonDocument]()
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(target.uri.value))
      // The Scala driver's registry, as `MongoClient(uri)` has it: the Java default would encode a Scala
      // `Document` with its generic `Bson` codec, which cannot decode one.
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit =
          if (Planned(event.getCommandName) && !isChangeStream(event.getCommand)) sent.add(event.getCommand.clone())
      })
      .build())
    val database = client.getDatabase(IntegrationCorpusDatabase.named(target, purpose))
    try body(database, () => sent.asScala.toSeq)
    finally { Await.result(database.drop().toFuture(), 60.seconds); client.close() }
  }

  /** Every statement of `commands` planned against `database` as it holds now: a write command carrying
   *  several statements is explained one statement at a time, the only way `explain` takes one. */
  def explain(database: MongoDatabase, commands: Seq[BsonDocument]): Seq[Plan] =
    commands.flatMap(statements).map { statement =>
      val explain   = new BsonDocument("explain", statement).append("verbosity", new BsonString("executionStats"))
      val explained = Await.result(database
        .runCommand[BsonDocument](explain).toFuture(), 30.seconds)
      val stages = winningStages(explained)
      Plan(statement.getString(statement.getFirstKey).getValue, statement, stages,
        sum(explained, "totalDocsExamined"), sum(explained, "totalKeysExamined"))
    }

  private def isChangeStream(command: BsonDocument): Boolean =
    Option(command.get("pipeline")).exists(p => p.isArray && p.asArray.asScala.exists(s => s.isDocument && s.asDocument.containsKey("$changeStream")))

  private def statements(command: BsonDocument): Seq[BsonDocument] = {
    val bare = new BsonDocument()
    command.entrySet.asScala.foreach(entry => if (!Plumbing.contains(entry.getKey)) bare.put(entry.getKey, entry.getValue))
    bare.getFirstKey match {
      case verb @ ("update" | "delete") =>
        val field = if (verb == "update") "updates" else "deletes"
        bare.getArray(field).asScala.toSeq.map { one =>
          val single = bare.clone()
          single.put(field, new BsonArray(java.util.List.of(one)))
          single
        }
      case _ => Seq(bare)
    }
  }

  /** The stage names of the winning plan, outermost first — classic and slot-based explains alike. */
  private def winningStages(explained: BsonDocument): Seq[String] = {
    def walk(value: BsonValue, inWinning: Boolean): Seq[String] = value match {
      case d: BsonDocument =>
        d.entrySet.asScala.toSeq.flatMap { entry =>
          entry.getKey match {
            case "rejectedPlans" | "allPlansExecution" | "slotBasedPlan" => Nil
            case "stage" if inWinning && entry.getValue.isString => Seq(entry.getValue.asString.getValue)
            case "winningPlan" => walk(entry.getValue, inWinning = true)
            case _             => walk(entry.getValue, inWinning)
          }
        }
      case a: BsonArray => a.asScala.toSeq.flatMap(walk(_, inWinning))
      case _            => Nil
    }
    walk(explained, inWinning = false)
  }

  private def sum(value: BsonValue, field: String): Long = value match {
    case d: BsonDocument => d.entrySet.asScala.toSeq.map { entry =>
      if (entry.getKey == field && entry.getValue.isNumber) entry.getValue.asNumber.longValue else sum(entry.getValue, field)
    }.sum
    case a: BsonArray => a.asScala.toSeq.map(sum(_, field)).sum
    case _            => 0L
  }

  /** The field names a statement filters, sorts, or hints on, nested values replaced by their keys. */
  private def keysOf(statement: BsonDocument): String = {
    def shape(value: BsonValue): String = value match {
      case d: BsonDocument => d.entrySet.asScala.map(e => s"${e.getKey}${if (e.getValue.isDocument || e.getValue.isArray) ":" + shape(e.getValue) else ""}").mkString("{", ",", "}")
      case a: BsonArray    => a.asScala.map(shape).distinct.mkString("[", ",", "]")
      case _               => ""
    }
    val parts = Seq("filter", "query", "q", "sort", "hint", "pipeline").flatMap { k =>
      Option(statement.get(k)).map(v => s"$k${shape(v)}")
    } ++ Seq("updates", "deletes").flatMap(k => Option(statement.get(k)).map(v => shape(v.asArray.get(0).asDocument.get("q"))))
    parts.mkString(" ")
  }
}
