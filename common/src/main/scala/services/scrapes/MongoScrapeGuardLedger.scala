package services.scrapes

import models.Cinema
import org.mongodb.scala.bson.{BsonDocument, BsonInt32, BsonString}
import org.mongodb.scala.model.{Filters, Projections, Updates}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture}
import play.api.Logging
import services.movies.{ScrapeGuardLedger, ScrapeGuardState}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * [[ScrapeGuardLedger]] kept in `cinema_scrapes`, as a `guard` sub-document on the
 * venue's archive row: the row already exists per venue, is written by the one worker
 * that owns the country, and is where "what did this venue's last scrape look like"
 * already lives. The archive only ever `$set`s its own fields, so the two never
 * overwrite each other.
 *
 * Never creates a row: the runner archives every attempt BEFORE the guards run, so the
 * venue's row is already there, and a guard-only row would read to the archive's
 * consumers as a venue that has never produced anything. Were the archive write to
 * fail, this tick's guard state is dropped with it — the same best-effort bargain.
 *
 * Best-effort both ways, like the archive. A failed read returns
 * [[ScrapeGuardState.Fresh]] — no rewire inferred, the grace counted from zero: the
 * guards stay conservative rather than guessing. A failed write logs and the next
 * tick writes again.
 */
class MongoScrapeGuardLedger(sharedDb: Option[MongoDatabase]) extends ScrapeGuardLedger with Logging {

  private val Field = "guard"

  private lazy val coll: Option[MongoCollection[Document]] =
    sharedDb.map(_.getCollection[Document](ScrapeArchiveRepository.Collection))

  def get(cinema: Cinema): ScrapeGuardState = coll.flatMap { c =>
    attempt(cinema, "get")(Await.result(
      c.find(Filters.eq("_id", cinema.displayName)).projection(Projections.include(Field)).headOption(),
      10.seconds)).flatten
  }.flatMap(_.get[BsonDocument](Field)).map { g =>
    def int(name: String) = Option(g.get(name)).filter(_.isInt32).map(_.asInt32.getValue).getOrElse(0)
    ScrapeGuardState(
      sourceKey         = Option(g.get("sourceKey")).filter(_.isString).map(_.asString.getValue),
      depthRejections   = int("depthRejections"),
      breadthRejections = int("breadthRejections"))
  }.getOrElse(ScrapeGuardState.Fresh)

  def put(cinema: Cinema, state: ScrapeGuardState): Unit = coll.foreach { c =>
    val guard = new BsonDocument()
      .append("depthRejections", BsonInt32(state.depthRejections))
      .append("breadthRejections", BsonInt32(state.breadthRejections))
    state.sourceKey.foreach(key => guard.append("sourceKey", BsonString(key)))
    attempt(cinema, "put")(Await.result(
      c.updateOne(Filters.eq("_id", cinema.displayName), Updates.set(Field, guard))
        .toFuture(),
      10.seconds))
  }

  private def attempt[A](cinema: Cinema, op: String)(body: => A): Option[A] =
    Try(body) match {
      case Success(value) => Some(value)
      case Failure(e)     =>
        logger.warn(s"ScrapeGuardLedger.$op(${cinema.displayName}) failed: ${e.getMessage}")
        None
    }
}
