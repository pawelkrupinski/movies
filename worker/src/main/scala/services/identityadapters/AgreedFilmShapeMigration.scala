package services.identityadapters

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import org.bson.BsonDocument
import org.bson.conversions.Bson
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Projections, Sorts, UpdateOneModel, Updates}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.identity.IdentityAnswerBson
import services.identity.agreement.AgreementVerdicts

/**
 * Rewrites every agreed film `identity_agreements` still keeps in the format agreed films were written in until
 * 2026-11-21 (FilmBson: the cast under `cast`, no `alternativeTitles` when there were none) into the one format every
 * store writes a film in ([[IdentityAnswerBson]]): each read by the one reader, which reads both, and written again,
 * so the reader's earlier-format path can go once none is left (the owner, 2026-11-21: "one writer, migration then one
 * reader").
 *
 * Idempotent and resumable: it selects only documents still in the earlier shape, so a second run, or one after a
 * restart cut the first short, finds only what is left. Each rewrite is conditional on the film being the one read, so
 * a verdict the agreement stage rewrote meanwhile (already in the one format) is left alone. Batched by `batch`, walked
 * in `_id` order, at most `maxBatches` a run: a film the reader cannot read is skipped, never asked again in the run,
 * and still counted as left.
 */
final class AgreedFilmShapeMigration(db: MongoDatabase, batch: Int = AgreementVerdicts.WriteBatch, maxBatches: Int = 200) {
  import AgreedFilmShapeMigration._
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](AgreementVerdicts.Collection)

  /** How many verdicts still keep their agreed film in the earlier shape. */
  def left(): Long = Await.result(collection.countDocuments(EarlierShape).toFuture(), Timeout)

  def run(): Outcome = {
    val before = left()
    if (before == 0) Outcome(0, 0, 0, 0)
    else {
      val (rewritten, unreadable) = walk(None, maxBatches, 0, 0)
      Outcome(before, rewritten, unreadable, left())
    }
  }

  /** Rewrites the earlier-shape films after `after`, at most `batches` pages of them: how many it rewrote and how many it
   *  could not read, with those counted so far. */
  @annotation.tailrec
  private def walk(after: Option[String], batches: Int, rewritten: Int, unreadable: Int): (Int, Int) =
    if (batches == 0) (rewritten, unreadable)
    else {
      val found   = page(after)
      val written = rewriteAll(found)
      val (moved, skipped) = (rewritten + written, unreadable + found.size - written)
      if (found.size < batch) (moved, skipped) else walk(found.lastOption.map(_.getString("_id").getValue), batches - 1, moved, skipped)
    }

  /** The next `batch` verdicts whose agreed film is in the earlier shape, after `after` in `_id` order: their ids and films. */
  private def page(after: Option[String]): Seq[BsonDocument] =
    Await.result(collection.find(Filters.and((EarlierShape +: after.map(Filters.gt("_id", _)).toSeq)*))
      .sort(Sorts.ascending("_id")).limit(batch).projection(Projections.include(FilmField)).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
      .map(_.toBsonDocument)

  /** Writes every film of `found` the reader can read in the one format: how many. */
  private def rewriteAll(found: Seq[BsonDocument]): Int = {
    val rewrites = found.flatMap(rewrite)
    if (rewrites.nonEmpty) Await.result(collection.bulkWrite(rewrites).toFuture(), Timeout)
    rewrites.size
  }

  /** The update writing `d`'s agreed film in the one format, provided it is still the film read; none when the reader
   *  cannot read it. */
  private def rewrite(d: BsonDocument): Option[UpdateOneModel[Document]] = {
    val stored = d.getDocument("agreed").getDocument("film")
    Try(IdentityAnswerBson.film(Some(IdentityAnswerBson.recordOf(stored)))).toOption.map(written =>
      new com.mongodb.client.model.UpdateOneModel[Document](Filters.and(Filters.equal("_id", d.get("_id")), Filters.equal(FilmField, stored)), Updates.set(FilmField, written)))
  }
}

object AgreedFilmShapeMigration {
  private val FilmField = "agreed.film"

  /** An agreed film in the earlier shape: its cast under `cast`, or no `alternativeTitles`, which the one format always
   *  writes. */
  val EarlierShape: Bson = Filters.and(Filters.`type`(FilmField, org.bson.BsonType.DOCUMENT),
    Filters.or(Filters.exists(s"$FilmField.cast"), Filters.exists(s"$FilmField.alternativeTitles", false)))

  /** A run: how many were in the earlier shape before it, how many it rewrote, how many it could not read, how many are
   *  left after it (0 once done). */
  final case class Outcome(before: Long, rewritten: Int, unreadable: Int, left: Long) {
    def line: String = s"agreed films in the earlier shape: $before before, $rewritten rewritten, $unreadable unreadable, $left left"
  }
}
