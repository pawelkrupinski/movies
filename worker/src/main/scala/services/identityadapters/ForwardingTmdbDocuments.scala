package services.identityadapters

import org.bson.BsonDocument
import services.identity.vocabulary.Hit

/**
 * `inner`'s documents, every read and write forwarded as asked: the base of a layer over the store's documents that
 * changes a few of them (keeps answers, coalesces round-trips, appends to the history) and leaves the rest as they were.
 * Each read is forwarded, never left to [[TmdbDocuments]]' defaults: those read through `get`, which would bypass the
 * inner store's own answers (its projection, or the cache that keeps them).
 */
abstract class ForwardingTmdbDocuments(inner: TmdbDocuments) extends TmdbDocuments {
  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.get(kind, ids)
  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answers(kind, ids)
  override def filmHits(ids: Seq[String]): Map[String, Hit] = inner.filmHits(ids)
  override def answersReadThrough(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answersReadThrough(kind, ids)
  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = inner.put(kind, docs)
}
