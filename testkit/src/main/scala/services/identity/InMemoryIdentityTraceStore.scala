package services.identity

import services.movies.ListingKey

import java.util.Locale

/** Traces held in memory, each written as `identity_traces` stores it to BSON and read back — for tests. */
final class InMemoryIdentityTraceStore extends IdentityTraceStore with IdentityTraceReads {
  private val held = scala.collection.mutable.LinkedHashMap.empty[ListingKey, ListingTrace]
  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit = synchronized {
    held.filterInPlace((_, trace) => !removed(trace.family)); added.iterator.flatMap(_.build()).map(stored).foreach(trace => held(trace.listing) = trace)
  }
  private val stored = services.BsonRoundTrip.via(MongoIdentityTraceStore.encode, MongoIdentityTraceStore.decode)
  private def all = synchronized(held.values.toSeq)
  def byRule(rule: String, limit: Int)  = all.filter(_.rules.contains(rule)).take(limit)
  def byFilm(film: Int, limit: Int)     = all.filter(_.film.contains(film)).take(limit)
  def byTitle(text: String, limit: Int) = all.filter(_.listing.rawTitle.toLowerCase(Locale.ROOT).contains(text.toLowerCase(Locale.ROOT))).take(limit)
  def blockers()                        = BlockerCount.of(all)
  def byBlocker(blocker: String, limit: Int) = all.filter(_.blocker.contains(blocker)).take(limit)
  def ruleCounts()                      = all.flatMap(_.rules).groupBy(identity).map { case (rule, hits) => rule -> hits.size }.toSeq.sortBy(c => (-c._2, c._1))
}
