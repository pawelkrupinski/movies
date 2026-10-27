package services.identity

import services.movies.ListingKey

/** Traces held in memory, each written as `identity_traces` stores it to BSON and read back — for tests. */
final class InMemoryIdentityTraceStore extends IdentityTraceStore {
  private val held = scala.collection.mutable.LinkedHashMap.empty[ListingKey, ListingTrace]
  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit = synchronized {
    held.filterInPlace((_, trace) => !removed(trace.family)); added.iterator.flatMap(_.build()).map(stored).foreach(trace => held(trace.listing) = trace)
  }
  private val stored = services.BsonRoundTrip.via(MongoIdentityTraceStore.encode, MongoIdentityTraceStore.decode)
}
