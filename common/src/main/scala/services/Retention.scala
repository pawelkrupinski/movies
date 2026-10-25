package services

/** How a Mongo collection's documents stop being kept. */
enum Retention {
  /** A TTL index on `field` expires them. */
  case Ttl(field: String)
  /** Code deletes them: `by`, a class in main sources, owns the deletes. */
  case Sweep(by: String)
  /** Kept for good, and `why` that is bounded or right (one per venue, a fixed key set, an id that must never be reused). */
  case KeptForever(why: String)
  /** Nothing deletes them yet, and they grow: the backlog `CollectionRetentionSpec` lets only shrink. */
  case Unswept(todo: String)
}
