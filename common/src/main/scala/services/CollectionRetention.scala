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

/**
 * Every Mongo collection the code reads or writes, with how its documents stop being kept.
 *
 * The class of bug this closes: a collection filled forever because nobody decided otherwise — the
 * TMDB store kept every answer and gap marker it was ever given, with a comment promising a sweep
 * that did not exist. `CollectionRetentionSpec` enumerates the collection names in main sources and
 * fails on one not declared here, on an entry no source names any more, on a [[Retention.Sweep]] whose
 * class is gone, and on a [[Retention.Unswept]] backlog that grew.
 */
object CollectionRetention {
  import Retention._

  private val OnePerVenue = KeptForever("one document per venue, rewritten in place")

  val Declared: Map[String, Retention] = Map(
    "uptimeBuckets"           -> Ttl("bucket"),
    "uptimeServiceTags"       -> KeptForever("one document per monitored service (unique index)"),
    "database_owner"          -> KeptForever("a handful of owner markers"),
    "detail_cache"            -> Ttl("expireAt"),
    "resolve_imdb"            -> Ttl("at"),
    "resolve_rt"              -> Ttl("at"),
    "resolve_mc"              -> Ttl("at"),
    "resolve_filmweb"         -> Ttl("at"),
    "tasks"                   -> Sweep("MongoTaskQueue"),
    "bulk_task_results"       -> KeptForever("at most one document per bulk task type"),
    "filmwebFallback"         -> OnePerVenue,
    "cinema_scrapes"          -> OnePerVenue,
    "identity_listings"       -> OnePerVenue,
    "movies"                  -> Sweep("IdentityProjection"),
    "screenings"              -> Sweep("StrandedSideRowsCleanup"),
    "movie_slots"             -> Sweep("StrandedSideRowsCleanup"),
    "web_movies"              -> Sweep("ReadModelProjector"),
    "web_screenings"          -> Sweep("ReadModelProjector"),
    "read_model_derivation"   -> KeptForever("two fixed ids: the projection's and the pass's marker"),
    "change_stream_tokens"    -> KeptForever("one resume token per change stream"),
    // A gone film's TMDB-keyed rows are swept; its title-keyed ones (no tmdbId, venue detail stamps) are not.
    "freshness"               -> Sweep("OrphanFilmStateSweep"),
    "enrichment_attempts"     -> Sweep("OrphanFilmStateSweep"),
    "rating_cadence"          -> Sweep("OrphanFilmStateSweep"),
    "identity_traces"         -> Sweep("MongoIdentityTraceStore"),
    "identity_model_families" -> Sweep("MongoIdentityModelStore"),
    "identity_model_meta"     -> KeptForever("the model store's meta documents, a fixed set"),
    "identity_pins"           -> KeptForever("operator pins, removed by hand"),
    "identity_film_ids"       -> KeptForever("a film's id must never be reused, so its counter row outlives the film"),
    "identity_proposals"      -> Unswept("one per title key a language model was asked about; a title no venue lists keeps it. " +
      "The rule would be 'no listing of the taken-up model carries the key', but nothing yet says the model's listings " +
      "are complete, and a wrong delete re-pays a language-model call for a title that comes back"),
    "tmdb_films"              -> Sweep("TmdbStoreSweep"),
    "tmdb_people"             -> Sweep("TmdbStoreSweep"),
    "tmdb_queries"            -> Sweep("TmdbStoreSweep"),
    "env_overrides"           -> KeptForever("one per overridden setting, deleted on unset"),
    "env_registry"            -> KeptForever("the fixed set of settings, republished whole"),
    "authExchangeCodes"       -> Ttl("issuedAt"),
    "users"                   -> KeptForever("one per account, deleted with it"),
    "userStates"              -> KeptForever("one per account"),
    "scheduled_runs"          -> Ttl("claimedAt"),
    "scrape_runs"             -> Ttl("createdAt"),
    "scrape_chunks"           -> Ttl("storedAt"),
    "scrape_chunk_pages"      -> Ttl("at"),
    "scrape_costs"            -> KeptForever("one per venue, its runs capped server-side ($slice)"),
    "omdb_attempts"           -> Ttl("at"),
    "venue_closures"          -> KeptForever("one per venue, cleared by the closure sweep"),
    "venue_pages"             -> Unswept("one per venue detail page ever read; a gone page is flagged, never deleted. The rule would be 'no " +
      "current listing names (its group, page)', but listings name pages through each cinema's enricher group and no " +
      "complete read of them exists yet to sweep against"),
    "facebook_rescrapes"      -> Sweep("MongoFacebookRescrapeStore"))
}
