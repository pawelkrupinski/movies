package services

import play.api.libs.json.{JsObject, Json}

/**
 * The collections the dev-only `/debug*` pages read through the local read-mirror
 * (`MONGODB_MOVIES_MIRROR_URI`), and therefore the collections
 * `scripts/local-mirror/mirror-targets.js` has to sync. The list is ONE file,
 * `common/src/main/resources/debug-mirror-collections.json`, which the sync reads too and which
 * states why each collection is there.
 *
 * Why this exists: web's `Wiring` points EVERY reader in a `/debug` country stack
 * at the mirror connection, and a collection the sync doesn't carry reads as
 * permanently EMPTY rather than failing — so the page renders blank with nothing
 * in the logs. That has now happened twice: `web_movies`/`web_screenings` (the
 * `/debug/readmodel` dump, blank for every non-boot country since the mirror
 * widened to per-country stacks) and `movie_slots` (blank showtimes once the
 * corpus stopped embedding `sourceData`). Both were silent because the sync's
 * collection list and the app's readers were only ever linked by a hand-written
 * literal in a spec, which drifted.
 *
 * So: every reader Wiring points at the mirror has its collection in that file, and
 * `MongoConnectionSpec` fails when one is missing. ADD A READER TO A `/debug` STACK ⇒ ADD ITS COLLECTION TO THE FILE.
 *
 * The reverse does not hold: an entry here need not back a page. `cinema_scrapes`
 * is mirrored for INSPECTION and local replay rather than for a reader, and
 * `uptimeBuckets` and `filmwebFallback` for the white-cinema sweep — see their notes in the file. The rule is "a reader implies an entry", not "an entry implies a
 * reader".
 *
 * `tasks` is deliberately absent: it is a WRITE path (the /debug row's ↻
 * re-enrich enqueues for the real worker to consume), so mirroring it would
 * strand the work locally rather than make a page fast.
 */
object DebugMirror {
  /** The venue film pages the worker's `VenuePageStore` keeps — named here because the reader is web's. */
  val VenuePages       = "venue_pages"
  /** Each venue's last scrape as the identity intake holds it (worker `IdentityListingIntake`). */
  val IdentityListings = "identity_listings"
  /** The worker's normalized TMDB store of films (worker `TmdbKind.Film`), one document per TMDB id. */
  val TmdbFilms        = "tmdb_films"
  /** What the other film databases answered, and the posters' hashes (worker `TmdbKind.Family`): the review pages read
   *  a film's poster paths from it. */
  val FamilyAnswers    = "identity_family_answers"
  /** The identity model's families and each listing's trace (`MongoIdentityModelStore`, `MongoIdentityTraceStore`, which
   *  a spec holds to these names: the stores are the identity code's, which this mirror does not reach). */
  val IdentityFamilies = "identity_model_families"
  val IdentityTraces   = "identity_traces"
  /** What each country's worker publishes of its identity stage's waits (`IdentityStaging`, which a spec holds to this
   *  name): the staging screen's, `/debug/identity/staging`. */
  val IdentityStaging  = "identity_staging"

  /** The collection list itself, with a rationale for each: one checked-in file that the sync reads too
   *  (`scripts/local-mirror/mirror-targets.js`), so a collection is added in ONE edit. */
  val ListResource = "debug-mirror-collections.json"

  /** Every collection the sync carries, read from [[ListResource]]. The specs hold the stores' own collection names
   *  (`MongoConnectionSpec`, identity's `MirroredStoresSpec`) to this set. */
  val Collections: Set[String] = {
    val in = Option(getClass.getClassLoader.getResourceAsStream(ListResource))
      .getOrElse(throw new IllegalStateException(s"$ListResource is missing from the classpath"))
    try (Json.parse(in) \ "collections").as[Seq[JsObject]].map(c => (c \ "name").as[String]).toSet
    finally in.close()
  }
}
