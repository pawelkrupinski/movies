package services

import play.api.libs.json.{JsString, Json}

/**
 * The collections the dev-only `/debug*` pages read through the local read-mirror
 * (`MONGODB_MOVIES_MIRROR_URI`), and therefore the collections
 * `scripts/local-mirror/mirror-targets.js` has to sync: the [[MongoCollections]] entries marked mirrored, each with
 * why, written out for the sync as `common/src/main/resources/debug-mirror-collections.json`.
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
 * So: every reader Wiring points at the mirror has its collection marked mirrored, and
 * `MongoConnectionSpec` fails when one is missing. ADD A READER TO A `/debug` STACK ⇒ MARK ITS COLLECTION MIRRORED.
 *
 * The reverse does not hold: an entry here need not back a page. `cinema_scrapes`
 * is mirrored for INSPECTION and local replay rather than for a reader, and
 * `uptimeBuckets` and `filmwebFallback` for the white-cinema sweep — see their notes in the registry. The rule is "a reader implies an entry", not "an entry implies a
 * reader".
 *
 * `tasks` is deliberately absent: it is a WRITE path (the /debug row's ↻
 * re-enrich enqueues for the real worker to consume), so mirroring it would
 * strand the work locally rather than make a page fast.
 */
object DebugMirror {
  /** The venue film pages the worker's `VenuePageStore` keeps. */
  val VenuePages       = "venue_pages"
  /** Each venue's last scrape as the identity intake holds it (worker `IdentityListingIntake`). */
  val IdentityListings = "identity_listings"
  /** The worker's normalized TMDB store of films (worker `TmdbKind.Film`), one document per TMDB id. */
  val TmdbFilms        = "tmdb_films"
  /** What the other film databases answered, and the posters' hashes (worker `TmdbKind.Family`). */
  val FamilyAnswers    = "identity_family_answers"
  /** The identity model's families and each listing's trace (`MongoIdentityModelStore`, `MongoIdentityTraceStore`, which
   *  a spec holds to these names: the stores are the identity code's, which this mirror does not reach). */
  val IdentityFamilies = "identity_model_families"
  val IdentityTraces   = "identity_traces"
  /** What each country's worker publishes of its identity stage's waits (`IdentityStaging`, which a spec holds to this
   *  name): the staging screen's, `/debug/identity/staging`. */
  val IdentityStaging  = "identity_staging"

  /** The collection list the sync reads (`scripts/local-mirror/mirror-targets.js`), with a rationale for each: GENERATED
   *  from [[MongoCollections]] by `sbt 'worker/Test/runMain deploy.DebugMirrorList'` ([[rendered]]), which
   *  `DebugMirrorListSpec` holds it to. */
  val ListResource = "debug-mirror-collections.json"

  /** Every collection the sync carries: the registry's mirrored ones. The specs hold the stores' own collection names
   *  (`MongoConnectionSpec`, identity's `MirroredStoresSpec`) to this set. */
  val Collections: Set[String] = MongoCollections.mirrored.map(_.name).toSet

  private val About =
    "THE list of collections the local prod mirror syncs (scripts/local-mirror reads it through mirror-targets.js; " +
      "services.DebugMirror reads it as DebugMirror.Collections). One entry per collection, each with why it is mirrored. A " +
      "collection missing here reads as permanently EMPTY on a /debug page, not slowly-from-prod. A running mirror notices an " +
      "edit within a minute and restarts its tailers. `tasks` is deliberately absent: it is a WRITE path (the /debug row's " +
      "re-enrich enqueues for the real worker), so mirroring it would strand the work locally."

  /** [[ListResource]]'s content, as generated from the registry. */
  def rendered: String = {
    def text(s: String) = Json.stringify(JsString(s))
    val entries = MongoCollections.mirrored.map(c => s"""    { "name": ${text(c.name)}, "why": ${text(c.mirrored.get)} }""")
    s"""{
       |  "_about": ${text(About)},
       |  "collections": [
       |${entries.mkString(",\n")}
       |  ]
       |}
       |""".stripMargin
  }
}
