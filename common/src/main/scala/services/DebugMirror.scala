package services

import play.api.libs.json.{JsString, Json}

/**
 * The collections the local read-mirror (`MONGODB_MOVIES_MIRROR_URI`) syncs from prod, and therefore the collections
 * `scripts/local-mirror/mirror-targets.js` has to sync: the [[MongoCollections]] entries marked mirrored, each with
 * why, written out for the sync as `common/src/main/resources/debug-mirror-collections.json`.
 *
 * Why this exists: a reader pointed at the mirror reads a collection the sync doesn't carry as permanently EMPTY
 * rather than failing. That happened twice to the retired `/debug` pages (`web_movies`/`web_screenings` and
 * `movie_slots`), silently, because the sync's collection list and the app's readers were only ever linked by a
 * hand-written literal in a spec, which drifted. So every reader of the mirror has its collection marked mirrored,
 * and `MongoConnectionSpec` fails when one is missing. ADD A MIRROR READER ⇒ MARK ITS COLLECTION MIRRORED.
 *
 * The reverse does not hold: most entries back no reader in the apps. They are mirrored for local inspection, replays
 * and the agents' identity and white-cinema scripts — see their notes in the registry. The rule is "a reader implies
 * an entry", not "an entry implies a reader".
 *
 * `tasks` is deliberately absent: it is a WRITE path, so mirroring it would strand work locally.
 */
object DebugMirror {
  /** The identity model's families and each listing's trace (`MongoIdentityModelStore`, `MongoIdentityTraceStore`, which
   *  a spec holds to these names: the stores are the identity code's, which this mirror does not reach). */
  val IdentityFamilies = "identity_model_families"
  val IdentityTraces   = "identity_traces"
  /** What each country's worker publishes of its identity stage's waits (`IdentityStaging`, which a spec holds to this
   *  name). */
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
      "collection missing here reads as permanently EMPTY to a mirror reader, not slowly-from-prod. A running mirror notices an " +
      "edit within a minute and restarts its tailers. `tasks` is deliberately absent: it is a WRITE path, so mirroring it " +
      "would strand work locally."

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
