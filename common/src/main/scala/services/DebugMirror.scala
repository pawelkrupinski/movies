package services

import services.attempts.EnrichmentAttempts
import services.cadence.RatingCadenceReader
import services.movies.{MovieRepository, ScreeningsRepository, SlotsRepository}
import services.readmodel.MongoReadModelRepository
import services.staging.StagingRepository

/**
 * The collections the dev-only `/debug*` pages read through the local read-mirror
 * (`MONGODB_MOVIES_MIRROR_URI`), and therefore the collections
 * `scripts/local-mirror/mirror-targets.js` has to sync.
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
 * So: every reader Wiring points at the mirror names its collection here, and
 * `MongoConnectionSpec` fails when `mirror-targets.js` doesn't carry one of them.
 * ADD A READER TO A `/debug` STACK ⇒ ADD ITS COLLECTION HERE.
 *
 * `tasks` is deliberately absent: it is a WRITE path (the /debug row's ↻
 * re-enrich enqueues for the real worker to consume), so mirroring it would
 * strand the work locally rather than make a page fast.
 */
object DebugMirror {
  val Collections: Set[String] = Set(
    // The corpus table: the film rows, their showtimes, and their per-cinema slots.
    MovieRepository.Collection,
    ScreeningsRepository.Collection,
    SlotsRepository.Collection,
    // The per-row expand: each rating source's last fetch and its refresh cadence.
    EnrichmentAttempts.Collection,
    RatingCadenceReader.Collection,
    // The "pending enrichment (staging)" table.
    StagingRepository.Collection,
    // The /debug/readmodel dump of what the serving app actually serves.
    MongoReadModelRepository.MoviesCollection,
    MongoReadModelRepository.ScreeningsCollection
  )
}
