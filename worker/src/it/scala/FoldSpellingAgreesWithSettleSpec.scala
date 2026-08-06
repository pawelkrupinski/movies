package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.SourceData
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, SingleObservableFuture, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import services.staging.{MongoStagingFolder, StagingRepository}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Loop A — the 30-minute settle beat — at the seam that causes it.
 *
 * A film's identity spelling is a plurality vote over the cinema titles on the row
 * (`FilmCanonicalizer.canonical` → `MovieRecord.displayTitle` → `chooseDisplay`). TWO
 * components run that vote, and under the storage split they do not see the same pool:
 *
 *   - the settle (`MovieCache.canonicalizeBySanitize`) votes on the STITCHED record — every
 *     cinema slot the film has, read back out of `movie_slots`;
 *   - the fold (`MongoStagingFolder.foldOnce`) plans against RAW `movies` documents
 *     (`StagingFoldIntegrationSpec` pins that read), and a migrated film's `sourceData` is
 *     empty — its cinemas are rows in `movie_slots`. So the only cinema titles the fold can
 *     see are the ones on the STAGING rows: whichever venues happen to have diverted.
 *
 * One diverted venue publishing a decorated spelling is therefore an unopposed plurality of
 * one, and the fold re-keys the whole film onto it. The settle then reads the stitched
 * record, sees the twelve venues that publish the film plainly, and re-keys it back. Neither
 * component is wrong on its own inputs and neither converges: ~83
 * `merges_total{reason="canonicalize"}` a day on the :21/:51 beat, three rating lookups per
 * film per cycle, and the fixpoint leg flipping `Arek. Mama. Panorama` against
 * `Przedpremiera: Arek. Mama. Panorama | Wakacje z dokumentem` on alternate ticks.
 *
 * This is invisible without the split — with `sourceData` embedded, the sibling row carries
 * its twelve plain slots into the fold's pool and the two components agree. That is why the
 * in-memory reproduction self-heals in two cycles, and why this spec lives at the `it` layer.
 *
 * Note the difference from `StagingFoldIntegrationSpec`'s "migrated film" case, which probed
 * this same read and concluded the film stays put: it used a SHOUTED variant, which
 * sanitizes to the same key, so no re-key was possible either way. The spellings here
 * sanitize apart, which is the case that moves.
 */
class FoldSpellingAgreesWithSettleSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri    = Env.get("MONGODB_URI").get
  private val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

  // Its own sentinel prefix and its own cleanup — the it suites share one database and run
  // in parallel (see the naming note in StagingFoldIntegrationSpec).
  private val bare      = "__loopaspelling-it-sentinel__"
  private val decorated = s"Przedpremiera: $bare | Wakacje z dokumentem"
  private val tmdbId    = 42432

  // Guard: the two spellings really are distinct keys. If a rule ever collapses them the
  // re-key under test becomes impossible and this spec would pass while asserting nothing.
  require(titleNormalizer.sanitize(bare) != titleNormalizer.sanitize(decorated),
    "the decorated form is now collapsed by a rule — pick another to keep this honest")

  private val bareSanitize      = titleNormalizer.sanitize(bare)
  private val decoratedSanitize = titleNormalizer.sanitize(decorated)

  /** The venues that publish the film plainly — the settled plurality. */
  private val plainVenues = Seq(models.Multikino, models.Helios, models.KinoApollo,
    models.KinoBulgarska, models.CharlieMonroe)
  /** The one venue that dresses it up, and has just diverted into staging. */
  private val fancyVenue  = models.KinoMuza

  it should "keep a film on the spelling its stitched cinemas report, not the one diverted venue's" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val slots      = new MongoSlotsRepository(Some(db))
    val screenings = new MongoScreeningsRepository(Some(db))
    val staging    = db.getCollection(StagingRepository.Collection)
    val movies     = db.getCollection(services.movies.MovieRepository.Collection)
    val settled    = StoredMovieRecord.idFor(bare, Some(2026), titleNormalizer)

    def folder = new MongoStagingFolder(connection, normalizer = titleNormalizer,
      movieRepository = new services.movies.MongoMovieRepository(Some(db), fallbackToOwnInit = false,
        normalizer = titleNormalizer, screenings = Some(screenings), slots = Some(slots)))

    try {
      // A fully MIGRATED film, which is what prod's corpus is: the `movies` document carries
      // no `sourceData` at all, and every cinema it has lives in `movie_slots`. Five venues
      // publish it plainly and one dresses it up, so the settled spelling is the plain one.
      Await.result(movies.replaceOne(Filters.eq("_id", settled),
        org.mongodb.scala.Document("_id" -> settled, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      slots.replaceFilm(settled,
        (plainVenues.map(c => c.displayName -> SourceData(title = Some(bare), releaseYear = Some(2026))) :+
          (fancyVenue.displayName -> SourceData(title = Some(decorated), releaseYear = Some(2026)))).toMap)

      // The fancy venue has diverted: its slot is a staging row, concluded on the same film.
      val stagingId = s"${fancyVenue.displayName}|$decoratedSanitize|2026"
      Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
        org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(fancyVenue.displayName ->
            org.mongodb.scala.Document("title" -> decorated)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      folder.foldGroup(decorated)

      // Premise: the fold consumed the staging row, so it really did choose a spelling.
      withClue("the fold consumed no staging row, so it never chose anything: ")(
        Await.result(staging.find(Filters.eq("_id", stagingId)).toFuture(), 10.seconds) shouldBe empty)

      val survivors = Await.result(movies.find(Filters.regex("_id",
        s"^($bareSanitize|$decoratedSanitize)\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))

      withClue(
        s"survivors=$survivors — the fold could not see the film's five stitched plain " +
        "cinemas (a migrated row reports none) and keyed it on the single diverted venue's " +
        "decorated spelling. The settle reads the stitched record, votes the plain form, and " +
        "re-keys it straight back: that disagreement IS the 30-minute beat.\n") {
        survivors shouldBe Seq(settled)
      }
    } finally {
      slots.deleteFilm(settled)
      screenings.deleteFilm(settled)
      Await.result(movies.find(Filters.regex("_id", s"^($bareSanitize|$decoratedSanitize)\\|"))
        .toFuture(), 10.seconds).flatMap(_.get("_id").map(_.asString().getValue))
        .foreach { id => slots.deleteFilm(id); screenings.deleteFilm(id) }
      Await.ready(movies.deleteMany(Filters.regex("_id",
        s"^($bareSanitize|$decoratedSanitize)\\|")).toFuture(), 10.seconds)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*($bareSanitize|$decoratedSanitize).*"))
        .toFuture(), 10.seconds)
      client.close()
    }
  }
}
