package services.movies

import services.movies.SingleCountryNormalizer.{titleNormalizer, given}

import models.{KinoMuranow, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * PROBE: does a duplicate MERGE keep both films' showtimes?
 *
 * The sibling of [[RekeyScreeningsIntegrationSpec]], on the call site that fix did not
 * cover. `MovieCache.foldDeterministically` merges two rows that share a tmdbId, persists
 * the union under a canonical key, and then DELETES the losers — which cascades
 * `screenings.deleteFilm(victimId)`. The merged record carries the victim's cinema SLOT
 * (so `movie_slots` survives under the canonical id), but the victim's showtimes only ever
 * existed in `screenings` under the victim's own id, and the `upsert` re-stitches against
 * the id it is WRITING to. So when the canonical key is not the victim's key, the victim's
 * showtimes are destroyed in exactly the same window a re-key used to destroy them.
 *
 * This is why prod kept bleeding after 762f04b4b: the 20:00 UTC cycle on 2026-07-27, with
 * that fix live, still logged 1,039 `movies.delete reason=title+year` and 240
 * `screenings.deleteFilm reason=film-deleted` — unchanged from every pre-deploy cycle.
 *
 * The assertion is canonical-key-agnostic on purpose: whichever id the merge settles on,
 * BOTH cinemas' showtimes must still be reachable somewhere. A merge unions cinemas; it
 * must never be a way to lose one.
 */
class MergeScreeningsIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri    = Env.get("MONGODB_URI").get
  // Its own corpus: this suite hydrates a `CaffeineMovieCache` over the WHOLE `movies`
  // collection and settles it, which is not survivable for a neighbouring suite's rows.
  private val dbName = tools.IntegrationCorpusDatabase.named("merge-screenings")

  // Two rows the fold will recognise as the same film (shared tmdbId) under different keys —
  // the cross-language duplicate shape (`Tangled` / `Zaplatani`) the canonicaliser exists for.
  private val titleA = "__merge-probe-sentinel-a__"
  private val titleB = "__merge-probe-sentinel-b__"
  private val when   = java.time.LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

  it should "keep both films' showtimes when a duplicate merge folds one into the other" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val screenings = new MongoScreeningsRepository(Some(db))
    val slots      = new MongoSlotsRepository(Some(db))
    val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), slots = Some(slots))
    val idA        = StoredMovieRecord.idFor(titleA, Some(2026))
    val idB        = StoredMovieRecord.idFor(titleB, Some(2026))
    try {
      val cache = new CaffeineMovieCache(repository)

      // Row B FIRST, so by the time A arrives B is resident in the cache STRIPPED — its
      // showtimes live only in `screenings` under B's own id, the slot carrying just a digest.
      // Order matters: the fold canonicalises onto A, making B the victim. A merge that
      // settles on the STRIPPED row's own key survives by luck (the re-stitch finds its rows
      // where it looks); this ordering is the one that does not.
      cache.put(CacheKey(titleB, Some(2026)), MovieRecord(
        tmdbId = Some(9912),
        data = Map[Source, SourceData](KinoMuranow -> SourceData(
          title = Some(titleB), showtimes = Seq(Showtime(when, None))))))

      // Row A: the duplicate arriving fresh — same tmdbId, different key, a DIFFERENT cinema.
      cache.put(CacheKey(titleA, Some(2026)), MovieRecord(
        tmdbId = Some(9912),
        data = Map[Source, SourceData](Multikino -> SourceData(
          title = Some(titleA), showtimes = Seq(Showtime(when, None))))))

      // Wherever the merge settled, both cinemas' showtimes must still be there.
      val surviving = (screenings.findForFilm(idA) ++ screenings.findForFilm(idB))
        .collect { case (slotKey, showtimes) if showtimes.nonEmpty => slotKey }.toSet
      info(s"after merge: A($idA)=${screenings.findForFilm(idA).keySet} B($idB)=${screenings.findForFilm(idB).keySet}")

      withClue(s"a merge must union the two rows' cinemas, not delete the loser's showtimes — surviving=$surviving: ") {
        surviving should contain (Multikino.displayName)
        surviving should contain (KinoMuranow.displayName)
      }
      cache.stop()
    } finally {
      Seq(idA, idB).foreach { id => screenings.deleteFilm(id); slots.deleteFilm(id) }
      Seq(titleA, titleB).foreach { t =>
        Await.ready(db.getCollection("movies")
          .deleteMany(Filters.regex("_id", s"^${titleNormalizer.sanitize(t)}\\|")).toFuture(), 10.seconds)
      }
      client.close()
    }
  }
}
