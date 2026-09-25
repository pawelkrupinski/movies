package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{MovieRecord, SourceData, Tmdb}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import tools.Env

/**
 * A resolution that collides with a SIBLING document's `tmdbId` must refuse the
 * WHOLE write, `movie_slots` included — not land the slot and then discover the
 * `movies` write collides.
 *
 * That ordering (slots first, `movies` second, the duplicate-key failure on the
 * second swallowed with a log line) stranded three prod films permanently
 * invisible on 2026-09-16: each kept its previous `tmdbId: null`, while its
 * `movie_slots` `TMDB` slot silently carried a match the `movies` document never
 * recorded. Nothing ever revisited them — `UnresolvedTmdbReaper` treats
 * `tmdbId`+`tmdbAttempt` both absent as "never even tried" — so the read model
 * pruned their cards while their cinemas still sold tickets. See
 * `docs/misresolution-sweep.md`.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class MovieRepositoryDuplicateTmdbIdDoesNotOrphanSlotIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val isolatedDb = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "movies-dup-tmdbid-spec")

  private val db = isolatedDb.database
  override protected def afterAll(): Unit = {
    isolatedDb.drop()
    super.afterAll()
  }

  private val slots = new MongoSlotsRepository(Some(db))
  private val repository = new MongoMovieRepository(Some(db),
    screenings = Some(new MongoScreeningsRepository(Some(db))),
    slots      = Some(slots),
    normalizer = titleNormalizer)

  private val titleA = "__dup-tmdbid-sentinel-a__"
  private val titleB = "__dup-tmdbid-sentinel-b__"
  private val year   = Some(2026)
  private val sharedTmdbId = 999999

  "an upsert that collides with another document's tmdbId" should
    "refuse the whole write and leave that document's movie_slots untouched" in {
    val idA = StoredMovieRecord.keyFor(titleA, year, titleNormalizer)
    val idB = StoredMovieRecord.keyFor(titleB, year, titleNormalizer)
    try {
      // Film A claims the tmdbId first.
      repository.upsert(titleA, year, MovieRecord(tmdbId = Some(sharedTmdbId)))
      withClue("film A's own resolution must land: ") {
        repository.findByIdChecked(FilmId(idA))._1.flatMap(_.record.tmdbId) shouldBe Some(sharedTmdbId)
      }

      // Film B exists, unresolved — every scraped film starts this way.
      repository.upsert(titleB, year, MovieRecord())

      // Film B's own resolution now (wrongly) lands on the SAME tmdbId A already holds,
      // carrying a `Tmdb` slot along with it — the shape a real resolution takes.
      val wrongMatch = SourceData(title = Some(titleA), synopsis = Some("actually A's film"))
      repository.upsert(titleB, year, MovieRecord(tmdbId = Some(sharedTmdbId), data = Map(Tmdb -> wrongMatch)))

      withClue("the colliding write must not set tmdbId on B: ") {
        repository.findByIdChecked(FilmId(idB))._1.flatMap(_.record.tmdbId) shouldBe None
      }
      withClue("and must not leave a TMDB slot behind that B's movies document never recorded: ") {
        slots.findForFilm(idB).get(Tmdb.displayName) shouldBe None
      }
    } finally {
      repository.delete(titleA, year)
      repository.delete(titleB, year)
    }
  }
}
