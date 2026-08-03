package integration

import services.movies.SingleCountryNormalizer.{titleNormalizer, given}

import models.{Multikino, SourceData}
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
 * `MongoStagingFolder` against a real replica set — it needs transactions, so this is the
 * only layer that reaches it at all.
 *
 * The case under test is the one the unit specs structurally cannot see, and it is the
 * one that cost prod: the fold retires a `movies` row with a direct in-transaction
 * `deleteOne`, and most of those retirements are RE-KEYS (`foo|` collapsing onto
 * `foo|2026` once TMDB concludes the year), not films leaving. The film's showtimes are
 * still stored under the OLD id at that moment — the winner's side rows are written later,
 * by `MovieRepository.upsert` — so a fold that "tidies up" the loser's screenings destroys
 * them. Shipped @8033e39c6, it took PL from 39,413 upcoming showtimes to 18,161 and UK
 * from 22,250 to 7,226 within twenty minutes of deploy, hitting hardest exactly where the
 * most folding was happening.
 */
class StagingFoldIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri    = Env.get("MONGODB_URI").get
  private val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

  // Two year-variants of one film. `planGroup` collapses them onto the TMDB year, so the
  // other is a merge loser — deleted in-transaction, exactly the bypass under test.
  //
  // The name deliberately does NOT sanitize to an `integrationtest…` prefix, and that is
  // load-bearing: `MovieRepositoryIntegrationSpec` purges `^integrationtest` from `movies`
  // in its own beforeAll/afterAll, and sbt runs the it suites in PARALLEL against the one
  // `kinowo_it` database. A conventionally-named sentinel here passed alone and failed
  // alongside that spec — its rows deleted mid-fold by the neighbour's purge. Anything new
  // sharing this database wants a prefix of its own, and its own cleanup.
  private val title  = "__foldorphans-it-sentinel__"
  private val winner = StoredMovieRecord.idFor(title, Some(2026))
  private val loser  = StoredMovieRecord.idFor(title, Some(2025))

  private def sd(t: String) = SourceData(title = Some(t))

  it should "keep a retired key's screenings — the winner has not inherited them yet" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val slots      = new MongoSlotsRepository(Some(db))
    val screenings = new MongoScreeningsRepository(Some(db))
    val staging    = db.getCollection(StagingRepository.Collection)
    val movies     = db.getCollection(services.movies.MovieRepository.Collection)
    try {
      // Both year-variants exist in `movies`, each with cinemas in the side collections.
      Seq(winner, loser).foreach { id =>
        Await.result(movies.replaceOne(Filters.eq("_id", id),
          org.mongodb.scala.Document("_id" -> id, "tmdbId" -> 4242, "sourceData" -> org.mongodb.scala.Document(),
            "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
          new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
        slots.replaceFilm(id, Map(Multikino.displayName -> sd("cinema")))
        screenings.replaceFilm(id, Map(Multikino.displayName -> Seq(
          models.Showtime(java.time.LocalDateTime.of(2026, 8, 1, 20, 0), None))))
      }
      slots.findForFilm(loser)      should not be empty
      screenings.findForFilm(loser) should not be empty

      // A staging row for the same group, concluded on the winner's year, drives the fold.
      val stagingId = s"${Multikino.displayName}|${titleNormalizer.sanitize(title)}|2026"
      Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
        org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> 4242,
          "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
            org.mongodb.scala.Document("title" -> title)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      new MongoStagingFolder(connection).foldGroup(title)

      val survivors = Await.result(movies.find(Filters.regex("_id",
        s"^${titleNormalizer.sanitize(title)}\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      survivors      should not be empty   // premise: the fold did run
      survivors.size shouldBe 1            // …and did collapse the two into one

      // The retired key's cinemas must SURVIVE the fold. The winner's side rows are not
      // written by this transaction, so until `MovieRepository.upsert` next writes that
      // film these rows are the only copy of its showtimes — deleting them here is what
      // emptied prod. They become inert once the winner is written; `ReapOrphanedFilmRows`
      // clears them then, out of band, without racing the re-key.
      Seq(winner, loser).filterNot(survivors.contains).foreach { retired =>
        withClue(s"$retired was retired by the fold, but its showtimes are still the only copy: ") {
          screenings.findForFilm(retired) should not be empty
          slots.findForFilm(retired)      should not be empty
        }
      }
    } finally {
      Seq(winner, loser).foreach { id => slots.deleteFilm(id); screenings.deleteFilm(id) }
      Await.ready(movies.deleteMany(Filters.regex("_id",
        s"^${titleNormalizer.sanitize(title)}\\|")).toFuture(), 10.seconds)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*${titleNormalizer.sanitize(title)}.*"))
        .toFuture(), 10.seconds)
      client.close()
    }
  }

  // A second sentinel, with its own cleanup — see the naming note above.
  private val blindTitle = "__foldblind-it-sentinel__"
  // A tmdbId no other suite writes. `MongoStagingFolder.foldOnce` pulls cross-title
  // same-tmdbId siblings from the WHOLE `movies` collection, so a tmdbId is a shared
  // namespace between parallel suites exactly as a title prefix is — and this one was
  // 4243, which `MovieRepositoryIntegrationSpec` also writes. Probed with that
  // neighbour's row resident and nothing happened, so it is latent rather than live —
  // but a tmdbId collision here would fold two unrelated films into one and delete the
  // loser, in whichever suite lost the race, so it does not get to stay a coincidence.
  private val blindTmdbId = 42431
  private def blindSanitize = titleNormalizer.sanitize(blindTitle)

  /**
   * The fold plans against RAW `movies` documents (`StoredMovieDto.toDomain`), never the
   * stitched view. Under the storage split a migrated film's `sourceData` is empty — its
   * cinemas are rows in `movie_slots` — so the fold sees a film that reports no cinemas at
   * all, and `StagingFold.planGroup` picks the surviving key from `canonical`, which derives
   * it from exactly those cinema-reported titles.
   *
   * The expectation was that the fold would therefore key the film on the staging row's
   * spelling while the cache keyed it on the stitched record, and that the corpus would stop
   * being a pure function of what was scraped. It does not: `FilmCanonicalizer.canonical`
   * weighs the ROWS' EXISTING KEYS alongside the slot titles, and the existing key already
   * carries the settled spelling, so the migrated film stays put whatever staging reports.
   *
   * Which is what this pins — and only that. Removing the film's stored cinemas entirely
   * does NOT change the outcome (checked), so the stored cinemas are not what decides it and
   * the name must not claim they are. The reasoning is kept because it is not visible from
   * the code, and the next reader of the raw-`movies` read will draw the same wrong
   * conclusion.
   */
  it should "keep a migrated film on its existing key rather than adopting the staging spelling" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val slots      = new MongoSlotsRepository(Some(db))
    val staging    = db.getCollection(StagingRepository.Collection)
    val movies     = db.getCollection(services.movies.MovieRepository.Collection)
    val existing   = StoredMovieRecord.idFor(blindTitle, Some(2026))
    // The bare spelling is what the cinemas report, and it is the one the settle would key
    // on. It exists ONLY in `movie_slots` — a fully migrated film, which is what prod's
    // corpus is converging to.
    //
    // The staging row must SANITIZE INTO THIS GROUP or the fold never sees it. It used to
    // report "Przeglad: …", and a programme prefix is exactly what `sanitize` does NOT
    // strip (that is `apiQuery`'s job — a decorated edition is a row of its own by
    // design), so `selectStagingGroup` matched nothing, `foldOnce` short-circuited on
    // `stagingRows.isEmpty`, and this test asserted only that the row it had just written
    // still existed. A SHOUTED spelling is the right shape here: same group, different
    // string, so the fold genuinely has to choose between the staging spelling and the
    // stored cinemas'.
    val shouted    = blindTitle.toUpperCase(java.util.Locale.ROOT)
    try {
      Await.result(movies.replaceOne(Filters.eq("_id", existing),
        org.mongodb.scala.Document("_id" -> existing, "tmdbId" -> blindTmdbId,
          "sourceData" -> org.mongodb.scala.Document(),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      slots.replaceFilm(existing, Map(
        Multikino.displayName -> sd(blindTitle),
        models.Helios.displayName -> sd(blindTitle)))

      // One staging row, reporting the DECORATED spelling for the same film.
      val stagingId = s"${models.KinoMuza.displayName}|$blindSanitize|2026"
      Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
        org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> blindTmdbId,
          "sourceData" -> org.mongodb.scala.Document(models.KinoMuza.displayName ->
            org.mongodb.scala.Document("title" -> shouted)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      new MongoStagingFolder(connection).foldGroup(blindTitle)

      val survivors = Await.result(movies.find(Filters.regex("_id", s"^$blindSanitize\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      withClue(s"survivors=$survivors — the fold saw the film's two stored cinemas reporting " +
               s"'$blindTitle' and still keyed it on the single staging row's spelling: ") {
        survivors shouldBe Seq(existing)
      }
      // …and the fold actually RAN. Without this the assertion above is satisfied just as
      // well by a fold that consumed nothing, which is precisely how it used to pass.
      withClue("the fold consumed no staging row, so it never chose anything: ")(
        Await.result(staging.find(Filters.eq("_id", stagingId)).toFuture(), 10.seconds) shouldBe empty)
    } finally {
      slots.deleteFilm(existing)
      Await.ready(movies.deleteMany(Filters.regex("_id", s"^$blindSanitize\\|")).toFuture(), 10.seconds)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*$blindSanitize.*")).toFuture(), 10.seconds)
      client.close()
    }
  }

  // The hint must not change WHICH rows fold — it only says which to READ. `foldOnce`
  // used to scan the entire staging collection inside every fold transaction, which is
  // 3,629 folds over 28,572 rows on the UK convergence leg: ~104M decodes and 47 of the
  // leg's 62 minutes. Passing the group's ids removes that, but only if the selection is
  // still `selectStagingGroup`'s — a hint that silently narrowed the group would be the
  // quietest possible corruption.
  it should "fold the same group whether or not it is told which rows to read" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val repository = new services.staging.MongoStagingRepository(Some(db))
    val folder     = new MongoStagingFolder(connection)
    val hintTitle  = "Fold Hint Sentinel"
    val anchor     = titleNormalizer.sanitize(hintTitle)

    val staged = db.getCollection(StagingRepository.Collection)
    // The same raw shape the tests above use — a CONCLUDED row (tmdbId present), which is
    // what makes a group foldable. Writing through the repository leaves it unconcluded,
    // so nothing folds and the assertion has nothing to compare.
    def stage(): Unit = Seq(2025, 2026).foreach { year =>
      val id = s"${Multikino.displayName}|$anchor|$year"
      Await.result(staged.replaceOne(Filters.eq("_id", id),
        org.mongodb.scala.Document("_id" -> id, "tmdbId" -> 778101,
          "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
            org.mongodb.scala.Document("title" -> hintTitle)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
    }

    val movies = db.getCollection(services.movies.MovieRepository.Collection)

    // Asserted on the EFFECT — which `movies` rows exist afterwards — not on the return
    // value, which carries only brand-new promotions and is empty on a re-fold.
    def foldedIds(fold: => Unit): Set[String] = {
      Await.result(movies.deleteMany(Filters.regex("_id", s"^$anchor\\|")).toFuture(), 10.seconds)
      stage()
      fold
      Await.result(movies.find(Filters.regex("_id", s"^$anchor\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue)).toSet
    }

    try {
      val unhinted = foldedIds(folder.foldGroup(hintTitle))
      val hinted   = foldedIds(folder.foldGroup(hintTitle,
                       Some(repository.findByAnchor(anchor).map(_.id).toSet)))
      // Deliberately over-broad: every staged id, not only this group's.
      val tooWide  = foldedIds(folder.foldGroup(hintTitle, Some(repository.findAll().map(_.id).toSet)))

      withClue("the fixture must actually fold something: ") { unhinted should not be empty }
      withClue("a hinted fold must land exactly what an unhinted one lands: ") { hinted shouldBe unhinted }
      withClue("an over-broad hint must not widen the group: ") { tooWide shouldBe unhinted }
    } finally {
      Await.result(staged.deleteMany(Filters.regex("_id", s".*\\|$anchor\\|.*")).toFuture(), 10.seconds)
      Await.result(movies.deleteMany(Filters.regex("_id", s"^$anchor\\|")).toFuture(), 10.seconds)
      client.close()
    }
  }
}