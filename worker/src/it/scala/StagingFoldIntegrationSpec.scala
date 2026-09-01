package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Multikino, SourceData}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{SingleObservableFuture, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoSlotsRepository, StoredMovieRecord}
import services.staging.StagingRepository
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
  private val winner = StoredMovieRecord.idFor(title, Some(2026), titleNormalizer)
  private val loser  = StoredMovieRecord.idFor(title, Some(2025), titleNormalizer)

  private def sd(t: String) = SourceData(title = Some(t))

  it should "keep a retired key's screenings — the winner has not inherited them yet" in {
    FoldFixture.withFold(titleNormalizer.sanitize(title)) { fold =>
      import fold.{movies, staging, slots, screenings}
      // Both year-variants exist in `movies`, each with cinemas in the side collections.
      Seq(winner, loser).foreach { id =>
        Await.result(movies.replaceOne(Filters.eq("_id", id),
          org.mongodb.scala.Document("_id" -> id, "tmdbId" -> 4242, "sourceData" -> org.mongodb.scala.Document(),
            "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
          new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
        // The slot's title is the cinema's reported title FOR THIS FILM, so it has to
        // sanitize into the film's group — the fold now plans against the stitched view
        // (`MongoStagingFolder.withStitchedCinemas`), so a placeholder here is a cinema
        // claiming to show a different film and the two year-variants stop clustering.
        slots.replaceFilm(id, Map(Multikino.displayName -> sd(title)))
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

      fold.folder().foldGroup(title)

      val survivors = Await.result(movies.find(Filters.regex("_id",
        s"^${titleNormalizer.sanitize(title)}\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      survivors      should not be empty   // premise: the fold did run
      survivors.size shouldBe 1            // …and did collapse the two into one

      // The retired key's cinemas must reach the WINNER. Deleting them outright is what
      // emptied prod (@8033e39c6: PL 39,413 → 18,161 showtimes) because most retirements
      // are re-keys and the winner's side rows are not written by this transaction —
      // so for a while the loser's rows are the only copy. Leaving them where they are
      // was the safe half of that lesson, but it is not the whole answer: the winner then
      // never inherits the board, and whether a film keeps its showtimes comes down to
      // whether the fold happened to retire it, which is a race against enrichment. The
      // migration is the fix the revert commit itself called for — "MIGRATING the loser's
      // rows onto the winner, not deleting them".
      val survivor = survivors.head
      withClue(s"the fold retired a key into $survivor but the winner did not inherit its board: ") {
        screenings.findForFilm(survivor) should not be empty
        slots.findForFilm(survivor)      should not be empty
      }
      Seq(winner, loser).filterNot(survivors.contains).foreach { retired =>
        withClue(s"$retired was retired and its rows migrated, so nothing may be left behind: ") {
          screenings.findForFilm(retired) shouldBe empty
          slots.findForFilm(retired)      shouldBe empty
        }
      }
      }
  }

  /** The other half of the same seam. Above: the fold must not DELETE side rows it is not
   *  replacing. Here: it must WRITE them for the film it graduates.
   *
   *  The transaction writes `movies` directly, embedding the film's `sourceData` and
   *  touching neither side collection. `SlotsRepository.merge` tolerates that (it unions
   *  stored with embedded), but `ScreeningsRepository.stitch` treats `screenings` as
   *  authoritative and EMPTIES the showtimes of any slot it has no row for — so a folded
   *  film read back through the repository had its cinemas and none of its showtimes, and
   *  rendered as a title with nothing under it until that cinema's next scrape. Measured
   *  on prod 2026-08-06 (`ktoscalkiemobcy|2024`: 0 slots, 0 screenings) and reproduced by
   *  every convergence order-independence pass once the harness was wired for the split.
   *
   *  Read back through `findByIdChecked`, deliberately: reading the raw document would
   *  show the embedded showtimes the transaction wrote and pass while every real reader
   *  saw none. */
  it should "give a graduated film its showtimes in `screenings`, not just embedded" in {
    FoldFixture.withFold(titleNormalizer.sanitize(newcomerTitle)) { fold =>
      import fold.{movies, staging, screenings, db}
      val repository = new services.movies.MongoMovieRepository(Some(db), fallbackToOwnInit = false,
      normalizer = titleNormalizer,
      screenings = Some(screenings), slots = Some(new MongoSlotsRepository(Some(db))))
      seedConcludedNewcomer(staging)

      fold.folder().foldGroup(newcomerTitle)

      val folded = Await.result(movies.find(Filters.regex("_id",
        s"^${titleNormalizer.sanitize(newcomerTitle)}\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      folded should not be empty   // premise: the fold really did graduate it

      folded.foreach { id =>
        withClue(s"$id graduated out of staging, but a reader sees a film with no showtimes: ") {
          val (row, readOk) = repository.findByIdChecked(id)
          readOk shouldBe true
          row.map(_.record.cinemaData.values.map(_.showtimes.size).sum).getOrElse(0) should be > 0
        }
      }
      }
  }

  /** A fold whose group is BIGGER THAN ONE DOCUMENT — the case that broke the United
   *  States and could not be seen from anywhere else.
   *
   *  The fold writes `movies` directly, in its own session, because its upserts and its
   *  staging deletes have to commit together and the repository's write path is not
   *  session-aware. That bypass means every rule `MovieRepository.upsert` applies on the
   *  way to disk has to be applied here by hand, and the storage rule was not: the
   *  document went in with each venue's board inline. That is linear in the number of
   *  venues screening the film — fine at Poland's 151, fatal at the United States' 5,031,
   *  where on 2026-09-01 the fold of `Avengers: Doomsday` threw
   *  `BsonMaximumSizeExceededException: Payload document size is larger than maximum of
   *  16793600` on every attempt. The exception carries no transient label, so
   *  `foldWithRetry` abandoned after one and rethrew; the staging rows were therefore
   *  never consumed, so `StagingReaper` enqueued the same fold on the next tick, and the
   *  next, with no backoff and no give-up.
   *
   *  Two cinemas, each holding a board that fits in its own staging row and does not fit
   *  when unioned with the other — which is exactly the shape a wide release has, at a
   *  scale that reproduces in seconds rather than needing five thousand venues. The
   *  assertion is simply that the fold HAPPENS: before the fix this suite failed here
   *  with the driver's exception, having consumed nothing.
   *
   *  Its sibling above proves the showtimes survive the trip; this one proves there is a
   *  trip to survive. `StagingFoldDocumentSizeSpec` measures the same ceiling against the
   *  real catalogue, in bytes, without needing a replica set. */
  it should "fold a film whose venues carry more board than one document can hold" in {
    FoldFixture.withFold(titleNormalizer.sanitize(oversizeTitle)) { fold =>
      import fold.{screenings, staging}
      val cinemas = Seq(Multikino, models.Helios)
      cinemas.foreach(seedOversizeRow(staging, _))

      fold.folder().foldGroup(oversizeTitle)

      withClue("the fold did not consume its rows — it threw before committing, which is " +
               "the overflow this test exists for: ") {
        Await.result(staging.find(Filters.regex("_id",
          s".*${titleNormalizer.sanitize(oversizeTitle)}.*")).toFuture(), 30.seconds) shouldBe empty
      }
      val folded = Await.result(fold.movies.find(Filters.regex("_id",
        s"^${titleNormalizer.sanitize(oversizeTitle)}\\|")).toFuture(), 30.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      folded should have size 1
      withClue("the fold committed but filed none of the board it folded: ") {
        screenings.findForFilm(folded.head).values.map(_.size).sum shouldBe cinemas.size * OversizeShowtimes
      }
      }
  }

  private val oversizeTitle = "__foldoversize-it-sentinel__"

  /** Enough showtimes that TWO of these slots cannot share one BSON document, and few
   *  enough that building them costs a second. The padding rides on `room`, a plain
   *  string field, so the weight is in the board itself — a fat synopsis would overflow
   *  the same document and prove nothing, since it is not what the storage rule removes. */
  private val OversizeShowtimes = 20000
  private val OversizeRoom      = "Auditorium " + ("x" * 420)

  private def seedOversizeRow(staging: org.mongodb.scala.MongoCollection[org.mongodb.scala.Document],
                              cinema: models.Cinema): Unit = {
    val board = (1 to OversizeShowtimes).map { n =>
      org.mongodb.scala.Document(
        "dateTime" -> java.util.Date.from(
          java.time.LocalDateTime.of(2026, 8, 1, 12, 0).plusMinutes(n.toLong)
            .toInstant(java.time.ZoneOffset.UTC)),
        "room" -> OversizeRoom).toBsonDocument
    }
    val id = s"${cinema.displayName}|${titleNormalizer.sanitize(oversizeTitle)}|2026"
    Await.result(staging.replaceOne(Filters.eq("_id", id),
      org.mongodb.scala.Document("_id" -> id, "tmdbId" -> 5253, "title" -> oversizeTitle,
        "year" -> 2026,
        "sourceData" -> org.mongodb.scala.Document(cinema.displayName ->
          org.mongodb.scala.Document("title" -> oversizeTitle,
            "showtimes" -> org.mongodb.scala.bson.BsonArray.fromIterable(board))),
        "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
      new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 30.seconds)
  }

  /** A newcomer incubating in staging, concluded, with a real board — and NO `movies` row
   *  yet, so the fold is what creates it. Shared by the two tests above, which ask
   *  opposite questions of the same fold and would otherwise each carry their own copy of
   *  this seeding. */
  private def seedConcludedNewcomer(staging: org.mongodb.scala.MongoCollection[org.mongodb.scala.Document]): Unit = {
    val when      = models.Showtime(java.time.LocalDateTime.of(2026, 8, 1, 20, 0), None)
    val stagingId = s"${Multikino.displayName}|${titleNormalizer.sanitize(newcomerTitle)}|2026"
    Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
      org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> 5252, "title" -> newcomerTitle,
        "year" -> 2026,
        "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
          org.mongodb.scala.Document("title" -> newcomerTitle,
            // A real BSON date — the showtime codec reads DATE_TIME, and a string here
            // aborts the fold's transaction rather than failing the assertion.
            "showtimes" -> org.mongodb.scala.bson.BsonArray.fromIterable(Seq(
              org.mongodb.scala.Document("dateTime" -> java.util.Date.from(
                when.dateTime.toInstant(java.time.ZoneOffset.UTC))).toBsonDocument)))),
        "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
      new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
  }

  private val newcomerTitle = "__foldnewcomer-it-sentinel__"

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
    FoldFixture.withFold(blindSanitize) { fold =>
      import fold.{movies, staging, slots}
      val existing = StoredMovieRecord.idFor(blindTitle, Some(2026), titleNormalizer)
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

      fold.folder().foldGroup(blindTitle)

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
      }
  }

  // The hint must not change WHICH rows fold — it only says which to READ. `foldOnce`
  // used to scan the entire staging collection inside every fold transaction, which is
  // 3,629 folds over 28,572 rows on the UK convergence leg: ~104M decodes and 47 of the
  // leg's 62 minutes. Passing the group's ids removes that, but only if the selection is
  // still `selectStagingGroup`'s — a hint that silently narrowed the group would be the
  // quietest possible corruption.
  it should "fold the same group whether or not it is told which rows to read" in {
    val hintTitle = "Fold Hint Sentinel"
    val anchor    = titleNormalizer.sanitize(hintTitle)
    FoldFixture.withFold(anchor) { fold =>
      import fold.db
      val repository = new services.staging.MongoStagingRepository(Some(db), titleNormalizer)
      val folder     = fold.folder()

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

      val unhinted = foldedIds(folder.foldGroup(hintTitle))
      val hinted   = foldedIds(folder.foldGroup(hintTitle,
                       Some(repository.findByAnchor(anchor).map(_.id).toSet)))
      // Deliberately over-broad: every staged id, not only this group's.
      val tooWide  = foldedIds(folder.foldGroup(hintTitle, Some(repository.findAll().map(_.id).toSet)))

      withClue("the fixture must actually fold something: ") { unhinted should not be empty }
      withClue("a hinted fold must land exactly what an unhinted one lands: ") { hinted shouldBe unhinted }
      withClue("an over-broad hint must not widen the group: ") { tooWide shouldBe unhinted }
      }
  }
}