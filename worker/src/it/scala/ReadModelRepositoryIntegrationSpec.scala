package integration

import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.MongoReadModelRepository
import tools.Eventually.eventually

/**
 * Read-only check that the read model's id-only projections
 * (`findAllMovieIds` / `findAllScreeningRefs`, which the reconcile prune uses to
 * stay off the heap) return the SAME ids as a full `findAllMovies` /
 * `findAllScreenings` decode — i.e. the server-side `{_id}` / `{_id, filmId}`
 * BsonDocument projection is faithful. Requires MONGODB_URI; skips otherwise.
 *
 * Runs in a database of its own, dropped in `afterAll`. The id checks once read the
 * SHARED `web_movies` / `web_screenings` and had to retry, because a sibling spec writing
 * its own sentinel between the two reads made them disagree (CI, 2026-09-04:
 * `Set()` against `Set("backfillstitchprobe|1904")`). In a database nobody else writes,
 * both reads see exactly the rows seeded here, so a single read is the assertion.
 */
class ReadModelRepositoryIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "readmodel-repository")
  private val db       = isolated.database
  private val rm       = new MongoReadModelRepository(Some(db))

  override protected def afterAll(): Unit = try { rm.close(); isolated.drop() } finally super.afterAll()

  private def resolvedMovie(id: String): models.ResolvedMovie = models.ResolvedMovie(_id = id, title = "Diuna",
    originalTitle = None, posterUrl = None, fallbackPosterUrls = Nil, runtimeMinutes = None, releaseYear = Some(2021),
    genres = Nil, countries = Nil, directors = Nil, cast = Nil, synopsis = None, trailerUrls = Nil,
    ratings = models.ResolvedRatings(None, None, None, "", None, "", None, ""), weightedRating = 0.0)

  "findAllMovieIds" should "project the same ids as a full findAllMovies decode" in {
    val ids = (0 until 3).map(i => s"__it-rm-movie-${i}__")
    ids.map(resolvedMovie).foreach(rm.upsertMovie)
    rm.findAllMovies().map(_._id).toSet shouldBe ids.toSet
    rm.findAllMovieIds().toSet shouldBe ids.toSet
  }

  "findAllScreeningRefs" should "project the same (_id, filmId) pairs as a full findAllScreenings decode" in {
    import models.CityScreening
    val pairs = (0 until 3).map(i => s"__it-rm-pair-${i}__" -> s"__it-rm-pair-film-${i % 2}__")
    pairs.foreach { case (id, film) =>
      rm.upsertScreening(CityScreening(_id = id, filmId = film, city = "poznan", cinema = "Cinema", filmUrl = None, showtimes = Nil))
    }
    try {
      rm.findAllScreenings().map(s => s._id -> s.filmId).toSet shouldBe pairs.toSet
      rm.findAllScreeningRefs().map(r => r._id -> r.filmId).toSet shouldBe pairs.toSet
    } finally pairs.foreach { case (id, _) => rm.deleteScreening(id) }
  }

  // findAllScreenings is now keyset-PAGED (KeysetScan), not one unbounded find().toFuture().
  // At corpus scale that single cursor timed out at 60s and returned Seq.empty, so the
  // projector's boot SEED was empty and every boot reproject rewrote the whole ~6.5k-screening
  // corpus (the reproject's phantom did_work). The 60s timeout only reproduces at prod scale
  // (7 real timeouts logged 2026-07-04); this guards the paging MECHANISM instead — every row
  // comes back exactly once across page boundaries (batchSize forced to 2 over 5 sentinels → 3
  // pages), the boundary correctness the empty-seed fix depends on.
  // NO SECONDARY INDEXES ON `web_screenings`. Two were created at every boot, on `city` and
  // `filmId`, for queries that do not exist: every read in `MongoReadModelRepository` goes
  // through `_id`. `$indexStats` on prod agreed — 0 operations against either over 73 hours,
  // against 246,309 on `_id_` — and this is the collection the projector rewrites, so each
  // one was an index write on every upsert and delete, paid forever for nothing.
  //
  // THE SHARE CARD REACHES web_movies, and the janitor's projected read of it is faithful: the
  // card's path and version. Its own
  // database, dropped afterwards.
  "web_movies" should "carry a film's share card, and hand the janitor its projected refs" in {
    import services.readmodel.ShareCardRef
    val ownDb   = tools.IntegrationCorpusDatabase.named(mongoTarget, "readmodel-sharecards")
    val client2 = MongoClient(mongoTarget.uri.value)
    val fresh   = new MongoReadModelRepository(Some(client2.getDatabase(ownDb)))
    try {
      val film = resolvedMovie("__it-rm-sharecard__").copy(posterUrl = Some("https://cdn.example/a.jpg"),
        fallbackPosterUrls = Seq("https://cdn.example/b.jpg"), shareCard = Some("f1.jpg?v=0123456789abcdef"),
        shareCardPending = true)
      val bare = film.copy(_id = "__it-rm-nocard__", posterUrl = None, fallbackPosterUrls = Nil, shareCard = None,
        shareCardPending = false)
      fresh.upsertMovie(film); fresh.upsertMovie(bare)
      fresh.findAllMovies().sortBy(_._id) shouldBe Seq(bare, film)
      fresh.findAllShareCardRefsChecked() shouldBe ((Seq(
        ShareCardRef("__it-rm-nocard__", None),
        ShareCardRef("__it-rm-sharecard__", Some("f1.jpg?v=0123456789abcdef"))),
        true))
    } finally {
      scala.concurrent.Await.ready(client2.getDatabase(ownDb).drop().toFuture(),
        scala.concurrent.duration.Duration(10, "seconds"))
      fresh.close(); client2.close()
    }
  }

  // Its own database, because the assertion is about what a BOOT creates: the shared one
  // still carries the indexes earlier builds made, and dropping them there would be a
  // destructive act in a spec.
  "the read model" should "create no secondary index on web_screenings" in {
    import models.CityScreening
    val ownDb   = tools.IntegrationCorpusDatabase.named(mongoTarget, "readmodel-indexes")
    val client2 = MongoClient(mongoTarget.uri.value)
    val fresh   = new MongoReadModelRepository(Some(client2.getDatabase(ownDb)))
    try {
      // The collection does not exist until something is written to it.
      fresh.upsertScreening(CityScreening(_id = "__it-rm-index__", filmId = "__it-rm-index-film__",
        city = "poznan", cinema = "Cinema", filmUrl = None, showtimes = Nil))
      val names = scala.concurrent.Await.result(
        client2.getDatabase(ownDb).getCollection("web_screenings").listIndexes().toFuture(),
        scala.concurrent.duration.Duration(10, "seconds")).flatMap(_.get("name").map(_.asString().getValue)).toSet
      withClue(s"an index nothing queries still costs a write on every projector upsert — " +
               s"re-add one only alongside the query that needs it (found $names): ") {
        names shouldBe Set("_id_")
      }
    } finally {
      scala.concurrent.Await.ready(client2.getDatabase(ownDb).drop().toFuture(),
        scala.concurrent.duration.Duration(10, "seconds"))
      fresh.close(); client2.close()
    }
  }

  "findAllScreenings" should "page across batch boundaries, returning every written screening exactly once in _id order" in {
    import models.CityScreening
    val paged = new MongoReadModelRepository(Some(db), findAllBatchSize = 2)
    val ids   = (0 until 5).map(i => s"__it-rm-page-${i}__")
    val docs  = ids.map(id => CityScreening(_id = id, filmId = "__it-rm-page-film__",
      city = "poznan", cinema = "Cinema", filmUrl = None, showtimes = Nil))
    try {
      docs.foreach(paged.upsertScreening)
      val got = paged.findAllScreenings().filter(_._id.startsWith("__it-rm-page-")).map(_._id)
      got shouldBe ids            // all 5, _id-sorted, no dup or skip across the 3 keyset pages
    } finally ids.foreach(paged.deleteScreening)
  }

  // The id-only projections the PRUNE reads were the last unpaged reads in the file.
  // findAllScreenings got KeysetScan when its single cursor timed out at corpus scale;
  // findAllScreeningRefs did not, over that same largest collection. Same mechanism
  // guard — every ref back exactly once across page boundaries — because a SHORT read
  // here silently shrinks the prune instead of failing. `findAllMovieIds` goes through
  // the same `pagedIds` helper, so this covers both.
  "findAllScreeningRefs" should "page across batch boundaries, returning every ref exactly once" in {
    import models.CityScreening
    val paged = new MongoReadModelRepository(Some(db), findAllBatchSize = 2)
    val ids   = (0 until 5).map(i => s"__it-rm-ref-${i}__")
    val docs  = ids.map(id => CityScreening(_id = id, filmId = "__it-rm-ref-film__",
      city = "poznan", cinema = "Cinema", filmUrl = None, showtimes = Nil))
    try {
      docs.foreach(paged.upsertScreening)
      val refs = paged.findAllScreeningRefs().filter(_._id.startsWith("__it-rm-ref-"))
      refs.map(_._id) shouldBe ids
      refs.map(_.filmId).distinct shouldBe Seq("__it-rm-ref-film__")
    } finally ids.foreach(paged.deleteScreening)
  }

  // THE WEB'S BOOT GAP, against a real change stream. `WebReadModel.start` hydrates, then
  // watches; a watch opened "from now" missed every write between the two (2026-09-23: two
  // Włodawa screenings, served 30 minutes late). A checkpoint taken before the write must make
  // the watch replay it — the server's cluster time, handed to `startAtOperationTime`.
  //
  // In its own database: the watch sees its whole collection, and in the shared one every
  // sibling spec's `web_screenings` write reaches it.
  "a watch from a stream checkpoint" should "replay a write made after the checkpoint but before the watch opened" in
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "readmodel-checkpoint") { own =>
      import models.CityScreening
      val isolated   = new MongoReadModelRepository(Some(own))
      val id         = "__it-rm-checkpoint__"
      val seen       = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      val checkpoint = isolated.streamCheckpoint()
      checkpoint shouldBe defined   // a replica set reports its operation time; a standalone cannot stream at all
      isolated.upsertScreening(CityScreening(_id = id, filmId = "__it-rm-checkpoint-film__",
        city = "poznan", cinema = "Cinema", filmUrl = None, showtimes = Nil))
      val watch = isolated.watchScreenings(s => { seen.add(s._id); () }, _ => (), from = checkpoint)
      try eventually(seen.contains(id) shouldBe true, timeoutMs = 10000, pollMs = 100)
      finally watch.foreach(_.close())
    }

}
