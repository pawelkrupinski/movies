package integration

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.MongoReadModelRepository
import tools.Env
import tools.Eventually.eventually

/**
 * Read-only check that the read model's id-only projections
 * (`findAllMovieIds` / `findAllScreeningRefs`, which the reconcile prune uses to
 * stay off the heap) return the SAME ids as a full `findAllMovies` /
 * `findAllScreenings` decode — i.e. the server-side `{_id}` / `{_id, filmId}`
 * BsonDocument projection is faithful. Requires MONGODB_URI; skips otherwise.
 *
 * Purely read-only against the live `web_movies` / `web_screenings`: it writes
 * nothing, so there are no sentinels to purge.
 *
 * BOTH ID CHECKS SETTLE, and they have to. Each compares TWO SEPARATE READS of a live
 * collection, and `IntegrationTest / parallelExecution` is on — so a sibling spec writing
 * or purging its own sentinel between the two reads makes them disagree about exactly that
 * row while both are perfectly faithful. That is what failed on CI on 2026-09-04:
 * `Set()` against `Set("backfillstitchprobe|1904")`, the sentinel
 * `BackfillReadModelStitchIntegrationSpec` creates and deletes. A GENUINE projection
 * infidelity is a property of the server-side `{_id}` / `{_id, filmId}` projection and
 * never settles; a concurrent write settles on the next read. Retrying is what tells the
 * two apart, and asserting on a single read is what conflated them.
 */
class ReadModelRepositoryIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
  private val rm     = new MongoReadModelRepository(Some(db))

  override protected def afterAll(): Unit = try { rm.close(); client.close() } finally super.afterAll()

  "findAllMovieIds" should "project the same ids as a full findAllMovies decode" in {
    // A wider poll than the helper's default: each attempt is TWO whole-collection reads,
    // so hammering them every 20ms would cost more than the race it is waiting out.
    eventually(rm.findAllMovieIds().toSet shouldBe rm.findAllMovies().map(_._id).toSet,
      timeoutMs = 10000, pollMs = 250)
  }

  "findAllScreeningRefs" should "project the same (_id, filmId) pairs as a full findAllScreenings decode" in {
    eventually(rm.findAllScreeningRefs().map(r => r._id -> r.filmId).toSet shouldBe
      rm.findAllScreenings().map(s => s._id -> s.filmId).toSet,
      timeoutMs = 10000, pollMs = 250)
  }

  // findAllScreenings is now keyset-PAGED (KeysetScan), not one unbounded find().toFuture().
  // At corpus scale that single cursor timed out at 60s and returned Seq.empty, so the
  // projector's boot SEED was empty and every boot reproject rewrote the whole ~6.5k-screening
  // corpus (the reproject's phantom did_work). The 60s timeout only reproduces at prod scale
  // (7 real timeouts logged 2026-07-04); this guards the paging MECHANISM instead — every row
  // comes back exactly once across page boundaries (batchSize forced to 2 over 5 sentinels → 3
  // pages), the boundary correctness the empty-seed fix depends on.
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

}
