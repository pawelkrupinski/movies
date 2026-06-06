package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards `MongoMovieRepo.idRanges` — the partitioning behind `findAll`'s
 * parallel ranged cursors.
 *
 * `findAll` reads in two phases: discover the sorted `_id` list, then fire
 * one ranged `find()` per chunk. Those phases aren't a snapshot — the
 * `movies` collection is written concurrently (enrichment write-through) —
 * so a document can be committed *between* the two phases. The ranges must
 * therefore tile the whole `_id` space with no gaps: every id, including one
 * that wasn't in the discovery list and sorts between two chunk boundaries,
 * must fall in exactly one range. The earlier `gte(head) … lte(last)`
 * bounding left a gap between adjacent chunks and silently dropped such a
 * concurrent insert.
 *
 * Pure logic, no Mongo — membership mirrors the half-open `gte(lower) … lt(upper)`
 * filter the production code builds from each range.
 */
class MongoMovieRepoIdRangesSpec extends AnyFlatSpec with Matchers {

  /** Mirror of the `[lower, upper)` filter `findAll` builds per range. */
  private def covers(range: (Option[String], Option[String]), id: String): Boolean = {
    val (lower, upper) = range
    lower.forall(id >= _) && upper.forall(id < _)
  }

  private def coveringCount(ranges: Seq[(Option[String], Option[String])], id: String): Int =
    ranges.count(covers(_, id))

  "idRanges" should "tile the whole id space with no gaps and no overlaps" in {
    val ids    = Vector("a", "b", "c", "d", "e", "f", "g", "h")
    val ranges = MongoMovieRepo.idRanges(ids, parallelism = 4)

    ranges should have size 4
    // First range is open below, last is open above.
    ranges.head._1 shouldBe None
    ranges.last._2 shouldBe None

    // Candidates: every discovered id, ids that sort *between* discovered
    // ones (concurrent inserts), and ids below/above the whole range.
    val candidates =
      ids ++ Vector("a0", "b5", "d0", "e9", "g5") ++ Vector("0", "zzz")
    candidates.foreach { id =>
      withClue(s"id=$id covered by ${coveringCount(ranges, id)} range(s): ") {
        coveringCount(ranges, id) shouldBe 1
      }
    }
  }

  it should "place a concurrent insert that sorts into a chunk seam in exactly one range" in {
    // With parallelism 4 over 8 ids the chunk heads are a, c, e, g, so the
    // seam between chunk 2 ([c,d]) and chunk 3 ([e,f]) is the open interval
    // (d, e). "d5" lives in that seam — it was committed after discovery so
    // it isn't in `ids`. Under the old per-chunk min/max bounding it matched
    // neither chunk; under half-open ranges it lands in the (c, e) range.
    val ids    = Vector("a", "b", "c", "d", "e", "f", "g", "h")
    val ranges = MongoMovieRepo.idRanges(ids, parallelism = 4)

    coveringCount(ranges, "d5") shouldBe 1
    // And it's the range whose lower bound is the chunk-3 head's predecessor
    // seam, i.e. lower = "c".
    ranges.find(covers(_, "d5")).map(_._1) shouldBe Some(Some("c"))
  }

  it should "return a single unbounded range when there are fewer ids than workers" in {
    val ranges = MongoMovieRepo.idRanges(Vector("only"), parallelism = 4)
    ranges shouldBe Seq((None, None))
    // An unbounded range matches everything — the whole (tiny) collection.
    coveringCount(ranges, "only") shouldBe 1
    coveringCount(ranges, "anything-else") shouldBe 1
  }

  it should "return no ranges for an empty id list" in {
    MongoMovieRepo.idRanges(Vector.empty, parallelism = 4) shouldBe empty
  }
}
