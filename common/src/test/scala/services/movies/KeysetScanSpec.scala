package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

/** Pins the keyset-paging contract shared by `MongoMovieRepository.findAll` and
 *  `MongoScreeningsRepository.findAll`. Both used to pull a whole collection through ONE
 *  unbounded `find().toFuture()`, which recursed the async Mongo driver's read-completion
 *  chain into a `StackOverflowError` once the collection grew (Sentry KINOWO-19). The fix
 *  routes both through [[KeysetScan]], which reads bounded `_id`-keyset pages. The
 *  StackOverflow itself only reproduces against the real driver under a large buffered
 *  read, so this guards the MECHANISM the fix introduces: bounded page reads, correct
 *  keyset advancement across boundaries (every row exactly once, no skip/dup), the
 *  short-page terminator, per-page retry, and the incomplete-scan failure contract. */
class KeysetScanSpec extends AnyFlatSpec with Matchers {

  // A tiny in-memory "collection" of string rows; `fetchPage` mimics a server-side
  // `find(_id > afterId).sort(_id).limit(n)` over it.
  private def collectionOf(rows: String*): (Option[String], Int) => Seq[String] = {
    val sorted = rows.sorted.toVector
    (afterId, limit) => sorted.dropWhile(id => afterId.exists(id <= _)).take(limit)
  }

  private def scanAll(
    fetchPage:   (Option[String], Int) => Seq[String],
    batchSize:   Int = 2,
    maxAttempts: Int = 1,
    onIncomplete: Throwable => Unit = _ => ()
  ): (Boolean, Vector[String]) = {
    val buf = Vector.newBuilder[String]
    val complete = KeysetScan.scan[String](
      label          = "test",
      batchSize      = batchSize,
      maxAttempts    = maxAttempts,
      initialBackoff = 1.milli,
      keyOf          = identity,
      fetchPage      = fetchPage,
      onIncomplete   = onIncomplete
    )(buf ++= _)
    (complete, buf.result())
  }

  "KeysetScan.scan" should "return every row exactly once, in _id order, across page boundaries" in {
    // 5 rows, batchSize 2 → pages [a,b] [c,d] [e] — several boundaries.
    val (complete, rows) = scanAll(collectionOf("c", "a", "e", "b", "d"), batchSize = 2)
    complete shouldBe true
    rows shouldBe Vector("a", "b", "c", "d", "e") // no skip, no duplicate at a boundary
  }

  it should "only ever request a bounded page (never the whole collection at once)" in {
    var maxRequested = 0
    val base         = collectionOf((1 to 50).map(i => f"id$i%02d")*)
    val spy: (Option[String], Int) => Seq[String] = (afterId, limit) => {
      val page = base(afterId, limit)
      maxRequested = math.max(maxRequested, page.size)
      page
    }
    val (complete, rows) = scanAll(spy, batchSize = 10)
    complete       shouldBe true
    rows           should have size 50
    maxRequested shouldBe 10 // the async driver never buffers more than one bounded page
  }

  it should "terminate on a short final page even when total is an exact multiple of the batch size" in {
    // 4 rows, batchSize 2 → pages [a,b] [c,d] [] — the empty page ends the loop.
    val (complete, rows) = scanAll(collectionOf("a", "b", "c", "d"), batchSize = 2)
    complete shouldBe true
    rows shouldBe Vector("a", "b", "c", "d")
  }

  it should "handle an empty collection as a complete, empty scan" in {
    val (complete, rows) = scanAll(collectionOf())
    complete shouldBe true
    rows shouldBe empty
  }

  it should "report the scan incomplete (false) and notify onIncomplete when a page keeps failing" in {
    var notified: Option[Throwable] = None
    val boom: (Option[String], Int) => Seq[String] = (_, _) => throw new RuntimeException("mongo down")
    val (complete, rows) = scanAll(boom, maxAttempts = 2, onIncomplete = e => notified = Some(e))
    complete shouldBe false                     // a pruning caller must skip its destructive step
    rows shouldBe empty
    notified.map(_.getMessage) shouldBe Some("mongo down")
  }

  it should "retry a transiently-failing page and still complete" in {
    val base            = collectionOf("a", "b", "c")
    var firstCallFailed = false
    val flaky: (Option[String], Int) => Seq[String] = (afterId, limit) =>
      if (!firstCallFailed) { firstCallFailed = true; throw new RuntimeException("transient") }
      else base(afterId, limit)
    val (complete, rows) = scanAll(flaky, batchSize = 2, maxAttempts = 3)
    complete shouldBe true
    rows shouldBe Vector("a", "b", "c")
  }
}
