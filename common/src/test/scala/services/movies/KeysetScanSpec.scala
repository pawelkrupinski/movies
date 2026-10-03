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
    (complete.isComplete, buf.result())
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

  it should "propagate an exception the CONSUMER throws, rather than passing it off as an incomplete read" in {
    // A bug in `onBatch` is not Mongo failing: reported as `false` it read as "a page failed
    // after retries", sent a pruning caller down its skip path with a misleading warning, and
    // hid the bug indefinitely. A read failure keeps its `false` (the spec above).
    var notified: Option[Throwable] = None
    val fetchesFrom = java.util.concurrent.ConcurrentHashMap[Option[String], Int]()
    val base = collectionOf("a", "b", "c", "d")
    val thrown = the[IllegalStateException] thrownBy KeysetScan.scan[String](
      label          = "test",
      batchSize      = 2,
      maxAttempts    = 3,
      initialBackoff = 1.milli,
      keyOf          = identity,
      fetchPage      = (afterId, limit) => { fetchesFrom.merge(afterId, 1, _ + _); base(afterId, limit) },
      onIncomplete   = e => notified = Some(e)
    )(batch => if (batch.contains("c")) throw new IllegalStateException("consumer bug"))
    thrown.getMessage shouldBe "consumer bug"
    notified shouldBe None      // not reported as an incomplete read
    // and not retried as one: no page was read twice
    fetchesFrom.values().stream().allMatch(_ == 1) shouldBe true
  }

  // Where the next page starts is part of READING the page: a row whose `_id` is not the
  // shape the caller's `keyOf` expects (a `getString("_id")` on an ObjectId) is a read that
  // cannot continue — incomplete, so a pruning caller skips — not a consumer bug.
  it should "report a page whose last key cannot be read as an incomplete scan" in {
    var notified: Option[Throwable] = None
    val complete = KeysetScan.scan[String](
      label          = "test",
      batchSize      = 2,
      maxAttempts    = 1,
      initialBackoff = 1.milli,
      keyOf          = id => if (id == "b") throw new ClassCastException("ObjectId is not a String") else id,
      fetchPage      = collectionOf("a", "b", "c", "d"),
      onIncomplete   = e => notified = Some(e)
    )(_ => ())
    complete.isComplete shouldBe false
    notified.map(_.getMessage) shouldBe Some("ObjectId is not a String")
  }

  it should "fetch the next page while the consumer is still working on this one" in {
    val base          = collectionOf("a", "b", "c", "d", "e")
    val secondStarted = java.util.concurrent.CountDownLatch(1)
    var overlapped    = false
    val complete = KeysetScan.scan[String](
      label          = "test",
      batchSize      = 2,
      maxAttempts    = 1,
      initialBackoff = 1.milli,
      keyOf          = identity,
      fetchPage      = (afterId, limit) => { if (afterId.contains("b")) secondStarted.countDown(); base(afterId, limit) }
    )(batch => if (batch.head == "a") overlapped = secondStarted.await(5, java.util.concurrent.TimeUnit.SECONDS))
    complete   shouldBe tools.ScanOutcome.Complete
    overlapped shouldBe true
  }

  private def byKeys(keys: Seq[String], inFlight: Int, fetchKeys: Seq[String] => Seq[String]): (Boolean, Vector[Seq[String]]) = {
    val pages = Vector.newBuilder[Seq[String]]
    val complete = KeysetScan.byKeys[String]("test", keys, batchSize = 2, inFlight = inFlight, maxAttempts = 1,
      initialBackoff = 1.milli, fetchKeys = fetchKeys)(pages += _)
    (complete.isComplete, pages.result())
  }

  "byKeys" should "read its pages side by side and hand them on in key order" in {
    // Each of the first two pages waits for the other's read to be under way: read one at a time,
    // the first never returns.
    val together = new java.util.concurrent.CyclicBarrier(2)
    val (complete, pages) = byKeys(Seq("a", "b", "c", "d", "e"), inFlight = 2, page => {
      if (page.head < "e") together.await(5, java.util.concurrent.TimeUnit.SECONDS)
      page.filterNot(_ == "c")   // a row gone by its page's read is simply absent
    })
    complete shouldBe true
    pages shouldBe Vector(Seq("a", "b"), Seq("d"), Seq("e"))
  }

  it should "stop at a page that still fails, the pages before it handed on" in {
    var failure = Option.empty[Throwable]
    val pages   = Vector.newBuilder[Seq[String]]
    val complete = KeysetScan.byKeys[String]("test", Seq("a", "b", "c", "d", "e"), batchSize = 2, inFlight = 1, maxAttempts = 1,
      initialBackoff = 1.milli, fetchKeys = page => if (page.contains("c")) throw new RuntimeException("down") else page,
      onIncomplete = e => failure = Some(e))(pages += _)
    complete shouldBe a[tools.ScanOutcome.Incomplete]
    pages.result() shouldBe Vector(Seq("a", "b"))
    failure.map(_.getMessage) shouldBe Some("down")
  }

  // A collecting reader once returned the rows it had read as the whole collection when a later
  // page failed (the cadence page's `all`): `collect` hands rows out only for a complete scan.
  "collect" should "answer every decoded row of a complete scan" in {
    KeysetScan.collect[String, String]("test", batchSize = 2, maxAttempts = 1, initialBackoff = 1.milli,
      keyOf = identity, fetchPage = collectionOf("a", "b", "c"))(row => Seq(row.toUpperCase))
      .shouldBe(tools.ReadOutcome.Answered(Vector("A", "B", "C")))
  }

  it should "fail — never answer the rows it got — when a page still fails" in {
    val base = collectionOf("a", "b", "c", "d", "e")
    val outcome = KeysetScan.collect[String, String]("test", batchSize = 2, maxAttempts = 1, initialBackoff = 1.milli,
      keyOf = identity, fetchPage = (after, limit) => if (after.contains("b")) throw new RuntimeException("down") else base(after, limit))(Seq(_))
    outcome match {
      case tools.ReadOutcome.Failed(cause) => cause.exception.getMessage shouldBe "down"
      case other                           => fail(s"a short scan answered $other")
    }
  }
}
