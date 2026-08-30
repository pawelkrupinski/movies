package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, Executors, TimeUnit}
import scala.jdk.CollectionConverters._

/**
 * `RoutingHttpFetch` is shared by every caller of the code under test, and a lot
 * of that code is parallel: each `*Ratings.refreshAll` fans its rows over a
 * `BoundedParallel` pool, and all of them hit the ONE stub.
 *
 * Its call log therefore has to be concurrent. It used to be a
 * `mutable.ListBuffer`, whose append is a three-step read-modify-write — under
 * concurrency that dropped entries and, worse, could throw an NPE out of `get`
 * itself. An exception from `get` is a FAILED READ to every enrichment client
 * (`EnrichmentRead` only turns 404/410 into `None`), so `FilmwebRatings.refreshAll`
 * swallowed it as a per-row failure and the row silently kept no `filmwebUrl` —
 * the `FilmwebRatingsSpec` "cheap rating-only path" flake, which only ever bit in
 * a loaded full-suite run.
 */
class RoutingHttpFetchSpec extends AnyFlatSpec with Matchers {

  private val Threads       = 8
  private val CallsPerThread = 500

  "RoutingHttpFetch" should "record every concurrent call, and never throw out of `get`" in {
    val fetch    = new RoutingHttpFetch(Map("/film/" -> "body"), unroutedIsNotFound = true)
    val failures = new ConcurrentLinkedQueue[Throwable]()
    val pool     = Executors.newFixedThreadPool(Threads)
    val start    = new CountDownLatch(1)
    val done     = new CountDownLatch(Threads)

    (1 to Threads).foreach { thread =>
      pool.execute { () =>
        try {
          start.await()
          (1 to CallsPerThread).foreach(i => fetch.get(s"https://example.test/film/$thread-$i") shouldBe "body")
        } catch { case failure: Throwable => failures.add(failure) }
        finally done.countDown()
      }
    }
    start.countDown()
    done.await(60, TimeUnit.SECONDS) shouldBe true
    pool.shutdown()

    failures.asScala.toSeq shouldBe empty
    fetch.calls should have size (Threads * CallsPerThread).toLong
    fetch.calls.map(_._1).distinct shouldBe Seq("GET")
  }

  it should "record concurrent posts with their bodies" in {
    val fetch = new RoutingHttpFetch(Map("/token" -> "ok"))
    val pool  = Executors.newFixedThreadPool(Threads)
    val done  = new CountDownLatch(Threads)

    (1 to Threads).foreach { thread =>
      pool.execute { () =>
        try (1 to CallsPerThread).foreach(i =>
          fetch.post("https://example.test/token", s"body-$thread-$i", "application/json"))
        finally done.countDown()
      }
    }
    done.await(60, TimeUnit.SECONDS) shouldBe true
    pool.shutdown()

    fetch.postBodies should have size (Threads * CallsPerThread).toLong
    fetch.calls     should have size (Threads * CallsPerThread).toLong
  }

  it should "keep call order for a single-threaded caller" in {
    val fetch = new RoutingHttpFetch(Map("/a" -> "A", "/b" -> "B"))
    fetch.get("https://example.test/a")
    fetch.post("https://example.test/b", "payload", "text/plain")
    fetch.calls shouldBe Seq("GET" -> "https://example.test/a", "POST" -> "https://example.test/b")
    fetch.postBodies shouldBe Seq(("https://example.test/b", "payload", "text/plain"))
  }
}
