package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * The film share card (`GET /:city/movie/og-image`) renders on its OWN bounded pool, never on
 * Play's default dispatcher, and answers at once when that pool is full.
 *
 * A render blocks on a poster download for up to 20s. On 2026-09-21 AhrefsBot sent 612 of them in
 * 85 minutes, the city pages' p95 rose to 1.4-2.1s alongside the card's own, and web-pl was
 * OOM-killed seven times. The fetch below stands in for a poster origin: it records which thread
 * asked, and can be held open to fill the pool.
 */
class FilmOgImageControllerSpec extends AnyFlatSpec with Matchers {

  private class HeldFetch extends tools.PosterFetch {
    val threads  = new ConcurrentLinkedQueue[String]()
    val entered  = new CountDownLatch(1)
    val release  = new CountDownLatch(1)
    def bytes(url: String): Option[Array[Byte]] = {
      threads.add(Thread.currentThread.getName)
      entered.countDown()
      release.await(30, TimeUnit.SECONDS)
      None
    }
  }

  private def record(title: String) = MovieRecord(
    imdbId = None,
    data = Map[Source, SourceData](Helios -> SourceData(
      title     = Some(title),
      posterUrl = Some(s"https://cinema.example/${title.toLowerCase}.jpg"),
      showtimes = Seq(models.Showtime(LocalDateTime.now().plusHours(2), None, None, Nil))
    ))
  )

  private val films = Seq(("Diuna", Option.empty[Int], record("Diuna")), ("Belle", Option.empty[Int], record("Belle")))

  // Every request runs off the test thread: before the render moved to its own pool, `apply` did
  // the whole render inline and would block whoever called it.
  private val callers = ExecutionContext.fromExecutor(java.util.concurrent.Executors.newCachedThreadPool(r => {
    val t = new Thread(r); t.setDaemon(true); t
  }))

  private def request(ctrl: MovieController, title: String): Future[Result] =
    Future(ctrl.ogImage("poznan", title).apply(FakeRequest()))(using callers).flatten

  "GET /:city/movie/og-image" should "render on the share-card pool, not on the thread that received the request" in {
    val fetch = new HeldFetch
    fetch.release.countDown()
    val (ctrl, _) = TestMovieController.build(films, posters = fetch)

    status(request(ctrl, "Diuna"))(using 30.seconds) shouldBe OK
    fetch.threads.isEmpty shouldBe false
    fetch.threads.forEach(_ should startWith(tools.ShareCardPool.ThreadPrefix))
  }

  it should "answer 503 with Retry-After at once when the pool and its queue are full" in {
    val fetch = new HeldFetch
    val (ctrl, _) = TestMovieController.build(films, posters = fetch,
                                               shareCardPool = new tools.ShareCardPool(threads = 1, queueDepth = 0))
    try {
      val first = request(ctrl, "Diuna")
      fetch.entered.await(10, TimeUnit.SECONDS) shouldBe true

      val second = Await.result(request(ctrl, "Belle"), 3.seconds)
      second.header.status shouldBe SERVICE_UNAVAILABLE
      second.header.headers.get("Retry-After") shouldBe Some("30")
      second.header.headers.get("Cache-Control") shouldBe Some("no-store")

      fetch.release.countDown()
      status(first)(using 30.seconds) shouldBe OK
    } finally fetch.release.countDown()
  }
}
