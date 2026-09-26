package integration

import models.{CityScreening, ResolvedMovie, ResolvedRatings, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import services.readmodel.{MongoReadModelRepository, WebReadModel}
import services.users.InMemoryUserRepository
import tools.ConcurrentInstances
import tools.Eventually.eventually

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.duration._

/**
 * Every web pod keeps its own caches in front of Mongo, and every one of them answers "not
 * modified" from what it holds. A cache that never hears about ANOTHER pod's write tells that
 * pod's clients nothing changed when it did — the failure df4146340 removed two whole caches for.
 * The caches that remain are fed by change streams precisely so the other pod's write reaches
 * them; these specs hold each one to the bound it documents:
 *
 *  - `CaffeineUserChangeTimeCache` — the other pod's write within seconds while the stream is
 *    live, and never later than `entryTtl` when it stalls silently;
 *  - `WebReadModel.lastModifiedFor(city)` — the per-city validator behind the ETag and the 304,
 *    moved on every pod by a write the worker made.
 */
class PerPodCachesAcrossPodsIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val clock = Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), ZoneOffset.UTC)
  // Well inside anything a stream needs, far below the 10-minute TTL that bounds a stall.
  private val StreamBoundMs = 10000L

  private def hiddenAfter(pod: UserStatePod, userId: String, ifModifiedSince: String): (Int, String) = {
    val result = pod.hiddenFilms(userId, Some(ifModifiedSince))
    (status(result), if (status(result) == OK) contentAsString(result) else "")
  }

  /** Two pods with their change-time caches started, a user whose first write went through pod
   *  A, and the `Last-Modified` A answered it with — once A's own cache holds it, so the fast
   *  path is what answers from here on. */
  private def withWarmCache(suite: String, cacheTtl: FiniteDuration)(body: (UserStatePod, UserStatePod, String, String) => Unit): Unit =
    ConcurrentInstances.withInstances(mongoTarget, suite) { instances =>
      val users = new InMemoryUserRepository
      val Seq(a, b) = instances.map(instance => new UserStatePod(instance.database, users, clock, cacheTtl = cacheTtl))
      try {
        Seq(a, b).foreach(_.changeTimes.start())
        val userId = UserStatePod.signIn(users, "cached")
        val first  = a.hide(userId, "Seen on A")
        status(first) shouldBe OK
        val lastModified = header("Last-Modified", first).get
        eventually(a.changeTimes.lastChangeAt(userId) shouldBe defined, timeoutMs = StreamBoundMs)
        // The positive control: pod A really is answering from its cache, so what follows tests it.
        hiddenAfter(a, userId, lastModified)._1 shouldBe NOT_MODIFIED
        body(a, b, userId, lastModified)
      } finally Seq(a, b).foreach(_.close())
    }

  "a pod's change-time cache" should "stop answering 304 once ANOTHER pod has written, while its stream is live" in
    withWarmCache("change-time-cache-two-pods", 10.minutes) { (a, b, userId, lastModified) =>
      status(b.hide(userId, "Hidden on B")) shouldBe OK
      eventually({
        val (code, body) = hiddenAfter(a, userId, lastModified)
        code shouldBe OK
        body should include ("Hidden on B")
      }, timeoutMs = StreamBoundMs, pollMs = 50)
    }

  it should "stop answering 304 for another pod's write within its TTL even when its stream stalls silently" in
    withWarmCache("change-time-cache-stalled", 1.second) { (a, b, userId, lastModified) =>
      a.changeTimes.stop()   // the cursor stops delivering and nothing says so: no disconnect, no clear
      status(b.hide(userId, "Hidden on B")) shouldBe OK
      eventually({
        val (code, body) = hiddenAfter(a, userId, lastModified)
        code shouldBe OK
        body should include ("Hidden on B")
      }, timeoutMs = 1000 + 2000, pollMs = 50)
    }

  private val ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private val film    = ResolvedMovie("f-two-pods", "Diuna", None, None, Nil, None, Some(2021), Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def screening(hour: Int) =
    CityScreening(s"${film._id}|poznan|Kino Muza", film._id, "poznan", "Kino Muza", None,
      Seq(Showtime(LocalDateTime.of(2026, 6, 2, hour, 0), None)))

  "every web pod's per-city validator" should "move, and serve the change, when the worker writes that city" in
    ConcurrentInstances.withInstances(mongoTarget, "per-city-validator-two-pods", count = 3) { instances =>
      val Seq(podA, podB, worker) = instances
      val writer = new MongoReadModelRepository(Some(worker.database))
      writer.upsertMovie(film)
      writer.upsertScreening(screening(18))
      val pods = Seq(podA, podB).map(pod => new WebReadModel(new MongoReadModelRepository(Some(pod.database))))
      try {
        pods.foreach(_.start())
        pods.foreach(pod => pod.screeningsForCity("poznan").flatMap(_.showtimes).map(_.dateTime.getHour) shouldBe Seq(18))
        val before = pods.map(_.lastModifiedFor("poznan"))

        writer.upsertScreening(screening(21))

        pods.zip(before).zipWithIndex.foreach { case ((pod, validator), i) =>
          withClue(s"web pod ${i + 1}: ") {
            eventually({
              pod.screeningsForCity("poznan").flatMap(_.showtimes).map(_.dateTime.getHour) shouldBe Seq(21)
              pod.lastModifiedFor("poznan").isAfter(validator) shouldBe true
            }, timeoutMs = StreamBoundMs, pollMs = 50)
          }
        }
      } finally pods.foreach(_.stop())
    }
}
